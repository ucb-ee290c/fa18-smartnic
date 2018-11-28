// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._
import interconnect._

// RSEncoders accepts k symbols
// It produces n symbols (k original symbols appended by (n - k) parity symbols)
// Each symbol has a width of *symbolWidth*
class RSEncoder(val rsParams: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(rsParams.symbolWidth.W)))
    val out = new DecoupledIO(UInt(rsParams.symbolWidth.W))
  })

  val inReadyReg = RegInit(true.B)
  val outValidReg = RegInit(false.B)

  io.in.ready := inReadyReg
  io.out.valid := outValidReg

  // Counter for input message
  val (inCntVal, inCntDone) = Counter(io.in.fire(), rsParams.k)
  // Counter for output message
  val (outCntVal, outCntDone) = Counter(io.out.fire(), rsParams.n)

  when (inCntDone) {
    // Stop accepting new input when there is enough data
    inReadyReg := false.B
  }
  .elsewhen (io.in.fire()) {
    // Start producing output when the first incoming input is fired
    outValidReg := true.B
  }

  when (outCntDone) {
    // When the last output is fired, ready to accept new input
    outValidReg := false.B
    inReadyReg := true.B
  }

  val Regs = RegInit(VecInit(Seq.fill(rsParams.n - rsParams.k)(
    0.U(rsParams.symbolWidth.W))))
  val inputBitsReg = RegInit(0.U(rsParams.symbolWidth.W))

  when (io.in.fire()) {
    inputBitsReg := io.in.bits
  }
  .otherwise {
    // reset the input register to cater the next input data
    inputBitsReg := 0.U
  }

  // Make sure the arithmetic operations are correct (in Galois field)
  val feedback = Mux(outCntVal < rsParams.k.asUInt(),
                     inputBitsReg ^ Regs(rsParams.n - rsParams.k - 1), 0.U)

  // The encoding computation is done in a LFSR fashion
  Regs.zipWithIndex.foldLeft(0.U) {
    case (prevReg, (nextReg, idx)) => {
      nextReg := prevReg ^ rsParams.GFOp.mul(feedback,
                             rsParams.gCoeffs(idx).asUInt())
      nextReg
    }
  }

  // The first k output messages are the original input messages
  // The subsequent messages are the generated parity messages
  io.out.bits := Mux(outCntVal < rsParams.k.asUInt(),
                     inputBitsReg, Regs(rsParams.n - rsParams.k - 1))
}

// CREECBus integration with the RSEncoder module
// What to do:
// *** Encoder
//   + Receive write header from upstream block (slave)
//   + Send write header to downstream block (master)
//   + Receive write data from upstream block (slave)
//   + Send *encoded* write data to downstream block (master)
class ECCEncoderTop(val rsParams: RSParams = new RSParams(),
                    val busParams: CREECBusParams = new CREECBusParams()
  ) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(busParams))
    val master = new CREECBus(busParams)
  })

  // *numItems* denotes the number of master outputs to be fired
  // One master output represents *dataWidth* / *symbolWidth* symbols
  // If the encoder produces more symbols than one master output can represent,
  // we need to fire multiple master outputs
  // For example, if we have RS(16, 8, 8), meaning 16 symbols with 8 original
  // symbols of 8-bit, we need to fire *two* 64-bit output at the master port
  // after encoding
  val numItems = rsParams.n * rsParams.symbolWidth / busParams.dataWidth

  // There are four states
  //  - sRecvHeader: for accepting the header from the slave port
  //  - sRecvData: for accepting the data from the slave port
  //  - sCompute: RS encoding
  //  - sDone: send the encoded data to the master port
  val sRecvHeader :: sRecvData :: sCompute :: sDone :: Nil = Enum(4)
  val state = RegInit(sRecvHeader)
  val header = Reg(chiselTypeOf(io.slave.header.bits))

  // TODO: Don't know how to handle metadata for now
  io.master.header.bits.compressed := header.compressed
  io.master.header.bits.ecc := true.B
  io.master.header.bits.encrypted := header.encrypted
  io.master.header.bits.compressionPadBytes := header.compressionPadBytes
  io.master.header.bits.eccPadBytes := 0.U
  io.master.header.bits.encryptionPadBytes := header.encryptionPadBytes

  // For the header, simply forward it to the master port.
  // However, we need to modify the beat length based on RSParams
  // because of the newly generated parity symbols.
  // TODO: Dealing with odd number of beats. Hopefully never.
  // Quirk: plus 1 (minus 1) because of the CREECBus convention
  // we are following (e.g., *len* - 1 means a beat length of *len*)
  io.master.header.bits.len := RegNext((io.slave.header.bits.len + 1.U) *
                                       numItems.asUInt() - 1.U)
  io.master.header.bits.addr := RegNext(io.slave.header.bits.addr)
  io.master.header.bits.id := RegNext(io.slave.header.bits.id)
  io.master.header.valid := RegNext(io.slave.header.valid)

  // TODO: Don't know how to handle id for now
  io.master.data.bits.id := RegNext(io.slave.data.bits.id)

  val enc = Module(new RSEncoder(rsParams))

  val dataInReg = RegInit(0.U(busParams.dataWidth.W))
  val dataOutReg = RegInit(0.U((rsParams.n * rsParams.symbolWidth).W))

  io.slave.header.ready := state === sRecvHeader

  io.slave.data.ready := state === sRecvData
  io.master.data.valid := state === sDone

  // Note that the output data only has a width of *dataWidth*
  io.master.data.bits.data := dataOutReg(busParams.dataWidth - 1, 0)

  // Trigger the encoding process when sCompute
  enc.io.in.valid := state === sCompute
  enc.io.in.bits := dataInReg
  enc.io.out.ready := state === sCompute

  val numBeats = RegInit(0.U(32.W))

  // Various counters for keeping track of progress
  val (encInCntVal, encInCntDone) = Counter(enc.io.in.fire(), rsParams.k)
  val (encOutCntVal, encOutCntDone) = Counter(enc.io.out.fire(), rsParams.n)
  val (itemCntVal, itemCntDone) = Counter(io.master.data.fire(), numItems)
  // Cannot use Counter for this because the maximum value is not statically known
  val beatCnt = RegInit(0.U(32.W))

  switch (state) {
    is (sRecvHeader) {
      // Properly reset registers to prepare for the next input
      beatCnt := 0.U
      dataInReg := 0.U
      dataOutReg := 0.U

      when (io.slave.header.fire()) {
        state := sRecvData
        numBeats := (io.slave.header.bits.len + 1.U) * numItems.asUInt()
        header := io.slave.header.bits
      }
    }

    is (sRecvData) {
      when (io.slave.data.fire()) {
        // start the encoding process once the first input item is fired
        state := sCompute
        dataInReg := io.slave.data.bits.data
        beatCnt := beatCnt + 1.U
      }
    }

    is (sCompute) {
      // Slice the dataInReg register into smaller chunk of size
      // *symbolWidth* for the encoding process
      when (enc.io.in.fire()) {
        dataInReg := (dataInReg >> rsParams.symbolWidth)
      }

      when (enc.io.out.fire()) {
        when (encOutCntDone) {
          state := sDone
        }

        // Note the endianness
        // The first encoding output is the LSB
        // The last encoding output is the MSB
        val shiftAmt = rsParams.n * rsParams.symbolWidth - rsParams.symbolWidth
        dataOutReg := (dataOutReg >> rsParams.symbolWidth) |
          (enc.io.out.bits << shiftAmt)
      }
    }

    is (sDone) {
      when (io.master.data.fire()) {
        dataOutReg := (dataOutReg >> busParams.dataWidth)
        // May take multiple cycles to send the output data
        // if it is larger than the bus data width
        when (itemCntDone) {
          when (beatCnt === numBeats - 1.U) {
            // If all data beats have been processed, receive the next header
            state := sRecvHeader
          }
          .otherwise {
            // Process the next data beat
            state := sRecvData
          }
        }
      }
    }

  }
}

