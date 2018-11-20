// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._
import interconnect._

// This module will accept k symbols (io.in.fire() === true.B until received k symbols)
// It will emit n symbols (io.out.fire() === true.B until sent n symbols)
// Each symbol has a width of *symbolWidth*
class RSEncoder(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  val inReadyReg = RegInit(true.B)
  val outValidReg = RegInit(false.B)

  io.in.ready := inReadyReg
  io.out.valid := outValidReg

  val (inCntVal, inCntDone) = Counter(io.in.fire(), p.k)
  val (outCntVal, outCntDone) = Counter(io.out.fire(), p.n)

  when (inCntDone) {
    inReadyReg := false.B
  }
  .elsewhen (io.in.fire()) {
    outValidReg := true.B
  }

  when (outCntDone) {
    outValidReg := false.B
    inReadyReg := true.B
  }

  val Regs = RegInit(VecInit(Seq.fill(p.n - p.k)(0.U(p.symbolWidth.W))))
  val inputBitsReg = RegInit(0.U(p.symbolWidth.W))
  when (io.in.fire()) {
    inputBitsReg := io.in.bits
  }
  .otherwise {
    inputBitsReg := 0.U
  }

  // Make sure the arithmetic operations are correct (in Galois field)
  val feedback = Mux(outCntVal < p.k.asUInt(),
                     inputBitsReg ^ Regs(p.n - p.k - 1), 0.U)

  Regs.zipWithIndex.foldLeft(0.U) {
    case (prevReg, (nextReg, idx)) => {
      nextReg := prevReg ^ p.GFOp.mul(feedback, p.gCoeffs(idx).asUInt())
      nextReg
    }
  }

  io.out.bits := Mux(outCntVal < p.k.asUInt(),
                     inputBitsReg, Regs(p.n - p.k - 1))
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

  val numItems = rsParams.n * rsParams.symbolWidth / busParams.dataWidth
  val sRecvHeader :: sRecvData :: sCompute :: sDone :: Nil = Enum(4)
  val state = RegInit(sRecvHeader)

  // TODO: handle the metadata appropriately
  io.master.header.bits.compressed := false.B
  io.master.header.bits.ecc := true.B
  io.master.header.bits.encrypted := false.B

  io.master.header.bits.addr := RegNext(io.slave.header.bits.addr)
  // Modify the beat number based on Reed-Solomon code configuration
  // TODO: Dealing with odd number of beats. Hopefully never
  io.master.header.bits.len := RegNext((io.slave.header.bits.len + 1.U) *
                                       numItems.asUInt() - 1.U)
  io.master.header.bits.id := RegNext(io.slave.header.bits.id)
  io.master.header.valid := RegNext(io.slave.header.valid)

  io.master.data.bits.id := RegNext(io.slave.data.bits.id)

  val enc = Module(new RSEncoder(rsParams))

  val dataInReg = RegInit(0.U(busParams.dataWidth.W))
  val dataOutReg = RegInit(0.U((rsParams.n * rsParams.symbolWidth).W))

  io.slave.header.ready := state === sRecvHeader

  io.slave.data.ready := state === sRecvData
  io.master.data.valid := state === sDone
  io.master.data.bits.data := dataOutReg(busParams.dataWidth - 1, 0)

  enc.io.in.valid := state === sCompute
  enc.io.in.bits := dataInReg
  enc.io.out.ready := state === sCompute

  val numBeats = RegInit(0.U(32.W))

  val (encInCntVal, encInCntDone) = Counter(enc.io.in.fire(), rsParams.k)
  val (encOutCntVal, encOutCntDone) = Counter(enc.io.out.fire(), rsParams.n)
  val (itemCntVal, itemCntDone) = Counter(io.master.data.fire(), numItems)
  // Cannot use Counter for this because the maximum value is not statically known
  val beatCnt = RegInit(0.U(32.W))

  switch (state) {
    is (sRecvHeader) {
      beatCnt := 0.U
      dataInReg := 0.U
      dataOutReg := 0.U

      when (io.slave.header.fire()) {
        state := sRecvData
        numBeats := io.slave.header.bits.len
      }
    }

    is (sRecvData) {
      when (io.slave.data.fire()) {
        state := sCompute
        dataInReg := io.slave.data.bits.data
        beatCnt := beatCnt + 1.U
      }
    }

    is (sCompute) {
      when (enc.io.in.fire()) {
        dataInReg := (dataInReg >> rsParams.symbolWidth)
      }

      when (enc.io.out.fire()) {
        when (encOutCntDone) {
          state := sDone
        }
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

