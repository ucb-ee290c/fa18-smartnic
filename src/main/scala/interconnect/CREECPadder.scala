package interconnect

import chisel3._
import chisel3.util._

/**
  * Pads incoming high-level transactions a multiple of padBytes bytes with zero bytes
  * Modifies encryptionPadBytes field as a hack to handle block size alignment
  * @param padBytes Pad output data to a multiple of padBytes
  */
class CREECPadder(busParams: BusParams, padBytes: Int = 8) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(busParams))
    val out = new CREECBus(busParams)
  })

  val sIdle :: sSendHeader :: sSendData :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val workingHeader = Reg(chiselTypeOf(io.in.header.bits))
  val extraBeats = Reg(chiselTypeOf(io.in.header.bits.len))
  val paddingData: Bool = workingHeader.len < extraBeats
  val zeroData = Wire(new TransactionData(busParams))
  zeroData.id := workingHeader.id
  zeroData.data := 0.U

  io.in.header.ready := state === sIdle
  io.out.header.bits := workingHeader
  // TODO: generalize what field in the header is touched when padding takes place
  io.out.header.bits.len := workingHeader.len + extraBeats
  io.out.header.bits.encryptionPadBytes := extraBeats * busParams.bytesPerBeat.U
  io.out.header.valid := state === sSendHeader

  // Use a shallow queue to get around testers2 threading ordering limitation on combinational input -> output paths
  val dataQueue = Module(new Queue(new TransactionData(), 1))

  // Push data into queue from the input directly or from the padder
  dataQueue.io.enq.valid := (state === sSendData) && (io.in.data.valid || paddingData)
  io.in.data.ready := (state === sSendData) && dataQueue.io.enq.ready
  dataQueue.io.enq.bits := Mux(state === sSendData && io.in.data.valid && !paddingData, io.in.data.bits, zeroData)

  // Pull data from queue as usual
  io.out.data <> dataQueue.io.deq

  switch (state) {
    is (sIdle) {
      when (io.in.header.fire()) {
        workingHeader := io.in.header.bits
        extraBeats := (io.in.header.bits.len + 1.U) % (padBytes / busParams.bytesPerBeat).U
        state := sSendHeader
      }
    }
    is (sSendHeader) {
      when (io.out.header.fire()) {
        state := sSendData
        workingHeader.len := workingHeader.len + extraBeats
      }
    }
    is (sSendData) {
      when (io.out.data.fire()) {
        workingHeader.len := workingHeader.len - 1.U
        when (workingHeader.len === 0.U) {
          state := sIdle
        }
      }
    }
  }
}

class CREECPadderModel(padBytes: Int = 8) extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    val paddedData = in.data.padTo(math.ceil(in.data.length / padBytes.toFloat).toInt * padBytes, 0.asInstanceOf[Byte])
    Seq(in.copy(data = paddedData.toList, encryptionPadBytes = paddedData.length - in.data.length))
  }
}
