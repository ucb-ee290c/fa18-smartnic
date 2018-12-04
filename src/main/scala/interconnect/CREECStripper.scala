package interconnect

import chisel3._
import chisel3.util._

// Strip the incoming bus transaction so that it has the original # data beats
// We need to look at the metadata to determine how much bytes to strip
// For example, if encryption is False, and encryptionPadBytes is not zero,
// we proceeds to strip *encryptionPadBytes* from the bus transaction
class CREECStripper(busParams: BusParams) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(busParams))
    val master = new CREECBus(busParams)
  })

  val sRecvHeader :: sSendHeader :: sRecvData :: sSendData :: Nil = Enum(4)
  val state = RegInit(sRecvHeader)

  val dataOutReg = RegInit(0.U(busParams.dataWidth.W))
  val headerReg = Reg(chiselTypeOf(io.slave.header.bits))
  val idReg = RegInit(0.U)
  io.master.data.bits.id := idReg
  io.master.header.bits <> headerReg

  // This looks like we have to follow the same order as we compose the blocks
  val newLen = Wire(chiselTypeOf(headerReg.len))
  when (!headerReg.ecc && headerReg.eccPadBytes =/= 0.U) {
    val sLen = headerReg.eccPadBytes.asTypeOf(chiselTypeOf(headerReg.len)) /
               busParams.bytesPerBeat.U
    newLen := headerReg.len - sLen
    io.master.header.bits.eccPadBytes := 0.U
  }
  .elsewhen (!headerReg.encrypted && headerReg.encryptionPadBytes =/= 0.U) {
    val sLen =
      headerReg.encryptionPadBytes.asTypeOf(chiselTypeOf(headerReg.len)) /
      busParams.bytesPerBeat.U
    newLen := headerReg.len - sLen
    io.master.header.bits.encryptionPadBytes := 0.U
  }
  .elsewhen (!headerReg.compressed && headerReg.compressionPadBytes =/= 0.U) {
    val sLen =
      headerReg.compressionPadBytes.asTypeOf(chiselTypeOf(headerReg.len)) /
      busParams.bytesPerBeat.U
    newLen := headerReg.len - sLen
    io.master.header.bits.compressionPadBytes := 0.U
  }
  .otherwise {
    newLen := headerReg.len
  }

  val numBeatsExpected = newLen + 1.U
  val numBeatsOriginal = headerReg.len + 1.U
  val beatCnt = RegInit(0.U(32.W))

  io.master.header.bits.len := newLen

  io.slave.header.ready := state === sRecvHeader
  io.master.header.valid := state === sSendHeader
  io.slave.data.ready := state === sRecvData
  io.master.data.valid := state === sSendData && (beatCnt <= numBeatsExpected)

  io.master.data.bits.data := dataOutReg

  switch (state) {
    is (sRecvHeader) {
      beatCnt := 0.U
      dataOutReg := 0.U

      when (io.slave.header.fire()) {
        state := sSendHeader
        headerReg := io.slave.header.bits
      }
    }

    is (sSendHeader) {
      when (io.master.header.fire()) {
        state := sRecvData
      }
    }

    is (sRecvData) {
      when (io.slave.data.fire()) {
        idReg := io.slave.data.bits.id
        dataOutReg := io.slave.data.bits.data
        beatCnt := beatCnt + 1.U
        state := sSendData
      }
    }

    is (sSendData) {
      when (io.master.data.fire() || beatCnt >= numBeatsExpected) {
        // Receive the next header if all beats have sent out
        when (beatCnt === numBeatsOriginal) {
          state := sRecvHeader
        }
        .otherwise {
          state := sRecvData
        }
      }
    }

  }
}

// This is a hack to strip padding which may have been added prior to the AES block
class CREECStripperModel extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    val strippedData = in.data.dropRight(in.encryptionPadBytes)
    Seq(in.copy(data = strippedData, encryptionPadBytes = 0))
  }
}
