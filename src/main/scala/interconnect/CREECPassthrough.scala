package interconnect

import chisel3._

/**
  * A RTL CREEC passthrough 'register slice'. Adds one to any data beat as it passes through.
  */
class CREECPassthrough(p: BusParams) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(p))
    val master = new CREECBus(p)
  })
  // TODO: this module is actually buggy since it doesn't handle backpressure from the master port and can drop 1 beat
  io.slave.header.ready := RegNext(io.master.header.ready)
  io.slave.data.ready := RegNext(io.master.data.ready)
  io.master.header <> RegNext(io.slave.header)
  io.master.data <> RegNext(io.slave.data)
  // Let's add a little logic here
  io.master.data.bits.data := RegNext(io.slave.data.bits.data + 1.U)
  io.master.header.valid := RegNext(io.slave.header.valid)
  io.master.data.valid := RegNext(io.slave.data.valid)
}

class CREECPassthroughModel(p: BusParams) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat => Seq(t)
      case t: CREECDataBeat =>
        // Prepend a 0 byte to make sure t.data is decoded as unsigned
        // Note: BigInt(Array(0, 0, 1) = 1
        // Note: BigInt(Array(0, 1, 1) = 257
        // Note: BigInt(Array(255, 255, 255) = -1 (not what we wanted)
        // Note: BigInt(Array(0, 255, 255, 255) = 16777215 (this is better)
        val dataAsInt = BigInt((0.asInstanceOf[Byte] +: t.data).toArray)
        val modifiedData = (dataAsInt + 1).toByteArray
        // We may have overflow (in which case there is 1 extra byte), or fewer than 8 bytes if the original data was small
        val refinedData = if (modifiedData.length > p.dataWidth/8) {
          modifiedData.reverse.slice(0, p.bytesPerBeat).reverse // truncate overflow
        } else if (modifiedData.length < p.bytesPerBeat) {
          modifiedData.reverse.padTo(p.bytesPerBeat, 0.asInstanceOf[Byte]).reverse // pad with zero bytes
        } else {
          modifiedData
        }
        Seq(CREECDataBeat(refinedData, 0)(p))
    }
  }
}
