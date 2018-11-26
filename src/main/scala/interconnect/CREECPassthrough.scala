package interconnect

import chisel3._
import interconnect.CREECAgent._

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
        val newData = bytesToBigInt(t.data) + 1
        // Take care of potential overflow TODO: maybe add intentional truncation support to bigIntToBytes
        val newDataBytes = bigIntToBytes(newData, p.bytesPerBeat + 1)
        Seq(t.copy(data = newDataBytes.slice(0, p.bytesPerBeat))(p))
    }
  }
}
