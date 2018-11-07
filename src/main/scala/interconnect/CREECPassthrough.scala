package interconnect

import chisel3._

class CREECPassthrough(p: BusParams) extends Module {
  val io = IO(new Bundle {
    val slave = new CREECWriteBus(p)
    val master = new CREECWriteBus(p)
  })
  io.master.header <> Reg(io.slave.header)
  io.master.data <> Reg(io.slave.data)
  // Let's add a little logic here
  io.master.data.bits.data := Reg(io.slave.data.bits.data + 1.U)
}

class CREECPassthroughModel extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  def process(in: Option[CREECLowLevelTransaction]): Option[Seq[CREECLowLevelTransaction]] = {
    in match {
      case Some(t) => t match {
        case t: CREECHeaderBeat => Some(Seq(t))
        case t: CREECDataBeat => Some(Seq(CREECDataBeat(t.data + 1, t.id)))
      }
      case None => None
    }
  }
}
