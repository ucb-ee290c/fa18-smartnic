package interconnect

import chisel3._

class CREECWriteWidthConverter(p: BusParams) extends Module {
  val io = IO(new Bundle {
    val slave = new CREECWriteBus(p)
    val master = new CREECWriteBus(p)
  })
  io.master.header <> Reg(io.slave.header)
  io.master.data <> Reg(io.slave.data)
  // Let's add a little logic here
  io.master.data.bits.data := Reg(io.slave.data.bits.data + 1.U)
}

/**
  * Expand or contract the width
  * 1:2 width converter => (ratio = 2, expand = True)
  * 2:1 width converter => (ratio = 2, expand = False)
  * 4:1 width converter => (ratio = 4, expand = False)
  * @param ratio Width ratio of inWidth:outWidth. Must be power of 2
  * @param expand Set to true if outWidth > inWidth.
  */
class CREECWidthConverterModel(ratio: Int, expand: Boolean) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat =>
        if (expand) {
          assert(t.len % ratio == 0)
          Seq(CREECHeaderBeat(t.len / ratio, t.id, t.addr))
        } else {
          Seq(CREECHeaderBeat(t.len * ratio, t.id, t.addr))
        }
      case t: CREECDataBeat =>
        if (expand) {
          Seq()
        } else {
          Seq()
        }
    }
  }
}
