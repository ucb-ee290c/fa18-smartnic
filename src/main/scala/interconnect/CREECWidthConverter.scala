package interconnect

import chisel3._

class CREECWidthConverter(p1: BusParams, p2: BusParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(p1))
    val out = new CREECBus(p2)
  })
  io.out.header <> Reg(io.in.header)
  io.out.data <> Reg(io.in.data)
}

/**
  * Expand or contract the width
  * 1:2 width converter => (ratio = 2, expand = True)
  * 2:1 width converter => (ratio = 2, expand = False)
  * 4:1 width converter => (ratio = 4, expand = False)
  * @param ratio Width ratio of inWidth:outWidth. Must be power of 2
  * @param expand Set to true if outWidth > inWidth.
  */
//class CREECWidthConverterModel(p1: BusParams, p2: BusParams, allowPadding: Boolean) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  /*
  val ratio = p1.dataWidth / p2.dataWidth
  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat =>
        if (expand) {
          assert(t.len % ratio == 0)
          Seq(CREECHeaderBeat(t.len / ratio, t.id, t.addr)(p))
        } else {
          Seq(CREECHeaderBeat(t.len * ratio, t.id, t.addr)(p))
        }
      case t: CREECDataBeat =>
        if (expand) {
          Seq()
        } else {
          Seq()
        }
    }
  }
  */
//}
