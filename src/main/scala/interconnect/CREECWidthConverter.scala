package interconnect

import chisel3._

import scala.collection.mutable

class CREECWidthConverter(p1: BusParams, p2: BusParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(p1))
    val out = new CREECBus(p2)
  })
  io.out.header <> Reg(io.in.header)
  io.out.data <> Reg(io.in.data)
}

class CREECWidthConverterModel(p1: BusParams, p2: BusParams)
  extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  object Behavior extends Enumeration {
    val EXPAND, CONTRACT, IDENTITY = Value
  }

  val (ratio, behavior) = if (p1.dataWidth > p2.dataWidth) {
    assert(p1.dataWidth % p2.dataWidth == 0, s"Contracting width converter must have rational dataWidth ratios: p1: ${p1.dataWidth},p2: ${p2.dataWidth}")
    (p1.dataWidth / p2.dataWidth, Behavior.CONTRACT)
  } else if (p1.dataWidth < p2.dataWidth) {
    assert(p2.dataWidth % p1.dataWidth == 0, s"Expanding width converter must have rational dataWidth ratios: p1: ${p1.dataWidth},p2: ${p2.dataWidth}")
    (p2.dataWidth / p1.dataWidth, Behavior.EXPAND)
  } else {
    (1, Behavior.IDENTITY)
  }

  // Map from transaction ID to data belonging to transaction (for expanding)
  val dataRepack = mutable.Map[Int, Seq[Byte]]()

  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat =>
        behavior match {
          case Behavior.IDENTITY => Seq(t)
          case Behavior.EXPAND =>
            assert((t.len + 1) % ratio == 0, s"Expanding width converter received a header beat with length that's not divisible by the ratio $ratio")
            Seq(t.copy(len = ((t.len + 1) / ratio) - 1)(p2))
          case Behavior.CONTRACT =>
            Seq(t.copy(len = ((t.len + 1) * ratio) - 1)(p2))
        }
      case t: CREECDataBeat =>
        behavior match {
          case Behavior.IDENTITY => Seq(t)
          case Behavior.EXPAND =>
            val recvData = dataRepack.getOrElse(t.id, Seq[Byte]())
            val newData = recvData ++ t.data
            if (newData.length == p2.bytesPerBeat) {
              dataRepack.update(t.id, Seq[Byte]())
              Seq(CREECDataBeat(newData, t.id)(p2))
            } else {
              dataRepack.update(t.id, newData)
              Seq()
            }
          case Behavior.CONTRACT =>
            val dataChunks = t.data.grouped(p2.bytesPerBeat).toList
            dataChunks.map {
              dataChunk => t.copy(data = dataChunk)(p2)
            }
        }
    }
  }
}
