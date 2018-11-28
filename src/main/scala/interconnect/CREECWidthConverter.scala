package interconnect

import chisel3._

import scala.collection.mutable

class CREECWidthConverter(p1: BusParams, p2: BusParams, allowPadding: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(p1))
    val out = new CREECBus(p2)
  })
  io.out.header <> Reg(io.in.header)
  io.out.data <> Reg(io.in.data)
}

class CREECWidthConverterModel(p1: BusParams, p2: BusParams, allowPadding: Boolean = false)
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

  // Map from transaction ID to data belonging to transaction
  val dataRepack = mutable.Map[Int, Seq[Byte]]()
  // Map of transactions in flight from transaction ID to the header beat
  val inFlight = mutable.Map[Int, CREECHeaderBeat]()

  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat =>
        behavior match {
          case Behavior.IDENTITY => Seq(t)
          case Behavior.EXPAND =>
            assert((t.len + 1) % ratio == 0 || allowPadding, s"Expanding width converter received a header beat with length that's not divisible by the ratio $ratio")
            inFlight.update(t.id, t)
            Seq(t.copy(len = ((t.len + 1) / ratio) - 1)(p2))
        }
      case t: CREECDataBeat =>
        behavior match {
          case Behavior.IDENTITY => Seq(t)
          case Behavior.EXPAND =>
            val recvData = dataRepack.getOrElse(t.id, Seq[Byte]())
            val newData = recvData ++ t.data
            val tx = inFlight.get(t.id)
            // TODO: not considering the case where allowPadding is used (need to also update and query inFlight)
            if (newData.length == p2.bytesPerBeat) { // Normal beat
              dataRepack.update(t.id, Seq[Byte]())
              inFlight.update(t.id, tx.get.copy(len = tx.get.len - ratio)(p1))
              Seq(CREECDataBeat(newData, t.id)(p2))
            } else {
              dataRepack.update(t.id, newData)
              Seq()
            }
        }
    }
  }
}
