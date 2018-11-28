package interconnect

import chisel3._
import chisel3.util._

import scala.collection.mutable

class CREECWidthConverter(p1: BusParams, p2: BusParams) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(p1))
    val master = new CREECBus(p2)
  })

  object Mode extends Enumeration {
    val Expand, Contract, Identity = Value
  }

  val mode = (p1.dataWidth compare p2.dataWidth) match {
    case -1 => Mode.Expand
    case 0  => Mode.Identity
    case 1  => Mode.Contract
  }

  val ratio = mode match {
    case Mode.Expand   => p2.dataWidth / p1.dataWidth
    case Mode.Contract => p1.dataWidth / p2.dataWidth
    case Mode.Identity => 1
  }

  val shiftAmt = mode match {
    case Mode.Expand   => p2.dataWidth - p1.dataWidth
    case Mode.Contract => p1.dataWidth - p2.dataWidth
    case Mode.Identity => 0
  }
    
  val sRecvHeader :: sRecvData :: sSend :: Nil = Enum(3)
  val state = RegInit(sRecvHeader)

  val dataInReg = RegInit(0.U(p1.dataWidth.W))
  val dataOutReg = RegInit(0.U(p2.dataWidth.W))

  val numBeats = RegInit(0.U(32.W))
  val beatCnt = RegInit(0.U(32.W))

  val ratioCnt = RegInit(0.U(32.W))

  val headerReg = Reg(chiselTypeOf(io.slave.header.bits))

  val idReg = RegInit(0.U)
  io.master.data.bits.id := idReg
  io.master.header.bits <> headerReg
  val newLen = mode match {
    case Mode.Expand   => (headerReg.len + 1.U) / ratio.asUInt() - 1.U
    case Mode.Contract => (headerReg.len + 1.U) * ratio.asUInt() - 1.U
    case Mode.Identity => headerReg.len
  }

  io.master.header.bits.len := newLen

  // TODO: this doesn't look like it can handle back-pressure from master
  io.master.header.valid := RegNext(state === sRecvHeader &&
                                    io.slave.header.fire())

  io.slave.header.ready := state === sRecvHeader

  io.slave.data.ready := state === sRecvData
  io.master.data.valid := state === sSend

  val outputData = mode match {
    case Mode.Expand   => dataOutReg
    case Mode.Contract => dataInReg(p2.dataWidth - 1, 0)
    case Mode.Identity => dataOutReg
  }

  io.master.data.bits.data := outputData

  switch (state) {
    is (sRecvHeader) {
      beatCnt := 0.U
      dataOutReg := 0.U

      when (io.slave.header.fire()) {
        state := sRecvData
        numBeats := io.slave.header.bits.len + 1.U
        headerReg := io.slave.header.bits
      }
    }

    is (sRecvData) {
      when (io.slave.data.fire()) {
        idReg := io.slave.data.bits.id
        beatCnt := beatCnt + 1.U

        mode match {
          case Mode.Expand =>
            beatCnt := beatCnt + 1.U
            dataOutReg := (dataOutReg >> p1.dataWidth) |
                          (io.slave.data.bits.data << shiftAmt)

            when (ratioCnt === (ratio - 1).asUInt()) {
              state := sSend
            }
            .otherwise {
              ratioCnt := ratioCnt + 1.U
            }

          case Mode.Contract =>
            dataInReg := io.slave.data.bits.data
            state := sSend

          case Mode.Identity =>
            dataOutReg := io.slave.data.bits.data
            state := sSend
        }

      }
    }

    is (sSend) {
      when (io.master.data.fire()) {
        mode match {
          case Mode.Expand =>
            ratioCnt := 0.U
            when (beatCnt === numBeats) {
              state := sRecvHeader
            }
            .otherwise {
              state := sRecvData
            }
          case Mode.Contract =>
            dataInReg := dataInReg >> p2.dataWidth
            when (ratioCnt === (ratio - 1).asUInt()) {
              ratioCnt := 0.U
              when (beatCnt === numBeats) {
                state := sRecvHeader
              }
              .otherwise {
                state := sRecvData
              }
            } .otherwise {
              ratioCnt := ratioCnt + 1.U
            }
          case Mode.Identity =>
            when (beatCnt === numBeats) {
              state := sRecvHeader
            }
            .otherwise {
              state := sRecvData
            }

        }
      }
    }

  }
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
