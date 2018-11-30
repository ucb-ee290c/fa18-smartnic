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
    case -1 => // p1.dataWidth < p2.dataWidth
      require(p2.dataWidth % p1.dataWidth == 0, s"Expanding width converter must have rational dataWidth ratios: p1: ${p1.dataWidth},p2: ${p2.dataWidth}")
      Mode.Expand
    case 1  => // p1.dataWidth > p2.dataWidth
      require(p1.dataWidth % p2.dataWidth == 0, s"Contracting width converter must have rational dataWidth ratios: p1: ${p1.dataWidth},p2: ${p2.dataWidth}")
      Mode.Contract
    case 0  => // p1.dataWidth == p2.dataWidth
      Mode.Identity
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

  // There are four stages
  //   - sRecvHeader: for accepting the header from the slave port
  //   - sSendHeader: for sending the header to the master port
  //   - sRecvData: for accepting the data from the slave port
  //   - sSendData: for sending the data to the master port
  val sRecvHeader :: sSendHeader :: sRecvData :: sSendData :: Nil = Enum(4)
  val state = RegInit(sRecvHeader)

  val dataInReg = RegInit(0.U(p1.dataWidth.W))
  val dataOutReg = RegInit(0.U(p2.dataWidth.W))

  // In the case of Expand mode, this counter keeps track of how many input
  // data needs to be accepted before sending a new output data.
  // In the case of Contract mode, this counter keeps track of how many output
  // data needs to be sent before accepting a new input data.
  // The Identity mode does not use this counter
  val ratioCnt = RegInit(0.U(32.W))

  val headerReg = Reg(chiselTypeOf(io.slave.header.bits))

  val idReg = RegInit(0.U)

  io.master.data.bits.id := idReg
  io.master.header.bits <> headerReg

  // Update the len field accordingly to the mode
  val newLen = mode match {
    case Mode.Expand   => (headerReg.len + 1.U + ratio.U - 1.U) / ratio.U - 1.U
    case Mode.Contract => (headerReg.len + 1.U) * ratio.asUInt() - 1.U
    case Mode.Identity => headerReg.len
  }

  // Keep track of how many beats are sent
  val numBeatsOriginal = headerReg.len + 1.U
  val numBeatsExpected = (newLen + 1.U) * ratio.U
  val beatCnt = RegInit(0.U(32.W))

  val padBytes = (numBeatsExpected - numBeatsOriginal) * p1.bytesPerBeat.U

  when (!headerReg.compressed && headerReg.compressionPadBytes === 0.U) {
    io.master.header.bits.compressionPadBytes := padBytes
  }
  .elsewhen (!headerReg.encrypted && headerReg.encryptionPadBytes === 0.U) {
    io.master.header.bits.encryptionPadBytes := padBytes
  }
  .elsewhen (!headerReg.ecc && headerReg.eccPadBytes === 0.U) {
    io.master.header.bits.eccPadBytes := padBytes
  }

  io.master.header.bits.len := newLen
  io.master.header.valid := state === sSendHeader

  io.slave.header.ready := state === sRecvHeader

  io.slave.data.ready := state === sRecvData && (beatCnt < numBeatsOriginal)
  io.master.data.valid := state === sSendData

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
        state := sSendHeader
        headerReg := io.slave.header.bits
      }
    }

    // We could probably overlap sSendHeader and sRecvData,
    // but let's play safe here by having a separate sSendHeader state
    is (sSendHeader) {
      when (io.master.header.fire()) {
        state := sRecvData
      }
    }

    is (sRecvData) {
      when (io.slave.data.fire() || beatCnt >= numBeatsOriginal) {
        idReg := io.slave.data.bits.id
        beatCnt := beatCnt + 1.U

        mode match {
          case Mode.Expand =>
            val inputData = Mux(beatCnt < numBeatsOriginal,
                                io.slave.data.bits.data,
                                0.U)
            // Accumulate the input data
            dataOutReg := (dataOutReg >> p1.dataWidth) |
                          (inputData << shiftAmt)

            // Send the output data to master if enough input data
            // have been accumulated
            when (ratioCnt === (ratio - 1).asUInt()) {
              state := sSendData
              ratioCnt := 0.U
            }
            .otherwise {
              ratioCnt := ratioCnt + 1.U
            }

          case Mode.Contract =>
            dataInReg := io.slave.data.bits.data
            state := sSendData

          case Mode.Identity =>
            dataOutReg := io.slave.data.bits.data
            state := sSendData
        }

      }
    }

    is (sSendData) {
      when (io.master.data.fire()) {
        mode match {
          case Mode.Expand | Mode.Identity =>
            // Receive the next header if all beats have sent out
            when (beatCnt === numBeatsExpected) {
              state := sRecvHeader
            }
            .otherwise {
              state := sRecvData
            }
          case Mode.Contract =>
            dataInReg := dataInReg >> p2.dataWidth
            // If enough data have been sent out, either accepting
            // new input data or new header depending on beatCnt
            when (ratioCnt === (ratio - 1).asUInt()) {
              ratioCnt := 0.U

              // Receive the next header if all beats have sent out
              when (beatCnt === numBeatsOriginal) {
                state := sRecvHeader
              }
              .otherwise {
                state := sRecvData
              }

            } .otherwise {
              ratioCnt := ratioCnt + 1.U
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
