package compression

import chisel3._
import chisel3.core.dontTouch
import chisel3.util._
import interconnect._

/*
 * A byte with a boolean flag attached.
 */
class FlaggedByte(width: Int = 8) extends Bundle {
  val byte = UInt(width.W)
  val flag = Bool()

  override def cloneType: FlaggedByte.this.type = new FlaggedByte(width).asInstanceOf[this.type]
}

/*
 * This module performs run-length encoding by looking at a stream of individual bytes.
 * Each byte that comes in is dealt with one at a time. The flag on the input byte is
 * used to tell the encoder to finish off runs that are in progress when the input steam
 * has finished.
 * //TODO: unroll this maybe somehow.
 */
class RunLengthCoder(coderParams: CoderParams,
                     byteWidth: Int = 8)
                    (implicit creecParams: BusParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Input(new FlaggedByte(byteWidth))))
    val out = Decoupled(Output(new FlaggedByte(byteWidth)))
  })
  //define state machine
  val sAccept :: sSend :: sStutter :: sEnd :: Nil = Enum(4)
  val state = RegInit(sAccept)

  //register the input and output bytes
  val byteIn = RegInit(0.U(byteWidth.W))
  val dataOut = Wire(new FlaggedByte(byteWidth))
  dataOut.flag := false.B
  dontTouch(dataOut.byte)
  dontTouch(dataOut.flag)

  //keep track of how many 0's have been seen for encoder, or how many zeros
  //    need to be sent for decoder.
  val run = RegInit(0.U(log2Ceil(creecParams.maxBeats * creecParams.dataWidth).W))

  /*
   * encoder
   */
  val stop = RegInit(false.B)
  val stutter = WireInit(false.B)
  val stutterByte = RegInit(0.U(byteWidth.W))

  /*
   * decoder
   */
  val goToStutter = RegInit(false.B)

  //true when the entire input is consumed and the last byte has been sent. Transitions
  //    into the sEnd state, where the output flag is high. When the output flag is high,
  //    the oputput byte is not valid.
  val finish = Wire(Bool())
  finish := false.B

  //state transitions
  when(state === sAccept) {
    dataOut.byte := 0.U
    finish := false.B
    byteIn := io.in.deq().byte
    stop := io.in.deq().flag
    io.out.noenq()
    when(io.in.fire()) {
      state := sSend
    }
  }.elsewhen(state === sSend) {
    io.in.nodeq()

    if (coderParams.encode) {
      dataOut.byte := Mux(byteIn === 0.U, run, Mux(run === 0.U, byteIn, run - 1.U))
      finish := stop && !stutter
      run := Mux(byteIn =/= 0.U || stop, 0.U, run + 1.U)
      stutter := Mux(byteIn === 0.U, stop && run === 0.U, run =/= 0.U)
      state := Mux(byteIn === 0.U && run =/= 0.U && !stop, sAccept, state)
      stutterByte := Mux(byteIn === 0.U,
        Mux(run === 0.U || stop, run, stutterByte),
        Mux(run =/= 0.U, byteIn, stutterByte))
      when(byteIn === 0.U && run =/= 0.U && !stop) {
        io.out.noenq()
      }.otherwise {
        io.out.enq(dataOut)
      }
    }
    else {
      dataOut.byte := byteIn
      finish := stop && !goToStutter
      run := byteIn - 1.U
      goToStutter := Mux(goToStutter, false.B, byteIn === 0.U)
      when(goToStutter) {
        io.out.noenq()
        state := Mux(byteIn === 0.U,
          Mux(stop, sEnd, sAccept),
          sStutter)
      }.otherwise {
        io.out.enq(dataOut)
      }
    }

    when(io.out.fire()) {
      when(stutter) {
        state := sStutter
      }.elsewhen(finish) {
        state := sEnd
      }.otherwise {
        state := sAccept
      }
    }
  }.elsewhen(state === sStutter) {
    io.in.nodeq()
    io.out.enq(dataOut)
    if (coderParams.encode) {
      dataOut.byte := stutterByte
      finish := true.B && stop
      when(stop) {
        run := 0.U
      }
    }
    else {
      dataOut.byte := 0.U
      finish := run === 0.U && stop
    }
    when(io.out.fire()) {
      when(finish) {
        state := sEnd
      }.otherwise {
        if (coderParams.encode) {
          state := sAccept
        }
        else {
          run := run - 1.U
          when(run === 0.U) {
            state := sAccept
          }.otherwise {
            state := sStutter
          }
        }
      }
    }
  }.otherwise /*sEnd*/ {
    io.in.nodeq()
    dataOut.byte := 77.U
    dataOut.flag := true.B
    io.out.enq(dataOut)
    when(io.out.fire()) {
      state := sAccept
    }
  }
}

/*
 * When encode is true, generate an encoder. When false, generate a decoder
 */
case class CoderParams(encode: Boolean) {
}

/*
 * This module does the actual differential coding. It has no timing.
 */
class DifferentialCoder(numElements: Int = 8,
                        byteWidth: Int = 8,
                        coderParams: CoderParams)
  extends Module {
  val io = IO(new Bundle {
    val input = Input(Vec(numElements, SInt(byteWidth.W)))
    val output = Output(Vec(numElements, SInt(byteWidth.W)))
    val last = Input(SInt(byteWidth.W))
  })
  //temporary wire
  val out = Wire(chiselTypeOf(io.output))
  //encode or decode //TODO: use some cool scala thing to do this without intermediate wires
  for (i <- 0 until io.input.length) {
    if (coderParams.encode) {
      out(i) := io.input(i) - (if (i == 0) io.last else io.input(i - 1))
    }
    else {
      out(i) := io.input(i) + (if (i == 0) io.last else io.output(i - 1))
    }
  }
  io.output := out
}

/*
 * This module lives on the CREEC bus and handles accumulation of transactions.
 */
//TODO: deal with CREECMetadata. i.e. if read data is not compressed, don't uncompress.
class CREECDifferentialCoder(coderParams: CoderParams)
                            (implicit creecParams: BusParams) extends Module {
  val io = IO(new Bundle {
    val in: CREECBus = Flipped(new CREECBus(creecParams))
    val out: CREECBus = new CREECBus(creecParams)
  })
  //States
  //  AwaitHeader: Waiting for a header to come in.
  //  SendHeader: Waiting to forward that header to the next block
  //  AwaitData: Waiting for some data to appear valid.
  //  SendData: Waiting for transformed data to be accepted.
  val sAwaitHeader :: sSendHeader :: sAwaitData :: sSendData :: Nil = Enum(4)
  val state = RegInit(sAwaitHeader)

  //register the header and data inputs once they have been accepted
  val headerIn = Reg(new TransactionHeader(creecParams))
  val headerOut = Reg(new TransactionHeader(creecParams))
  val dataOut = Reg(new TransactionData(creecParams))

  //keep track of how many more data beats we need to process
  val beatsToGo = Reg(chiselTypeOf(io.in.header.bits.len))
  //TODO: beatsToGo can be a vector that is n long, where n is the number of in-flight
  //      transactions. When a beat comes in, decrement the beatsToGo entry that corresponds
  //      to its id. For the differential encoder, this means we also have to have n
  //      registers for lastValue, each tracked by id.

  //keeps track of the last byte through the encoder across beats
  val lastValue = RegInit(0.S(8.W))

  //convert the current beat of data into a vec of bytes
  val bytesIn = Wire(Vec(creecParams.dataWidth / 8, SInt(8.W)))
  val bytesOut = Wire(Vec(creecParams.dataWidth / 8, SInt(8.W)))
  val coder = Module(new DifferentialCoder(creecParams.dataWidth / 8,
    8, CoderParams(encode = coderParams.encode)))
  bytesIn := io.in.data.bits.data.asTypeOf(bytesIn)
  coder.io.input := bytesIn
  bytesOut := coder.io.output
  coder.io.last := lastValue

  //in the AwaitHeader state, accept headers into the header register and transition
  //  to the SendHeader state when a header comes.
  //in the SendHeader state, wait to hand off the header.
  //in the AwaitData state, accept data into the data register and transition
  //  to the SendData state when one arrives.
  //in the SendData state, wait to hand off the data. Transition to the AwaitHeader
  //  state when the last one is handed off.
  when(state === sAwaitHeader) {
    headerIn := io.in.header.deq()
    headerOut := {
      val out = Wire(new TransactionHeader(creecParams))
      out.addr := io.in.header.bits.addr
      out.id := io.in.header.bits.id
      out.len := io.in.header.bits.len
      out.compressed := io.in.header.bits.compressed
      out.encrypted := io.in.header.bits.encrypted
      out.ecc := io.in.header.bits.ecc
      out.compressionPadBytes := 0.U
      out.eccPadBytes := 0.U
      out.encryptionPadBytes := 0.U
      out
    }
    io.in.data.nodeq()
    io.out.data.noenq()
    io.out.header.noenq()
    when(io.in.header.fire()) {
      state := sSendHeader
    }
  }.elsewhen(state === sSendHeader) {
    io.in.data.nodeq()
    io.in.header.nodeq()
    io.out.data.noenq()
    io.out.header.enq(headerOut)
    beatsToGo := headerIn.len
    when(io.out.header.fire()) {
      state := sAwaitData
    }
  }.elsewhen(state === sAwaitData) {
    io.in.header.nodeq()
    io.in.data.deq()
    dataOut := {
      val out = Wire(new TransactionData(creecParams))
      out.data := bytesOut.asUInt()
      out.id := io.in.data.bits.id
      out
    }
    io.out.header.noenq()
    io.out.data.noenq()
    when(io.in.data.fire()) {
      if (coderParams.encode)
        lastValue := bytesIn.last
      else
        lastValue := bytesOut.last
      state := sSendData
    }
  }.otherwise /*sSendData*/ {
    io.in.header.nodeq()
    io.in.data.nodeq()
    io.out.header.noenq()
    io.out.data.enq(dataOut)
    when(io.out.data.fire()) {
      when(beatsToGo === 0.U) {
        state := sAwaitHeader
        lastValue := 0.S
      }.otherwise {
        beatsToGo := beatsToGo - 1.U
        state := sAwaitData
      }
    }
  }
}

/*
 * Basic FIFO that does not use decoupled and has no notion of fullness.
 * This module is dangerous if not used properly. The output is always
 * valid, so pop must be set to high if the output is used in order to
 * advance the pointer. There is no bound checking.
 * //TODO: allow taking multiple out of the queue at once and/or putting multiple in
 * //TODO: turn this into a Mem or a SyncReadMem (++) or a Queue.
 */
class BasicFIFO(width: Int, length: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val push = Input(Bool())
    val pop = Input(Bool())
    val reset = Input(Bool())
    val out = Output(UInt(width.W))
  })
  val fifo = Reg(Vec(length, UInt(width.W)))
  val head = RegInit(0.U((log2Ceil(length) + 1).W))
  val tail = RegInit(0.U((log2Ceil(length) + 1).W))

  io.out := fifo(tail)
  for (i <- 0 until length) {
    when(io.reset) {
      fifo(i) := 0.U
    }
  }
  when(!io.reset) {
    fifo(head) := io.in
  }

  when(io.reset) {
    head := 0.U
    tail := 0.U
  }.otherwise {
    when(io.push) {
      head := head + 1.U
    }
    when(io.pop) {
      tail := tail + 1.U
    }
  }
}

/*
 * CREEC-level block for run-length encoding.
 * //TODO: combine this module with CREECDifferentialCoder
 */
class CREECRunLengthCoder(coderParams: CoderParams)
                         (implicit creecParams: BusParams) extends Module {
  val io = IO(new Bundle {
    val in: CREECBus = Flipped(new CREECBus(creecParams))
    val out: CREECBus = new CREECBus(creecParams)
  })
  //create state machine definitions
  val sAwaitHeader :: sAwaitData :: sProcessData :: sSendHeader :: sAccumulate :: sSendData :: Nil = Enum(6)
  val state = RegInit(sAwaitHeader)

  //register the header and data inputs once they have been accepted
  val headerIn = Reg(new TransactionHeader(creecParams))
  val dataOut = Wire(new TransactionData(creecParams))

  //dataInBuffer holds the beats to be processed, while dataOutBuffer holds processed bytes.
  val dataInBuffer = Module(new BasicFIFO(creecParams.dataWidth, creecParams.maxBeats))
  val dataOutBuffer = Module(new BasicFIFO(creecParams.dataWidth / 8, creecParams.maxBeats * 3 / 2 * 8))

  //how many beats we need to get before the processing starts or send at the end
  val beatsToReceive = Reg(chiselTypeOf(io.in.header.bits.len))
  val beatsToProcess = Reg(chiselTypeOf(io.in.header.bits.len))
  val bytesToSend = RegInit(0.asUInt((creecParams.beatBits + 1).W))

  //keeps track of which byte we are on for both the coded and uncoded sides.
  //    popCounter is for the data being popped off of dataInBuffer, and
  //    pushCounter is for the data being pushed onto dataOutBuffer.
  val counter = RegInit(0.U(log2Ceil(8).W))

  val coder = Module(new RunLengthCoder(coderParams))

  val beatBuilder = Reg(Vec(creecParams.dataWidth / 8, UInt(8.W)))

  //defaults //TODO: is there a way to do this automatically?
  dataInBuffer.io.in := 0.U
  dataInBuffer.io.push := false.B
  dataInBuffer.io.reset := false.B
  dataInBuffer.io.pop := false.B
  dataOutBuffer.io.in := 0.U
  dataOutBuffer.io.push := false.B
  dataOutBuffer.io.reset := false.B
  dataOutBuffer.io.pop := false.B
  coder.io.in.bits.byte := 0.U
  coder.io.in.bits.flag := false.B
  coder.io.in.valid := false.B
  coder.io.out.ready := false.B
  dataOut.data := beatBuilder.asUInt
  dataOut.id := 0.U

  when(state === sAwaitHeader) {
    dataInBuffer.io.reset := false.B
    headerIn := io.in.header.deq()
    dontTouch(headerIn.addr)
    beatsToReceive := io.in.header.deq().len + 1.U
    beatsToProcess := io.in.header.deq().len + 1.U
    io.in.data.nodeq()
    io.out.data.noenq()
    io.out.header.noenq()
    when(io.in.header.fire()) {
      state := sAwaitData
    }
  }.elsewhen(state === sAwaitData) {
    io.in.header.nodeq()
    io.in.data.deq()
    io.out.header.noenq()
    io.out.data.noenq()
    when(io.in.data.fire()) {
      when(beatsToReceive === 0.U) {
        state := sProcessData
      }.otherwise {
        beatsToReceive := beatsToReceive - 1.U
        dataInBuffer.io.in := io.in.data.bits.data
        dataInBuffer.io.push := true.B
        state := sAwaitData
      }
    }
    when(beatsToReceive === 0.U) {
      state := sProcessData
    }
  }.elsewhen(state === sProcessData) {
    io.in.header.nodeq()
    io.in.data.nodeq()
    io.out.header.noenq()
    io.out.data.noenq()

    coder.io.in.enq({
      val flaggedByte = Wire(new FlaggedByte())
      flaggedByte.byte := dataInBuffer.io.out.asTypeOf(Vec(8, UInt(8.W)))(counter)
      flaggedByte.flag := counter === 7.U && beatsToProcess === 1.U
      flaggedByte
    })

    when(!coder.io.out.deq().flag) {
      dataOutBuffer.io.in := coder.io.out.bits.byte
    }

    when(coder.io.out.fire()) {
      dataOutBuffer.io.push := true.B
      when(coder.io.out.bits.flag) {
        state := sSendHeader
        counter := 0.U
        bytesToSend := bytesToSend - headerIn.compressionPadBytes
      }.otherwise {
        bytesToSend := bytesToSend + 1.U
      }
    }

    when(coder.io.in.fire()) {
      when(beatsToProcess =/= 0.U) {
        counter := counter + 1.U
        when(counter === 7.U) {
          beatsToProcess := beatsToProcess - 1.U
          dataInBuffer.io.pop := true.B
        }
      }
    }
  }.elsewhen(state === sSendHeader) {
    io.in.data.nodeq()
    io.in.header.nodeq()
    io.out.data.noenq()
    io.out.header.enq({
      val out = Wire(new TransactionHeader(creecParams))
      out.addr := headerIn.addr
      out.id := headerIn.id
      out.len := Mux(bytesToSend % 8.U === 0.U, (bytesToSend / 8.U) - 1.U, bytesToSend / 8.U)
      out.compressed := coderParams.encode.B
      out.encrypted := headerIn.encrypted
      out.ecc := headerIn.ecc
      out.compressionPadBytes := 8.U - bytesToSend % 8.U
      out.eccPadBytes := headerIn.eccPadBytes
      out.encryptionPadBytes := headerIn.encryptionPadBytes
      out
    })
    when(io.out.header.fire()) {
      state := sAccumulate
    }
  }.elsewhen(state === sAccumulate) {
    io.in.header.nodeq()
    io.in.data.nodeq()
    io.out.header.noenq()
    io.out.data.noenq()
    beatBuilder(counter) := dataOutBuffer.io.out
    dataOutBuffer.io.pop := true.B
    counter := counter + 1.U
    when(counter === 7.U) {
      state := sSendData
      counter := 0.U
    }
  }.otherwise /*sSendData*/ {
    io.in.header.nodeq()
    io.in.data.nodeq()
    io.out.header.noenq()
    io.out.data.enq(dataOut)
    when(io.out.data.fire()) {
      when(bytesToSend <= 8.U) {
        state := sAwaitHeader
        dataInBuffer.io.reset := true.B
        dataOutBuffer.io.reset := true.B
        bytesToSend := 0.U
      }.otherwise {
        bytesToSend := bytesToSend - 8.U
        state := sAccumulate
      }
    }
  }
}

/*
 * Generic module for CREEC coders (differential or runLength).
 */
class CREECCoder(coderParams: CoderParams, operation: String)
                (implicit creecParams: CREECBusParams) extends Module {
  val io = IO(new Bundle {
    val in: CREECBus = Flipped(new CREECBus(creecParams))
    val out: CREECBus = new CREECBus(creecParams)
  })

  require(List("differential", "runLength", "compression").contains(operation))

  val coder: Module = if (operation == "differential")
    Module(new CREECDifferentialCoder(coderParams))
  else if (operation == "runLength")
    Module(new CREECRunLengthCoder(coderParams))
  else
    Module(new Compressor(BusParams.blockDev, compress = coderParams.encode))

  coder.io <> io
}

/*
 * max.
 * TODO:
 */
class SnappyCompressorParams {
  val outputSize: Int = 32 + 512 * 7 / 6
}

/*
 * Top-level module for whole compression scheme. Slots into CREEC bus.
 */
class Compressor(blockDeviceParams: BusParams,
                 compress: Boolean)
                (implicit creecParams: BusParams) extends Module {
  val io = IO(new Bundle {
    val in: CREECBus = Flipped(new CREECBus(blockDeviceParams))
    val out: CREECBus = new CREECBus(creecParams)
  })
  val differential = Module(new CREECDifferentialCoder(CoderParams(encode = compress)))
  val runLength = Module(new CREECRunLengthCoder(CoderParams(encode = compress)))

  if(compress) {
    io.in <> differential.io.in
    differential.io.out <> runLength.io.in
    io.out <> runLength.io.out
  }
  else {
    io.in <> runLength.io.in
    runLength.io.out <> differential.io.in
    io.out <> differential.io.out
  }
}

class CREECDifferentialCoderModel(encode: Boolean) extends
  SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    Seq(CREECHighLevelTransaction(CompressionUtils.differential(in.data.toList, encode), in.addr))
  }
}

//TODO: don't copy and paste this from the model below
class CREECRunLengthCoderModel(encode: Boolean) extends
  SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    val processedData = CompressionUtils.runLength(in.data.toList, encode)
    // TODO: hard-coded padding to 8 bytes in high level model is bad practice
    val paddedData = processedData.padTo(math.ceil(processedData.length / 8.0).toInt * 8, 0.asInstanceOf[Byte])
    Seq(in.copy(
      data = paddedData,
      compressed = encode,
      compressionPadBytes = paddedData.length - processedData.length))
  }
}

class CompressorModel(compress: Boolean) extends
  SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    compress match {
      case true =>
        val processedData = CompressionUtils.compress(in.data.toList, compress)
        // TODO: hard-coded padding to 8 bytes in high level model is bad practice
        val paddedData = processedData.padTo(math.ceil(processedData.length / 8.0).toInt * 8, 0.asInstanceOf[Byte])
        Seq(in.copy(
          data = paddedData,
          compressed = compress,
          compressionPadBytes = paddedData.length - processedData.length))
      case false =>
        val inPadStrip = in.data.take(in.data.length - in.compressionPadBytes)
        Seq(in.copy(
          data = CompressionUtils.compress(inPadStrip, compress),
          compressed = compress,
          compressionPadBytes = 0
        ))
    }
  }
}