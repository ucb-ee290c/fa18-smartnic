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
 * This module performs run-length encoding, by looking at a stream of individual bytes.
 * Each byte that comes in is dealt with one at a time. The flag on the input byte is
 * used to tell the encoder to finish off runs that are in progress when the input steam
 * has finished.
 * //TODO: unroll this maybe somehow.
 */
class RunLengthEncoder(creecParams: CREECBusParams = new CREECBusParams,
                       byteWidth: Int = 8) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Input(new FlaggedByte(byteWidth))))
    val out = Decoupled(Output(UInt(byteWidth.W)))
  })
  //define state machine
  val sAccept :: sSend :: sStutter :: Nil = Enum(3)
  val state = RegInit(sAccept)

  //register the input and output bytes
  val byteIn = RegInit(0.U(byteWidth.W))
  val stop = RegInit(false.B)
  val byteOut = Wire(UInt(byteWidth.W))
  dontTouch(byteOut)

  //keep track of how many 0's have been seen
  val run = RegInit(0.U(log2Ceil(creecParams.maxBeats * creecParams.dataWidth).W))

  //tell whether or not we need to go to the stutter state.
  val stutter = WireInit(false.B)
  val stutterByte = RegInit(0.U(byteWidth.W))

  //state transitions
  when(state === sAccept) {
    byteOut := 0.U
    byteIn := io.in.deq().byte
    stop := io.in.deq().flag
    io.out.noenq()
    when(io.in.fire()) {
      state := sSend
    }
  }.elsewhen(state === sSend) {
    io.in.nodeq()
    when(byteIn === 0.U) {
      byteOut := run
      run := run + 1.U
      stutter := stop && run === 0.U
      when(run =/= 0.U && !stop) {
        io.out.noenq()
        state := sAccept
      }.otherwise {
        stutterByte := run
        io.out.enq(byteOut)
        when(stop) {
          run := 0.U
        }
      }
    }.otherwise {
      run := 0.U
      when(run === 0.U) {
        stutter := false.B
        byteOut := byteIn
        io.out.enq(byteOut)
      }.otherwise {
        stutter := true.B
        byteOut := run - 1.U
        io.out.enq(byteOut)
        stutterByte := byteIn
      }
    }
    when(io.out.fire()) {
      when(stutter) {
        state := sStutter
      }.otherwise {
        state := sAccept
      }
    }
  }.otherwise /*sStutter*/ {
    io.in.nodeq()
    byteOut := stutterByte
    io.out.enq(byteOut)
    when(stop) {
      run := 0.U
    }
    when(io.out.fire()) {
      state := sAccept
    }
  }
}

/*
 * When encode is true, generate an encoder. When false, generate a decoder
 */
case class CoderParams(encode: Boolean = true) {
}

/*
 * This module does the actual differential coding. It has no timing.
 */
class DifferentialCoder(numElements: Int = 8, byteWidth: Int = 8, p: CoderParams = new CoderParams)
  extends Module {
  val io = IO(new Bundle {
    val input = Input(Vec(numElements, SInt(byteWidth.W)))
    val output = Output(Vec(numElements, SInt(byteWidth.W)))
    val last = Input(SInt(byteWidth.W))
  })
  //temporary wire
  val out = Wire(io.output.cloneType)
  //encode or decode //TODO: use some cool scala thing to do this without intermediate wires
  for (i <- 0 until io.input.length) {
    if (p.encode) {
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
class CREECDifferentialCoder(creecParams: CREECBusParams = new CREECBusParams,
                             coderParams: CoderParams = new CoderParams) extends Module {
  val io = IO(new Bundle {
    val in: CREECBus = {
      if (coderParams.encode)
        Flipped(new CREECWriteBus(creecParams))
      else
        Flipped(new CREECReadBus(creecParams))
    }
    val out: CREECBus = {
      if (coderParams.encode)
        new CREECWriteBus(creecParams)
      else
        new CREECReadBus(creecParams)
    }
  })
  //States
  //  AwaitHeader: Waiting for a header to come in.
  //  SendHeader: Waiting to forward that header to the next block
  //  AwaitData: Waiting for some data to appear valid.
  //  SendData: Waiting for transformed data to be accepted.
  val sAwaitHeader :: sSendHeader :: sAwaitData :: sSendData :: Nil = Enum(4)
  val state = RegInit(sAwaitHeader)

  //register the header and data inputs once they have been accepted
  val headerIn = Reg(new TransactionHeader with BusAddress with CREECMetadata)
  val headerOut = Reg(new TransactionHeader with BusAddress with CREECMetadata)
  val dataOut = Reg(new TransactionData)

  //keep track of how many more data beats we need to process
  val beatsToGo = Reg(io.in.header.bits.len.cloneType)
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
      val out = Wire(new TransactionHeader(creecParams) with BusAddress with CREECMetadata)
      out.addr := io.in.header.bits.addr
      out.id := io.in.header.bits.id
      out.len := io.in.header.bits.len
      out.compressed := io.in.header.bits.compressed
      out.encrypted := io.in.header.bits.encrypted
      out.ecc := io.in.header.bits.ecc
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
      state := sSendData
    }
  }.otherwise /*sSendData*/ {
    io.in.header.nodeq()
    io.in.data.nodeq()
    io.out.header.noenq()
    io.out.data.enq(dataOut)
    when(beatsToGo === 1.U) {
      lastValue := 0.S
    }.otherwise {
      if (coderParams.encode)
        lastValue := bytesIn.last
      else
        lastValue := bytesOut.last
    }
    when(io.out.data.fire()) {
      when(beatsToGo === 1.U) {
        state := sAwaitHeader
      }.otherwise {
        beatsToGo := beatsToGo - 1.U
        state := sAwaitData
      }
    }
  }
}

//TODO ===========================================================================================
//TODO ===========================================================================================
//TODO ===========================================================================================

class RunLengthDecoder(creecParams: CREECBusParams = new CREECBusParams,
                       byteWidth: Int = 8) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Input(UInt(byteWidth.W))))
    val out = Decoupled(Output(UInt(byteWidth.W)))
  })
  //TODO
  io.out.valid := false.B
  io.in.ready := false.B
  io.out.bits := DontCare
}

/*
 * This module builds a coder, but keeps all the input and output in a buffer.
 * It decides once the buffer is full or the input is all consumed whether to
 * send on the input or the output.
 */
class RegisteredCoder(creecParams: CREECBusParams = new CREECBusParams,
                      coderParams: CoderParams = new CoderParams) extends Module {
  val io = IO(new Bundle {
    val in: CREECBus = {
      if (coderParams.encode)
        Flipped(new CREECWriteBus(creecParams))
      else
        Flipped(new CREECReadBus(creecParams))
    }
    val out: CREECBus = {
      if (coderParams.encode)
        new CREECWriteBus(creecParams)
      else
        new CREECReadBus(creecParams)
    }
  })
  //create state machine definitions
  val sAwaitHeader :: sAwaitData :: sSendHeader :: sSendData :: Nil = Enum(4)
  val state = RegInit(sAwaitHeader)

  //header and data inputs that have been accepted
  val headerIn = Reg(io.in.header.cloneType)
  val dataIn = Reg(io.in.data.cloneType)
  //header and data outputs that will be sent
  val headerOut = Reg(io.out.header.cloneType)
  val dataOut = Reg(io.out.data.cloneType)

  //hold the original input in a buffer
  val bufferedInput = Queue(dataIn.cloneType, creecParams.maxBeats)
  //hold the encoded output in a buffer
  val bufferedOutput = Queue(dataIn.cloneType, creecParams.maxBeats)

  //how many more beats need to be accepted
  val beatsToRead = Reg(io.in.header.bits.len.cloneType)
  //how many more beats need to be sent
  val beatsToWrite = Reg(beatsToRead.cloneType)

  when(state === sAwaitHeader) {
    headerIn := io.in.header.deq()
    beatsToRead := headerIn.bits.len
    headerOut := {
      val out = new TransactionHeader(creecParams) with BusAddress
      out.addr := headerIn.bits.addr
      out.id := headerIn.bits.id
      out.len := headerIn.bits.len
      out
    }
    io.in.data.nodeq()
    io.out.data.noenq()
    io.out.header.noenq()
    when(io.in.header.fire()) {
      state := sAwaitData
    }
  }.elsewhen(state === sAwaitData) {
    io.in.header.nodeq()
    dataIn := io.in.data.deq()
    dataOut := {
      val out = new TransactionData(creecParams)
      //      out.data := bytesOut.asUInt()
      out.id := dataIn.bits.id
      out
    }
    io.out.header.noenq()
    io.out.data.noenq()
    when(io.in.data.fire()) {
      state := sSendData //TODO
    }
  }.elsewhen(state === sSendHeader) {

  }.elsewhen(state === sSendData) {

  }
}

case class CompressorParams(busWidth: Int = 8, chunkSize: Int = 512, compress: Boolean = true) {
}

class SnappyCompressorParams extends CompressorParams {
  val outputSize: Int = 32 + chunkSize * 7 / 6
}

/*
 * Top-level module for whole compression scheme. Slots into CREEC bus.
 */
class Compressor(p: CompressorParams = new CompressorParams()) extends Module {
  val io = IO(new Bundle {
    val in: CREECBus = {
      if (p.compress)
        Flipped(new CREECWriteBus(new BlockDeviceIOBusParams))
      else
        Flipped(new CREECReadBus(new BlockDeviceIOBusParams))
    }
    val out: CREECBus = {
      if (p.compress)
        new CREECWriteBus(new CREECBusParams)
      else
        new CREECReadBus(new CREECBusParams)
    }
  })
  //TODO:
}