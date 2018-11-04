package compression

import chisel3._
import chisel3.util._
import interconnect._

/*
 * When encode is true, generate an encoder. When false, generate a decoder
 */
case class CoderParams(encode: Boolean = true) {
}

/*
 * This module does the actual differential coding. It has no timing.
 */
class DifferentialCoder(numElements: Int, byteWidth: Int = 8, encode: Boolean = true)
  extends Module {
  val io = IO(new Bundle {
    val input = Input(Vec(numElements, UInt(byteWidth.W)))
    val output = Output(Vec(numElements, UInt(byteWidth.W)))
    val last = Input(UInt(byteWidth.W))
  })
//  //temporary wire
//  val out = Wire(io.output.cloneType)
//  //encode or decode //TODO: use some cool scala thing to do this without intermediate wires
//  for (i <- 0 until io.input.length) {
//    if (encode)
//      out(i) = io.input(i) - (if (i == 0) io.last else io.output(i - 1))
//    else
//      out(i) = io.input(i) + (if (i == 0) io.last else io.output(i - 1))
//  }
//  io.output := out
}

/*
 * This module lives on the CREEC bus and handles accumulation of transactions
 */
//TODO: deal with CREECMetadata. i.e. if read data is not compressed, don't uncompress.
class Coder(p: CoderParams = new CoderParams) extends Module {
  val io = IO(new Bundle {
    val in: CREECBus = {
      if (p.encode)
        Flipped(new CREECWriteBus(new CREECBusParams))
      else
        Flipped(new CREECReadBus(new CREECBusParams))
    }
    val out = {
      if (p.encode)
        new CREECWriteBus(new CREECBusParams)
      else
        new CREECReadBus(new CREECBusParams)
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
  val headerIn = Reg(io.in.header.cloneType)
  val dataIn = Reg(io.in.data.cloneType)
  val headerOut = Reg(io.out.header.cloneType)
  val dataOut = Reg(io.out.data.cloneType)

  //keep track of how many more data beats we need to process
  val beatsToGo = Reg(io.in.header.bits.len.cloneType)
  //TODO: beatsToGo can be a vector that is n long, where n is the number of in-flight
  //      transactions. When a beat comes in, decrement the beatsToGo entry that corresponds
  //      to its id. For the differential encoder, this means we also have to have n
  //      registers for lastValue, each tracked by id.

  //keeps track of the last byte through the encoder across beats
  val lastValue = RegInit(0.U(8.W))

  //convert the current beat of data into a vec of bytes
  val bytesIn = Wire(dataIn.bits.data.asTypeOf(Vec(new CREECBusParams().dataWidth / 8, UInt(8.W))))
  val bytesOut = Wire(bytesIn.cloneType)
  val coder = Module(new DifferentialCoder(new CREECBusParams().dataWidth / 8,
    8, p.encode))
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
    beatsToGo := headerIn.bits.len
    headerOut := {
      val out = new TransactionHeader(new CREECBusParams) with BusAddress
      out.addr := headerIn.bits.addr
      out.id := headerIn.bits.id
      out.len := headerIn.bits.len
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
    io.out.header.enq(headerOut.bits)
    when(io.out.header.fire()) {
      state := sAwaitData
    }
  }.elsewhen(state === sAwaitData) {
    io.in.header.nodeq()
    dataIn := io.in.data.deq()
    dataOut := {
      val out = new TransactionData(new CREECBusParams)
      out.data := bytesOut.asUInt()
      out.id := dataIn.bits.id
      out
    }
    io.out.header.noenq()
    io.out.data.noenq()
    when(io.in.data.fire()) {
      state := sSendData
    }
  }.elsewhen(state === sSendData) {
    io.in.header.nodeq()
    io.in.data.nodeq()
    io.out.header.noenq()
    io.out.data.enq(dataOut.bits)
    lastValue := Mux(p.encode.asBool(), bytesIn.last, bytesOut.last)
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

/*
 * This module performs run-length encoding, by looking at a stream of individual bytes.
 * Each byte that comes in is dealt with one at a time.
 * //TODO: unroll this maybe somehow.
 */
class RunLengthEncoder(creecParams: CREECBusParams = new CREECBusParams,
                       coderParams: CoderParams = new CoderParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Input(UInt(8.W))))
    val out = Decoupled(Output(UInt(8.W)))
  })
  //define state machine
  val sAccept :: sSend :: sStutter :: Nil = Enum(3)
  val state = RegInit(sAccept)

  //register the input and output bytes
  val byteIn = RegInit(0.U(8.W))
  val byteOut = Wire(UInt(8.W))

  //keep track of how many 0's have been seen
  val run = RegInit(0.U(log2Ceil(creecParams.maxBeats * creecParams.dataWidth).W))

  //tell whether or not we need to go to the stutter state.
  val stutter = WireInit(false.B)
  val stutterByte = RegInit(0.U(8.W))

  //state transitions
  when(state === sAccept) {
    byteOut := 0.U
    printf("WAITING TO ACCEPT\n")
    byteIn := io.in.deq()
    io.out.noenq()
    when(io.in.fire()) {
      printf("ACCEPTING  %d\n", io.in.bits)
      state := sSend
    }
  }.elsewhen(state === sSend) {
    printf("WAITING TO SEND\n")
    io.in.nodeq()
    when(byteIn === 0.U) {
      byteOut := 0.U
      stutter := false.B
      run := run + 1.U
      when(run === 0.U) {
        io.out.enq(byteOut)
      }.otherwise {
        io.out.noenq()
        state := sAccept
        byteIn := io.in.bits
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
      printf("SENT\n")
      when(stutter) {
        state := sStutter
      }.otherwise {
        state := sAccept
        byteIn := io.in.bits
      }
    }
  }.otherwise {
    printf("WAITING TO SEND STUTTER\n")
    io.in.nodeq()
    byteOut := stutterByte
    io.out.enq(byteOut)
    when(io.out.fire()) {
      printf("SENT STUTTER\n")
      state := sAccept
      byteIn := io.in.bits
    }
  }
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
    val out = {
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
  val outputSize = 32 + chunkSize * 7 / 6
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