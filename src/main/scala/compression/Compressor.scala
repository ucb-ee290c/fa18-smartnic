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
private class DifferentialCoder(numElements: Int, byteWidth: Int = 8, encode: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val input = Input(Vec(numElements, UInt(byteWidth.W)))
    val output = Output(Vec(numElements, UInt(byteWidth.W)))
    val last = Input(UInt(byteWidth.W))
  })
  //temporary wire
  val out = Wire(io.output.cloneType)
  //encode or decode //TODO: use some cool scala thing to do this without intermediate wires
  for (i <- 0 until io.input.length) {
    if (encode)
      out(i) = io.input(i) - (if (i == 0) io.last else io.output(i - 1))
    else
      out(i) = io.input(i) + (if (i == 0) io.last else io.output(i - 1))
  }
  io.output := out
}

/*
 * This module lives on the CREEC bus and handles accumulation of transactions
 */
//TODO: deal with CREECMetadata
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
  //  AwaitRequest: Waiting for a request to come in.
  //  SendRequest: Waiting to forward that request to the next block
  //  AwaitData: Waiting for some data to appear valid.
  //  SendData: Waiting for transformed data to be accepted.
  val sAwaitRequest :: sSendRequest :: sAwaitData :: sSendData :: Nil = Enum(4)
  val state = RegInit(sAwaitRequest)

  //register the request and data inputs once they have been accepted
  val requestIn = Reg(io.in.header.bits.cloneType)
  val dataIn = Reg(io.in.data.bits.cloneType)
  val requestOut = Reg(io.out.header.cloneType)
  val dataOut = Reg(io.out.data.bits.cloneType)

  //keep track of how many more data beats we need to process
  val beatsToGo = Reg(io.in.header.bits.len.cloneType)
  //TODO: beatsToGo can be a vector that is n long, where n is the number of in-flight
  //      requests. When a beat comes in, decrement the beatsToGo entry that corresponds
  //      to its id. For the differential encoder, this means we also have to have n
  //      registers for lastValue, each tracked by id.

  //keeps track of the last byte through the encoder across beats
  val lastValue = RegInit(0.U(8.W))

  //convert the current beat of data into a vec of bytes
  val bytesIn = Wire(dataIn.bits.data.asTypeOf(Vec(new CREECBusParams().dataWidth / 8, UInt(8.W))))
  val bytesOut = Wire(bytesIn.cloneType)
  val coder = Module(new DifferentialCoder(new CREECBusParams().dataWidth / 8, 8, p.encode))
  coder.io.input := bytesIn
  bytesOut := coder.io.output
  coder.io.last := lastValue

  //in the AwaitRequest state, accept requests into the request register and transition
  //  to the SendRequest state when a request comes.
  //in the SendRequest state, wait to hand off the request.
  //in the AwaitData state, accept data into the data register and transition
  //  to the SendData state when one arrives.
  //in the SendData state, wait to hand off the data. Transition to the AwaitRequest
  //  state when the last one is handed off.
  when(state === sAwaitRequest) {
    requestIn := io.in.header.deq()
    beatsToGo := requestIn.bits.len
    requestOut := {
      val out = new TransactionHeader(new CREECBusParams) with BusAddress
      out.addr := requestIn.bits.addr
      out.id := requestIn.bits.id
      out.len := requestIn.bits.len
      out
    }
    io.in.data.nodeq()
    io.out.data.noenq()
    io.out.header.noenq()
    when(io.in.header.fire()) {
      state := sSendRequest
    }
  }.elsewhen(state === sSendRequest) {
    io.in.data.nodeq()
    io.in.header.nodeq()
    io.out.data.noenq()
    io.out.header.enq(requestOut.bits)
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
        state := sAwaitRequest
      }.otherwise {
        beatsToGo := beatsToGo - 1.U
        state := sAwaitData
      }
    }
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