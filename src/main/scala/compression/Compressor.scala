package compression

import chisel3._
import chisel3.util._
import interconnect._

/*
 * When encode is true, generate an encoder. When false, generate a decoder
 */
case class CoderParams(encode: Boolean = true, p: CREECBusParams = new CREECBusParams) {
}

class DifferentialBlock() extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(new CREECBusParams))
    val master = new CREECBus(new CREECBusParams)
  })
  //encoder for write path
  val encoder = Module(new DifferentialCoder(new CoderParams(true)))
  //decoder for read path
  val decoder = Module(new DifferentialCoder(new CoderParams(false)))

  //hook up encoder
  encoder.io.inReq := io.slave.wrReq
  encoder.io.inData := io.slave.wrData
  io.master.wrReq := encoder.io.outReq
  io.master.wrData := encoder.io.outData

  //hook up decoder
  decoder.io.inReq := io.slave.rdReq
  decoder.io.inData := io.master.rdData
  io.master.rdReq := decoder.io.outReq
  io.slave.rdData := decoder.io.outData
}

//TODO: deal with CREECMetadata
class DifferentialCoder(p: CoderParams = new CoderParams) extends Module {
  val io = IO(new Bundle {
    val inReq = Flipped(Decoupled(new ReadRequest(p.p) with BusAddress))
    val inData = Flipped(Decoupled(new ReadData(p.p)))
    val outReq = Decoupled(new WriteRequest(p.p) with BusAddress)
    val outData = Decoupled(new WriteData(p.p))
  })
  //States
  //  AwaitRequest: Waiting for a request to come in.
  //  SendRequest: Waiting to forward that request to the next block
  //  AwaitData: Waiting for some data to appear valid.
  //  SendData: Waiting for transformed data to be accepted.
  val sAwaitRequest :: sSendRequest :: sAwaitData :: sSendData :: Nil = Enum(4)
  val state = RegInit(sAwaitRequest)

  //register the request and data inputs once they have been accepted
  val requestIn = Reg(io.inReq.cloneType)
  val dataIn = Reg(io.inData.cloneType)
  val requestOut = Reg(io.outReq.cloneType)
  val dataOut = Reg(io.outData.cloneType)

  //keep track of how many more data beats we need to process
  val beatsToGo = Reg(io.inReq.bits.len.cloneType)
  //TODO: beatsToGo can be a vector that is n long, where n is the number of in-flight
  //      requests. When a beat comes in, decrement the beatsToGo entry that corresponds
  //      to its id. For the differential encoder, this means we also have to have n
  //      registers for lastValue, each tracked by id.

  //keeps track of the last byte through the encoder across beats
  val lastValue = RegInit(0.U(8.W))

  //convert the current beat of data into a vec of bytes
  val bytesIn = dataIn.bits.data.asTypeOf(Vec(p.p.dataWidth / 8, UInt(8.W)))
  val bytesOut = Wire(bytesIn.cloneType)

  //encode or decode //TODO: use some cool scala thing to do this without intermediate wires
  for (i <- 0 until bytesIn.length) {
    if (p.encode)
      bytesOut(i) = bytesIn(i) - (if (i == 0) lastValue else bytesIn(i - 1))
    else
      bytesOut(i) = bytesIn(i) + (if (i == 0) lastValue else bytesOut(i - 1))
  }

  //in the AwaitRequest state, accept requests into the request register and transition
  //  to the SendRequest state when a request comes.
  //in the SendRequest state, wait to hand off the request.
  //in the AwaitData state, accept data into the data register and transition
  //  to the SendData state when one arrives.
  //in the SendData state, wait to hand off the data. Transition to the AwaitRequest
  //  state when the last one is handed off.
  when(state === sAwaitRequest) {
    requestIn := io.inReq.deq()
    beatsToGo := requestIn.bits.len
    requestOut := {
      val out = new WriteRequest(p.p) with BusAddress
      out.addr := requestIn.bits.addr
      out.id := requestIn.bits.id
      out.len := requestIn.bits.len
      out
    }
    io.inData.nodeq()
    io.outData.noenq()
    io.outReq.noenq()
    when(io.inReq.fire()) {
      state := sSendRequest
    }
  }.elsewhen(state === sSendRequest) {
    io.inData.nodeq()
    io.inReq.nodeq()
    io.outData.noenq()
    io.outReq.enq(requestOut.bits)
    when(io.outReq.fire()) {
      state := sAwaitData
    }
  }.elsewhen(state === sAwaitData) {
    io.inReq.nodeq()
    dataIn := io.inData.deq()
    dataOut := {
      val out = new WriteData(p.p)
      out.data := bytesOut.asUInt()
      out.id := dataIn.bits.id
      out
    }
    io.outReq.noenq()
    io.outData.noenq()
    when(io.inData.fire()) {
      state := sSendData
    }
  }.elsewhen(state === sSendData) {
    io.inReq.nodeq()
    io.inData.nodeq()
    io.outReq.noenq()
    io.outData.enq(dataOut.bits)
    lastValue := Mux(p.encode.asBool(), bytesIn.last, bytesOut.last)
    when(io.outData.fire()) {
      when(beatsToGo === 1.U) {
        state := sAwaitRequest
      }.otherwise {
        beatsToGo := beatsToGo - 1.U
        state := sAwaitData
      }
    }
  }
}

case class CompressorParams(busWidth: Int = 8, chunkSize: Int = 512) {
  val outputSize = 32 + chunkSize * 7 / 6
}

class Compressor(p: CompressorParams = new CompressorParams()) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(new BlockDeviceIOBusParams))
    val master = new CREECBus(new CREECBusParams)
  })
}