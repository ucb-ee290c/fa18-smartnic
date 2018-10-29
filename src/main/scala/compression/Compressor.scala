// See README.md for license details.

package compression

import chisel3._
import chisel3.util._
import interconnect._

/*
 * When encode is true, generate an encoder. When false, generate a decoder
 */
case class CoderParams(encode: Boolean = true) {
}

class DifferentialCoder(p: CoderParams = new CoderParams) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new Bus(new CREECBusParams))
    val master = new Bus(new CREECBusParams)
  })
  //States
  //  AwaitRequest: Waiting for a request to come in.
  //  SendRequest: Waiting to forward that request to the next block
  //  AwaitData: Waiting for some data to appear valid.
  //  SendData: Waiting for transformed data to be accepted.
  val sAwaitRequest :: sSendRequest :: sAwaitData :: sSendData :: Nil = Enum(4)
  val state = RegInit(sAwaitRequest)

  //keeps track of the last byte through the encoder across beats
  val lastValue = RegInit(0.U(8.W))

  //convert the current beat of data into a vec of bytes
  val bytesIn = dataIn.bits.data.asTypeOf(Vec(io.slave.p.dataWidth / 8, UInt(8.W)))
  val bytesOut = bytesIn.cloneType

  //encode or decode //TODO: use some cool scala thing to do this without intermediate wires
  for (i <- 0 until bytesIn.length) {
    if (p.encode)
      bytesOut(i) = bytesIn(i) - (if (i == 0) lastValue else bytesIn(i - 1))
    else
      bytesOut(i) = bytesIn(i) + (if (i == 0) lastValue else bytesOut(i - 1))
  }

  //hold the request and data inputs
  val requestIn = Reg(io.slave.rdReq.cloneType)
  val dataIn = Reg(io.slave.rdData.cloneType)
  val requestOut = Reg(io.master.wrReq.cloneType)
  val dataOut = Reg(io.master.wrData.cloneType)
  //keep track of how many more data beats we need to process
  val beatsToGo = Reg(io.slave.rdReq.bits.len.cloneType)

  //in the AwaitRequest state, accept requests into the request register and transition
  //  to the SendRequest state when a request comes.
  //in the SendRequest state, wait to hand off the request.
  //in the AwaitData state, accept data into the data register and transition
  //  to the SendData state when one arrives.
  //in the SendData state, wait to hand off the data. Transition to the AwaitRequest
  //  state when the last one is handed off.
  when(state === sAwaitRequest) {
    requestIn := io.slave.rdReq.deq()
    beatsToGo := requestIn.bits.len
    requestOut := {
      val out = new WriteRequest(io.master.p)
      out.addr := requestIn.bits.addr
      out.id := requestIn.bits.id
      out.len := requestIn.bits.len
      out
    }
    io.slave.rdData.nodeq()
    io.master.wrData.noenq()
    io.master.wrReq.noenq()
    when(io.slave.rdReq.fire()) {
      state := sSendRequest
    }
  }.elsewhen(state === sSendRequest) {
    io.slave.rdData.nodeq()
    io.slave.rdReq.nodeq()
    io.master.wrData.noenq()
    io.master.wrReq.enq(requestOut.bits)
    when(io.master.wrReq.fire()) {
      state := sAwaitData
    }
  }.elsewhen(state === sAwaitData) {
    io.slave.rdReq.nodeq()
    dataIn := io.slave.rdData.deq()
    dataOut := {
      val out = new WriteData(io.master.p)
      out.data := bytesOut.asUInt()
      out.id := dataIn.bits.id
      out
    }
    io.master.wrReq.noenq()
    io.master.wrData.noenq()
    when(io.slave.rdData.fire()) {
      state := sSendData
    }
  }.elsewhen(state === sSendData) {
    io.slave.rdReq.nodeq()
    io.slave.rdData.nodeq()
    io.master.wrReq.noenq()
    io.master.wrData.enq(dataOut.bits)
    lastValue := Mux(p.encode.asBool(), bytesIn.last, bytesOut.last)
    when(io.slave.wrData.fire()) {
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
    val slave = Flipped(new Bus(new BlockDeviceIOBusParams))
    val master = new Bus(new CREECBusParams)
  })
  val varintEncoder = Module(new VarintEncoder())

  //States
  //  Idle: Initial state. Nothing is happening.
  //  Accepting: The raw data buffer is being filled.
  //  Compressing: The compression algorithm is running.
  //  Sending: The compression is done, and the output buffer is being consumed.
  val sIdle :: sAccepting :: sCompressing :: sSending :: Nil = Enum(4)
  val state = RegInit(sAccepting)

  //Points to the location in the buffers for accepting and sending data
  val bufferFillPointer = RegInit(0.U(9.W))

  //Buffer for the raw data
  val rawData = Mem(p.chunkSize, UInt(8.W))
  //Buffer for the compressed data
  val compressedData = Mem(p.outputSize, UInt(8.W))

  //Flag that tells when the compression is done
  val compressionDone = RegInit(false.B)

  //State machine
  switch(state) {
    is(sIdle) {
      when(fire_accept(false.B)) {
        state := sAccepting
      }.otherwise {
        bufferFillPointer := 0.U
        compressionDone := false.B
      }
    }
    is(sAccepting) {
      when(bufferFillPointer === (p.chunkSize - 1).U) {
        bufferFillPointer := 0.U
        state := sCompressing
      }.elsewhen(fire_accept(false.B)) {
        bufferFillPointer := bufferFillPointer + 1.U
        rawData.write(bufferFillPointer, io.slave.rdData.bits.data)
      }
    }
    is(sCompressing) {
      //do nothing for now
      state := sSending
      compressionDone := true.B
    }
    is(sSending) {
      when(bufferFillPointer === (p.chunkSize - 1).U) {
        bufferFillPointer := 0.U
        state := sIdle
      }.elsewhen(fire_send(false.B)) {
        bufferFillPointer := bufferFillPointer + 1.U
        io.master.wrData.bits.data := compressedData.read(bufferFillPointer)
      }
    }
  }

  io.slave.wrData.ready := fire_accept(io.slave.wrData.valid)
  io.master.wrData.valid := fire_send(io.master.wrData.ready)

  def fire_accept(exclude: Bool, includes: Bool*) = {
    val values = Array(
      io.slave.wrData.valid,
      (state === sIdle || state === sAccepting) && bufferFillPointer < (p.chunkSize - 1).U
    )
    (values.filter(_ ne exclude) ++ includes).reduce(_ && _)
  }

  def fire_send(exclude: Bool, includes: Bool*) = {
    val values = Array(
      io.master.wrData.ready,
      (state === sSending || state === sCompressing) && compressionDone
    )
    (values.filter(_ ne exclude) ++ includes).reduce(_ && _)
  }
}


// TODO create a VarInt (or VarData) Chisel datatype extension
class VarintEncodedByte extends Bundle {
  val data = UInt(7.W)
  val valid = Bool()

  override def cloneType: this.type = new VarintEncodedByte().asInstanceOf[this.type]
}

/*
  maxBytes is the maximum number of bytes supported. The raw width
  is up to 7*maxBytes bits, and the encoded width is up to 8*maxBytes bits.
 */
case class VarintParams(maxBytes: Int = 5) {
  // TODO add all the bitwidth params here
  val bytes: Int = maxBytes
}


class VarintDecoder(p: VarintParams = VarintParams()) extends Module {
  val io = IO(new Bundle {
    // TODO fix the bitwidths
    val in = Input(UInt((p.bytes * 8).W))
    val out = Output(UInt((p.bytes * 7).W))
  })
  val inEncoded: Vec[UInt] = io.in.asTypeOf(Vec(p.bytes, UInt(8.W)))
  val outDecoded: Vec[UInt] = VecInit(inEncoded.map {
    encByte => encByte.asTypeOf(new VarintEncodedByte).data
  })
  io.out := outDecoded.asUInt()
}

/**
  * Varint Encoding
  * MSB                            LSB
  * Let   in = 0000000 |1010100 |0000000 |1010000
  * Then out = 00000000|10101001|00000001|10100001
  * LSB of each byte of out indicates a valid 7-bit data field (copied from in)
  */
class VarintEncoder(p: VarintParams = VarintParams()) extends Module {
  val io = IO(new Bundle {
    // TODO: the input bitwidth is wrong (should be a round up to multiple of 8)
    val in = Input(UInt((p.bytes * 7).W))
    val out = Output(UInt((p.bytes * 8).W))
  })
  val inCoded: Vec[UInt] = io.in.asTypeOf(Vec(p.bytes, UInt(7.W)))
  val numBytes: UInt = (Log2(io.in) >> 3.U).asUInt() + 1.U
  val outCoded: Vec[VarintEncodedByte] = VecInit(inCoded.zipWithIndex.map { case (inByte, i) => {
    val v = Wire(new VarintEncodedByte())
    v.data := inByte
    v.valid := i.U < numBytes
    v
  }
  })
  io.out := outCoded.asUInt()
}