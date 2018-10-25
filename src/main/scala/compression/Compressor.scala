// See README.md for license details.

package compression

import chisel3._
import chisel3.util._

class Compressor extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })

  io.out := io.in
}

/*
  maxBytes is the maximum number of bytes supported. The raw width
  is up to 7*maxBytes bits, and the encoded width is up to 8*maxBytes bits.
 */
case class VarintParams(maxBytes: Int = 5) {
  val encodedWidth = maxBytes * 8
  val rawWidth = maxBytes * 7
}

class VarintDecoder(p: VarintParams = new VarintParams()) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(p.encodedWidth.W))
    val out = Output(UInt(p.rawWidth.W))
  })
  val result = Wire(Vec(p.rawWidth, UInt(1.W)))
  //assign all the bits
  for(i <- 0 until p.rawWidth) {
    result(p.rawWidth - i - 1) := io.in(p.encodedWidth - 1 - (7 - i + 15 * (i / 7)))
  }
  io.out := Cat(result)
}

class VarintEncoder(p: VarintParams = new VarintParams()) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(p.rawWidth.W))
    val out = Output(UInt(p.encodedWidth.W))
  })
  //determine how many bytes are valid in the output
  val numBytes = Wire(UInt())
  //TODO: figure out how to do this properly
  when((io.in >> 7.U).asUInt() === 0.U) {
    numBytes := 1.U
  }.elsewhen((io.in >> 14.U).asUInt() === 0.U) {
    numBytes := 2.U
  }.elsewhen((io.in >> 21.U).asUInt() === 0.U) {
    numBytes := 3.U
  }.elsewhen((io.in >> 28.U).asUInt() === 0.U) {
    numBytes := 4.U
  }.otherwise {
    numBytes := 5.U
  }
  val result = Wire(Vec(p.encodedWidth, UInt(1.W)))
  //assign all the bits
  for (i <- 0 until p.encodedWidth) {
    //first bit of byte
    if (i % 8 == 0) {
      //0 on last byte or after
      result(i) := Mux(i.asUInt() >= (numBytes - 1.U) * 8.U, 0.U(1.W), 1.U(1.W))
    }
    else {
      result(i) := io.in(7 - i + 15 * (i / 8))
    }
  }
  io.out := Cat(result)
}