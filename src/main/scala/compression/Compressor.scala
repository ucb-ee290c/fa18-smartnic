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
  maxBytes is the maximum number of bytes supported. The input width
  is up to 7*maxBytes bits, and the output width is up to 8*maxBytes bits.
 */
case class VarintParams (maxBytes: Int) {
  val outWidth = maxBytes * 8
  val inWidth = maxBytes * 7
}

class VarintEncoder (p: VarintParams) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(p.inWidth.W))
    val out = Output(UInt(p.outWidth.W))
//    val outBytes = Output(UInt(log2Up(p.maxBytes).W))
  })
  val numBytes = Mux((io.in >> Log2(io.in)).asUInt() === 0.U, Log2(io.in), Log2(io.in) + 1.U)
  val result = Wire(Vec(p.outWidth, UInt(1.W)))
  //assign all the bits
  for(i <- 0 until p.outWidth) {
    //first bit of byte
    if(i % 8 == 0) {
      //0 on last byte
      result(i) := Mux((p.outWidth - i == (p.maxBytes-1)*8).asBool(), 0.U(1.W), 1.U(1.W))
    }
    else {
      result(i) := io.in(7 - i + 15 * (i/8))
    }
  }
  io.out := Cat(result)
}