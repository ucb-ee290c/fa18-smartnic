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

class VarintEncoder (bytes: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt((bytes*7).W))
    val out = Output(UInt((bytes*8).W))
  })
  val outWidth = bytes * 8
  val inWidth = bytes * 7
  val result = Wire(Vec(outWidth, UInt(1.W)))
  //assign all the bits
  for(i <- 0 until outWidth) {
    //first bit of byte
    if(i % 8 == 0) {
      //0 on last byte
      result(i) := Mux((outWidth - i == 8).asBool(), 0.U(1.W), 1.U(1.W))
    }
    else {
      result(i) := io.in(7 - i + 15 * (i/8))
    }
  }
  io.out := Cat(result)
}