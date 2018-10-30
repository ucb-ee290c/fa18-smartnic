// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._

// Reference: http://ptgmedia.pearsoncmg.com/images/art_sklar7_reed-solomon/elementLinks/art_sklar7_reed-solomon.pdf
//
// Reed-Solomon(7, 3) with 3-bit symbol
// #symbols: n=7, #message symbols: k=3, #parity symbols: n-k=4
// With symbolWidth = 3 (or GF(2^3))
// a^0 --> 1
// a^1 --> 2
// a^2 --> 4
// a^3 --> 3
// a^4 --> 6
// a^5 --> 7
// a^6 --> 5
// a^7 --> 1
// We need two things:
// Primitive polynomial f(X) = X^3 + X^1 + 1 --> 1011(2) --> 11(10)
// Generator polynomial g(X) = a^3 * X^0 + a^1 * X^1 + a^0 * X^2 + a^3 * X^3 + X^4
//                      g(X) =   3 * X^0 +   2 * X^1 +   1 * X^2 +   3 * X^3 + X^4
// output(X) = message(X) * X^(n - k) + (message(X) * X^(n - k) % g(X))
// Note that the arithmetic operations in GF(2^m) are different from the conven-
// tional ones
// Addition is simply bit-wise XOR
// Multiplication is slightly more complicated. The result needs to be mod by
// the value representing the primitive polynomial (in this case, 11)
// In general, the operations of two m-bit operands results to a m-bit value
//
case class RSParams(
  val n: Int = 7,
  val k: Int = 3,
  val symbolWidth: Int = 3,
  val gCoeffs: Seq[Int] = Seq(3, 2, 1, 3),
  val fConst: Int = 11
)

// TODO: Evaluate the effectiveness of this combinational circuit
// of doing Galois multiplication versus the simpler approach of using
// a Lookup Table
object GMul {
  def apply(a: UInt, b: UInt, dataWidth: Int, fConst: UInt): UInt = {
    val op1 = a.asTypeOf(UInt(dataWidth.W))
    val op2 = b.asTypeOf(UInt(dataWidth.W))
    val tmp = Wire(Vec(dataWidth, UInt((2 * dataWidth - 1).W)))
    for (i <- dataWidth - 1 to 0 by - 1) {
      val tmp0 = if (i == dataWidth - 1) {
                 Mux(op2(i), op1 << i, 0.U)
               } else {
                 tmp(i + 1) ^ Mux(op2(i), op1 << i, 0.U)
               }

      val tmp1 = if (i == 0) {
                tmp0
              } else {
                Mux(tmp0(i + dataWidth - 1), tmp0 ^ fConst << (i - 1), tmp0)
              }

      tmp(i) := tmp1
    }
    tmp(0)
  }
}

// This module will accept k symbols (io.in.fire() === true.B until received k symbols)
// It will emit n symbols (io.out.fire() === true.B until sent n symbols)
// Each symbol has a width of *symbolWidth*
// FIXME: the incoming data is likely to be a multiple of symbol width
// TODO:
//   + Incorporate CREECBus
//   + symbolWidth of 3 is rather odd. Should test with 4 or 8
class RSEncoder(val param: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(param.symbolWidth.W)))
    val out = new DecoupledIO(UInt(param.symbolWidth.W))
  })

  val inReadyReg = RegInit(true.B)
  val outValidReg = RegInit(false.B)

  val inputSymbolCnt = RegInit(0.U(32.W))
  val outputSymbolCnt = RegInit(0.U(32.W))

  io.in.ready := inReadyReg
  io.out.valid := outValidReg

  when (inputSymbolCnt === param.k.asUInt() - 1.U) {
    inputSymbolCnt := 0.U
    inReadyReg := false.B
  }
  .elsewhen (io.in.fire()) {
    inputSymbolCnt := inputSymbolCnt + 1.U
    outValidReg := true.B
  }

  when (outputSymbolCnt === param.n.asUInt() - 1.U) {
    outputSymbolCnt := 0.U
    outValidReg := false.B
    inReadyReg := true.B
  }
  .elsewhen (io.out.fire()) {
    outputSymbolCnt := outputSymbolCnt + 1.U
  }

  val Regs = RegInit(VecInit(Seq.fill(param.n - param.k)(0.U(param.symbolWidth.W))))
  val inputBitsReg = RegNext(io.in.bits, 0.U)

  // Make sure the arithmetic operations are correct (in Galois field)
  val feedback = Mux(outputSymbolCnt < param.k.asUInt(),
                     inputBitsReg ^ Regs(param.n - param.k - 1), 0.U)
  for (i <- 0 until param.n - param.k) {
    if (i == 0) {
      Regs(0) := GMul(feedback, param.gCoeffs(0).asUInt(),
                      param.symbolWidth, param.fConst.asUInt())
    } else {
      Regs(i) := Regs(i - 1) ^ GMul(feedback, param.gCoeffs(i).asUInt(),
                                    param.symbolWidth, param.fConst.asUInt())
    }
  }

  io.out.bits := Mux(outputSymbolCnt < param.k.asUInt(),
                     inputBitsReg, Regs(param.n - param.k - 1))
}
