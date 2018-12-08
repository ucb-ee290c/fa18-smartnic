// See README.md for license details.

package ecc

import chisel3._

// References:
// [1] http://ptgmedia.pearsoncmg.com/images/art_sklar7_reed-solomon/elementLinks/art_sklar7_reed-solomon.pdf
// [2] https://downloads.bbc.co.uk/rd/pubs/whp/whp-pdf-files/WHP031.pdf
// Here is a brief description on one example of Reed-Solomon encoder
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
// In general, a GF operations of two m-bit operands results to a m-bit value
//
case class RSParams(
  // Sample configuration of RS(7, 3) with 3-bit symbol
  val n: Int = 7,
  val k: Int = 3,
  val symbolWidth: Int = 3,
  val gCoeffs: Seq[Int] = Seq(3, 2, 1, 3),
  val fConst: Int = 11,
  val Log2Val: Seq[Int] = Seq(1, 2, 4, 3, 6, 7, 5, 1),
  val invTable: Seq[Int] = Seq(1, 5, 6, 7, 2, 3, 4) // inv(i) == invTable(i - 1)
) {

  // GF Arithmetic operations
  object GFOp {

    // TODO: pipeline this operation to improve timing.
    def mul(a: UInt, b: UInt): UInt = {
      val op1 = a.asTypeOf(UInt(symbolWidth.W))
      val op2 = b.asTypeOf(UInt(symbolWidth.W))

      val result = Seq.fill(symbolWidth)(
        Wire(UInt((2 * symbolWidth - 1).W))
      ).zipWithIndex.foldRight(0.U) {
        case ((nextWire, idx), prevWire) => {
          val currentMultResult = prevWire ^ Mux(op2(idx), op1 << idx, 0.U)

          if (idx == 0) {
            nextWire := currentMultResult
          }
          else {
            nextWire := Mux(currentMultResult(idx + symbolWidth - 1),
                          currentMultResult ^ (fConst.asUInt() << (idx - 1)),
                          currentMultResult)
          }
          nextWire
        }
      }
      result
    }

    def shlByOne(a: UInt): UInt = {
      val mask = math.pow(2, symbolWidth).toInt - 1
      val aShifted = a.asTypeOf(UInt((symbolWidth + 1).W)) << 1
      val result = Mux(aShifted(symbolWidth),
        (aShifted & mask.asUInt()) ^ (fConst.asUInt() & mask.asUInt()),
        aShifted)
      result
    }

    // Inversion can be implemented simply by powering
    // the input by 2**(symbolWidth) thanks to GF.
    // However, this leads to a very cumbersome hardware logic
    // and potentially worsen the critical path.
    // TODO: pipeline this operation to improve timing.
    def inv(a: UInt): UInt = {
      val op = a.asTypeOf(UInt(symbolWidth.W))
      val numVals = math.pow(2, symbolWidth).toInt - 1

      val rootsFromOp = Seq.fill(numVals)(Wire(UInt(symbolWidth.W))).scan(op) {
        (prevWire, nextWire) => {
          nextWire := shlByOne(prevWire)
          nextWire
        }
      }

      val rootsFromOne = Seq.fill(numVals)(Wire(UInt(symbolWidth.W))).scan(1.U) {
        (prevWire, nextWire) => {
          nextWire := shlByOne(prevWire)
          nextWire
        }
      }

      val rootsMasked = rootsFromOp.zip(rootsFromOne).map {
        case (rootFromOp, rootFromOne) =>
          Mux(rootFromOp === 1.U, rootFromOne, 0.U)
      }

      val result = rootsMasked.reduce(_ | _)
      result
    }
  }
}

// Precomputed RSParams
object RSParams {
  // RS(16, 8) with 8 bit symbols
  val RS16_8_8 = {
    val numSymbols = 16
    val numMsgs = 8
    val symbolWidth = 8
    val rs = new RSCode(numSymbols, numMsgs, symbolWidth)
    RSParams(
      n = numSymbols,
      k = numMsgs,
      symbolWidth = symbolWidth,
      gCoeffs = rs.gCoeffs,
      fConst = rs.fConst,
      rs.Log2Val,
      rs.invTable
    )
  }
}
