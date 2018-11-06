package ecc
import scala.math

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
//import interconnect._

// Software implementation of the RSEncoder
class RSCode(numSyms: Int, symbolWidth: Int,
             msgs: Seq[Int]) {
  val numVals = BigInt(2).pow(symbolWidth).toInt
  var Log2Val: Array[Int] = new Array[Int](numVals)
  var Val2Log: Array[Int] = new Array[Int](numVals)
  // Primitive Polynomial
  val fConst = {
    symbolWidth match {
      // x^3 + x + 1
      case 3 => Integer.parseInt("1011", 2)
      // x^4 + x + 1
      case 4 => Integer.parseInt("10011", 2)
      // x^5 + x^2 + 1
      case 5 => Integer.parseInt("100101", 2)
      // x^6 + x + 1
      case 6 => Integer.parseInt("1000011", 2)
      // x^7 + x^3 + 1
      case 7 => Integer.parseInt("10001001", 2)
      // x^8 + x^4 + x^3 + x^2 + 1
      case 8 => Integer.parseInt("100011101", 2)
      // x^16 + x^12 + x^3 + x + 1
      case 16 => Integer.parseInt("10001000000001011", 2)
      case _ => 0
    }
  }

  require(fConst != 0, "Unsupported symbol width!")

  Log2Val(0) = 1
  Log2Val(1) = 2 // according to the spec, usually choose a^1 to be 2
  for (i <- 2 until numVals) {
    Log2Val(i) = Log2Val(i - 1) << 1
    if (Log2Val(i) >= numVals) {
      Log2Val(i) = (Log2Val(i) % numVals) ^ (fConst % numVals)
    }
  }

  for (i <- 0 until numVals) {
    Val2Log(Log2Val(i)) = i
  }

  def add(a: Int, b: Int): Int = {
    a ^ b
  }

  def mul(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      0
    } else {
      Log2Val((Val2Log(a) + Val2Log(b)) % (numVals - 1))
    }
  }

  def pow(a: Int, n: Int): Int = {
    var t: Int = 1
    for (i <- 0 until n) {
      t = mul(a, t)
    }
    t
  }

  def printValRootTable() {
    for (i <- 0 until numVals) {
      printf("Val2Log(%d) = %d\n", i, Val2Log(i))
    }
  }

  def printLogRootTable() {
    for (i <- 0 until numVals) {
      printf("Log2Val(%d) = %d\n", i, Log2Val(i))
    }
  }

  def inv(a: Int): Int = {
    //pow(a, numVals - 2)
    var tmp = a
    var result = 1
    while (tmp != 1) {
      tmp = tmp << 1
      if (tmp >= numVals) {
        tmp = (tmp % numVals) ^ (fConst % numVals)
      }
      result = result << 1
      if (result >= numVals) {
        result = (result % numVals) ^ (fConst % numVals)
      }

    }
    result
  }

  def test_t() {
    for (i <- 0 until 16) {
      val a = Log2Val(i)
      val tmp0 = add(mul(7, pow(a, 4)), mul(7, pow(a, 3)))
      val tmp1 = add(tmp0, mul(9, pow(a, 2)))
      printf("result %d: %d %d\n", i, tmp1, a)
    }

    val r0 = 12
    val tmp2 = mul(3, pow(r0, 3)) ^ mul(14, pow(r0, 2))
    val tmp3 = mul(7, pow(r0, 2))
    val tmp4 = inv(mul(tmp3, r0))
    val tmp5 = mul(tmp2, tmp4)
    printf("Y0=%d\n", tmp5)

    val r1 = 13
    val tmp6 = mul(3, pow(r1, 3)) ^ mul(14, pow(r1, 2))
    val tmp7 = mul(7, pow(r1, 2))
    val tmp8 = inv(mul(tmp7, r1))
    val tmp9 = mul(tmp6, tmp8)
    printf("Y1=%d\n", tmp9)

  }

  def test() {
  // d - 1 = 4
  val d = 5
  var o_a: Array[Int] = new Array[Int](d)
  var o_b: Array[Int] = new Array[Int](d)
  var tmp_ob: Array[Int] = new Array[Int](d)
  var t_a: Array[Int] = new Array[Int](d)
  var t_b: Array[Int] = new Array[Int](d)
  var tmp_tb: Array[Int] = new Array[Int](d)

  o_a(0) = 0
  o_a(1) = 0
  o_a(2) = 0
  o_a(3) = 0
  o_a(4) = 1
  o_b(0) = 3
  o_b(1) = 4
  o_b(2) = 12
  o_b(3) = 15
  o_b(4) = 0

  t_a(0) = 0
  t_a(1) = 0
  t_a(2) = 0
  t_a(3) = 0
  t_a(4) = 0
  t_b(0) = 1
  t_b(1) = 0
  t_b(2) = 0
  t_b(3) = 0
  t_b(4) = 0

  for (i <- 0 until d) {
    tmp_ob(0) = o_b(0)
    tmp_ob(1) = o_b(1)
    tmp_ob(2) = o_b(2)
    tmp_ob(3) = o_b(3)
    tmp_ob(4) = o_b(4)

    tmp_tb(0) = t_b(0)
    tmp_tb(1) = t_b(1)
    tmp_tb(2) = t_b(2)
    tmp_tb(3) = t_b(3)
    tmp_tb(4) = t_b(4)

    val tmp4_oa = mul(tmp_ob(3), o_a(4))
    val tmp4_ob = mul(o_a(4), tmp_ob(3))
    o_b(4) = add(tmp4_oa, tmp4_ob)
    val tmp3_oa = mul(tmp_ob(3), o_a(3))
    val tmp3_ob = mul(o_a(4), tmp_ob(2))
    o_b(3) = add(tmp3_oa, tmp3_ob)
    val tmp2_oa = mul(tmp_ob(3), o_a(2))
    val tmp2_ob = mul(o_a(4), tmp_ob(1))
    o_b(2) = add(tmp2_oa, tmp2_ob)
    val tmp1_oa = mul(tmp_ob(3), o_a(1))
    val tmp1_ob = mul(o_a(4), tmp_ob(0))
    o_b(1) = add(tmp1_oa, tmp1_ob)
    val tmp0_oa = mul(tmp_ob(3), o_a(0))
    val tmp0_ob = mul(o_a(4), 0)
    o_b(0) = add(tmp0_oa, tmp0_ob)

    val tmp4_ta = mul(tmp_ob(3), t_a(4))
    val tmp4_tb = mul(o_a(4), tmp_tb(3))
    t_b(4) = add(tmp4_ta, tmp4_tb)
    val tmp3_ta = mul(tmp_ob(3), t_a(3))
    val tmp3_tb = mul(o_a(4), tmp_tb(2))
    t_b(3) = add(tmp3_ta, tmp3_tb)
    val tmp2_ta = mul(tmp_ob(3), t_a(2))
    val tmp2_tb = mul(o_a(4), tmp_tb(1))
    t_b(2) = add(tmp2_ta, tmp2_tb)
    val tmp1_ta = mul(tmp_ob(3), t_a(1))
    val tmp1_tb = mul(o_a(4), tmp_tb(0))
    t_b(1) = add(tmp1_ta, tmp1_tb)
    val tmp0_ta = mul(tmp_ob(3), t_a(0))
    val tmp0_tb = mul(o_a(4), 0)
    t_b(0) = add(tmp0_ta, tmp0_tb)

    printf("[iter %d]\n", i)
    printf("theta=%d, gamma=%d\n", tmp_ob(3), o_a(4))

    o_a(1) = tmp_ob(0)
    o_a(2) = tmp_ob(1)
    o_a(3) = tmp_ob(2)
    o_a(4) = tmp_ob(3)

    t_a(1) = tmp_tb(0)
    t_a(2) = tmp_tb(1)
    t_a(3) = tmp_tb(2)
    t_a(4) = tmp_tb(3)

    printf("-->o_a(0)=%d, o_a(1)=%d, o_a(2)=%d, o_a(3)=%d, o_a(4)=%d\n",
      o_a(0), o_a(1), o_a(2), o_a(3), o_a(4));
    printf("-->o_b(0)=%d, o_b(1)=%d, o_b(2)=%d, o_b(3)=%d, o_b(4)=%d\n",
      o_b(0), o_b(1), o_b(2), o_b(3), o_b(4));
    printf("-->t_a(0)=%d, t_a(1)=%d, t_a(2)=%d, t_a(3)=%d, t_a(4)=%d\n",
      t_a(0), t_a(1), t_a(2), t_a(3), t_a(4));
    printf("-->t_b(0)=%d, t_b(1)=%d, t_b(2)=%d, t_b(3)=%d, t_b(4)=%d\n",
      t_b(0), t_b(1), t_b(2), t_b(3), t_b(4));


  }
  }

  val numMsgs = msgs.size
  val numPars = numSyms - numMsgs

  // Generator Polynomial
  // g(X) = (X + a^1)(X + a^2)(X + a^3) ... (X + a^numPars)
  //      = gCoeffs(0) + gCoeffs(1) * X^1 + gCoeffs(2) * X^2 + ... + gCoeffs(numPars) * X^numPars
  val gCoeffs = {
    var coeffs = Seq[Int]()
    val powSets = (0 to numPars - 1).toSet[Int].subsets.map(_.toList).toList
    for (j <- numPars to 1 by -1) {
      var tmpSum: Int = 0
      for (i <- 0 until powSets.size) {
        if (powSets(i).size == j) {
          var tmpMul: Int = 1
          for (k <- 0 until j) {
            tmpMul = mul(tmpMul, Log2Val(powSets(i)(k)))
          }
          tmpSum = add(tmpSum, tmpMul)
        }
      }
      coeffs = coeffs :+ tmpSum
    }
    coeffs
  }

  def encode(): Seq[Int] = {
    var pars: Array[Int] = new Array[Int](numPars)
    for (i <- 0 until numMsgs) {
      val tmp = pars.map(x => x)
      val feedback = add(msgs(i), pars(numPars - 1))

      for (j <- 0 until numPars) {
        if (j == 0) {
          pars(j) = mul(feedback, gCoeffs(j))
        } else {
          pars(j) = add(mul(feedback, gCoeffs(j)), tmp(j - 1))
        }
      }
    }
    // Append input messages with the generated parity symbols
    msgs ++ pars.reverse
  }


  // Make sure the generated symbol sequence forms a polynomial
  // that equals to zero at all the *numPars* roots
  // E.g., out(X) = syms(0) * X^(n - 1) + sym(1) * X^(n - 2) + ... + sym(n - 1) * X^0
  // then we need to ensure that: out(a^1) = 0, out(a^2) = 0, ... out(a^(numPars)) = 0
  def verify_encode(syms: Seq[Int]): Boolean = {
    var check: Boolean = true
    for (r <- 0 to numPars - 1) {
      var t: Int = 0
      for (i <- 0 until syms.size) {
        t = add(t, mul(syms(i), pow(Log2Val(r), syms.size - i - 1)))
      }
      if (t != 0) {
        check = false
      }
    }
    check
  }

}

// This only tests the Reed-Solomon encoder
class RSEncoderUnitTester(c: RSEncoder, swSyms: Seq[Int]) extends PeekPokeTester(c) {
  var hwSyms = List[Int]()

  poke(c.io.in.valid, true)
  poke(c.io.out.ready, true)

  for (i <- 0 until c.p.k) {
    poke(c.io.in.bits, swSyms(i))
    // This is rather awkward
    if (peek(c.io.out.valid) == BigInt(1) &&
        peek(c.io.out.ready) == BigInt(1)) {
      hwSyms = hwSyms :+ peek(c.io.out.bits).toInt
    }
    step(1)
  }

  poke(c.io.in.valid, false)

  while (peek(c.io.out.valid) == BigInt(1) &&
         peek(c.io.out.ready) == BigInt(1)) {
    hwSyms = hwSyms :+ peek(c.io.out.bits).toInt
    step(1)
  }

  for (i <- 0 until c.p.n) {
    printf("hwSyms(%d) = %d\n", i, hwSyms(i))
  }

  for (i <- 0 until c.p.n) {
    expect(hwSyms(i) == swSyms(i), "symbols do not match!")
  }
}

class PolyComputeUnitTester(c: PolyCompute, swSyms: Seq[Int]) extends PeekPokeTester(c) {
  poke(c.io.in.valid, true)
  poke(c.io.out.ready, true)

  val maxCycles = 30

  var numCycles = 0
  var outCnt = 0
  var inCnt = 0

  while (numCycles < maxCycles && outCnt < c.numCells) {
    numCycles += 1
    if (numCycles >= maxCycles) {
      expect(false, "timeout!")
    }

    if (inCnt == c.numInputs) {
      poke(c.io.in.valid, false)
    }

    if (peek(c.io.in.valid) == BigInt(1) &&
        peek(c.io.in.ready) == BigInt(1) && inCnt < c.numInputs) {
      poke(c.io.in.bits, swSyms(inCnt))
      inCnt += 1
    }

    if (peek(c.io.out.valid) == BigInt(1) &&
        peek(c.io.out.ready) == BigInt(1)) {
      expect(c.io.out.bits, 0)
      outCnt += 1
    }

    step(1)
  }
}

class GFInversionUnitTester(c: GFInversion) extends PeekPokeTester(c) {
  poke(c.io.in, 10)
  step(10)
}

class ErrorPolyGenUnitTester(c: ErrorPolyGen, inSyms: Seq[Int]) extends PeekPokeTester(c) {
  poke(c.io.in.valid, true)
  poke(c.io.out.ready, true)

  val maxCycles = 60

  var numCycles = 0
  var outCnt = 0
  var inCnt = 0

  while (numCycles < maxCycles) {
    numCycles += 1
    if (numCycles >= maxCycles) {
      expect(false, "timeout!")
    }

    if (inCnt == c.p.n) {
      poke(c.io.in.valid, false)
    }

    if (peek(c.io.in.valid) == BigInt(1) &&
        peek(c.io.in.ready) == BigInt(1) && inCnt < c.p.n) {
      poke(c.io.in.bits, inSyms(inCnt))
      inCnt += 1
    }

//    if (peek(c.io.out.valid) == BigInt(1) &&
//        peek(c.io.out.ready) == BigInt(1)) {
//      expect(c.io.out.bits, 0)
//      outCnt += 1
//    }

    step(1)
  }
}

// This will test the whole ECC block with the CREECBus
// At the moment, the ECC only handles encoding
// This test assumes that the upstream block sends a
// write request to the ECC block. The ECC block generates
// the parity symbols and then sends a write request to
// the downstream block.
// Apparently, RS(16, 8) of 8-bit symbols is chosen. It means
// the data bus of 64-bit will be decomposed into eight 8-bit symbols
// We need to generate 8 parity symbols, which is another 64-bit data
// The flow goes like this:
// Upstream --> 64-bit slave wrData --> ECC Computation --> 128-bit master wrData --> Downstream
// In this case, two bus transactions will be needed to send the data (of 64-bit each) to the downstream block
// I have no idea how good this scheme is, or is it even practical.
// TODO:
//   + Test the decoder
//   + Test the bus interface rigorously
//class ECCEncoderUnitTester(c: ECC, swSyms: Seq[Int]) extends PeekPokeTester(c) {
//  var hwSyms = List[Int]()
//
//  poke(c.io.slave.wrReq.valid, true)
//  poke(c.io.slave.wrData.valid, true)
//  poke(c.io.master.wrReq.ready, true)
//  poke(c.io.master.wrData.ready, true)
//
//  var inputBits: BigInt = 0
//  for (i <- 0 until c.rsParams.k) {
//    inputBits = (inputBits << c.rsParams.symbolWidth) + swSyms(c.rsParams.k - i - 1)
//  }
//  poke(c.io.slave.wrData.bits.data, inputBits)
//
//  var numCycles = 0
//  val maxCycles = 30
//  var tmpList = List[Int]()
//
//  // Wait until getting enough data or timeout
//  while (numCycles < maxCycles && tmpList.size < swSyms.size) {
//    numCycles += 1
//    if (numCycles >= maxCycles) {
//      expect(false, "timeout!")
//    }
//
//    if (peek(c.io.master.wrReq.valid) == BigInt(1) &&
//        peek(c.io.master.wrReq.ready) == BigInt(1) &&
//        peek(c.io.master.wrData.valid) == BigInt(1) &&
//        peek(c.io.master.wrData.valid) == BigInt(1)) {
//      var result: BigInt = peek(c.io.master.wrData.bits.data)
//      var mask = BigInt(2).pow(c.rsParams.symbolWidth) - 1
//      for (i <- 0 until c.busParams.dataWidth / c.rsParams.symbolWidth) {
//        tmpList = tmpList :+ (result & mask).toInt
//        result = result >> c.rsParams.symbolWidth
//      }
//    }
//    step(1)
//  }
//  // Be careful of the order of the bytes
//  hwSyms = hwSyms ++ tmpList.reverse
//
//  for (i <- 0 until c.rsParams.n) {
//    printf("hwSyms(%d) = %d\n", i, hwSyms(i))
//  }
//
//  for (i <- 0 until c.rsParams.n) {
//    expect(hwSyms(i) == swSyms(i), "symbols do not match!")
//  }
//}

/**
  * From within sbt use:
  * testOnly ecc.ECCTester
  * From a terminal shell use:
  * sbt 'testOnly ecc.ECCTester'
  */
class ECCTester extends ChiselFlatSpec {
  // RS(16, 8)
  val numSymbols = 15
  val numMsgs = 11
  val symbolWidth = 4
//  var msgs = Seq.fill(numMsgs) {
//    scala.util.Random.nextInt(BigInt(2).pow(symbolWidth).toInt - 1)
//  }

  val msgs = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

  val rs = new RSCode(numSymbols, symbolWidth, msgs)

  rs.test_t()
  //rs.test()
  printf("INV: %d\n", rs.inv(10))

  val gCoeffs = rs.gCoeffs
  val fConst = rs.fConst

  val swSyms = rs.encode()
  for (i <- 0 until swSyms.size) {
    printf("swSyms(%d) = %d\n", i, swSyms(i))
  }

  // Need to pass this test to go further
  require(rs.verify_encode(swSyms), "Incorrect software RSEncoding generator!")

  val params = RSParams(
    n = numSymbols,
    k = msgs.size,
    symbolWidth = symbolWidth,
    gCoeffs = gCoeffs,
    fConst = fConst,
    rs.Log2Val,
    rs.Val2Log
  )

  "RSEncoder" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new RSEncoder(params)) {
      c => new RSEncoderUnitTester(c, swSyms)
    } should be(true)
  }

  "PolyCompute" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new PolyCompute(symbolWidth, numSymbols - msgs.size,
                    numSymbols, rs.Log2Val, fConst)) {
      c => new PolyComputeUnitTester(c, swSyms)
    } should be(true)
  }

//  "ECC" should "work" in {
//    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
//    new ECC(params)) {
//      c => new ECCUnitTester(c, swSyms)
//    } should be(true)
//  }

//  "GFInversion" should "work" in {
//    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
//    new GFInversion(params)) {
//      c => new GFInversionUnitTester(c)
//    } should be(true)
//  }

  val inSyms = Seq(1, 2, 3, 4, 5, 11, 7, 8, 9, 10, 11, 3, 1, 12, 12)

  "ErrorPolyGen" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new ErrorPolyGen(params)) {
      c => new ErrorPolyGenUnitTester(c, inSyms)
    } should be(true)
  }

}
