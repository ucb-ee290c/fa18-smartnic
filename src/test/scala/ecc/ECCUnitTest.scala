package ecc
import scala.math

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

// Software implementation of the RSEncoder
class RSCode(numSyms: Int, symbolWidth: Int,
             msgs: Seq[Int], gCoeffs: Seq[Int], fConst: Int) {
  val numVals = BigInt(2).pow(symbolWidth).toInt
  var Log2Val: Array[Int] = new Array[Int](numVals)
  var Val2Log: Array[Int] = new Array[Int](numVals)

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

  val numMsgs = msgs.size
  val numPars = numSyms - numMsgs

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

}

class RSEncoderUnitTester(c: RSEncoder, swSyms: Seq[Int]) extends PeekPokeTester(c) {
  var hwSyms = List[Int]()

  poke(c.io.in.valid, true)
  poke(c.io.out.ready, true)

  for (i <- 0 until c.param.k) {
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

  for (i <- 0 until c.param.n) {
    printf("hwSyms(%d) = %d\n", i, hwSyms(i))
  }

  for (i <- 0 until c.param.n) {
    expect(hwSyms(i) == swSyms(i), "symbols do not match!")
  }
}

/**
  * From within sbt use:
  * testOnly example.test.ECCTester
  * From a terminal shell use:
  * sbt 'testOnly example.test.ECCTester'
  */
class ECCTester extends ChiselFlatSpec {
  // RS(7, 3) --> 7 symbols in total: 3 message symbols, 4 parity symbols
  val numSymbols = 7
  val numMsgs = 3
  val symbolWidth = 3 
  var msgs = Seq.fill(numMsgs) {
    scala.util.Random.nextInt(BigInt(2).pow(symbolWidth).toInt - 1) }

  // gCoeffs and fConst need to be pre-computed (see the docs)
  // g(X) = (X + a^1)(X + a^2)(X + a^3)(X + a^4)
  // --> g(X) = 3 + 2 * X^1 + 1 * X^2 + 3 * X^3 + X^4
  val gCoeffs = Seq(3, 2, 1, 3)
  // f(X) = X^3 + X + 1 --> 1011 --> 11
  val fConst = 11
  val rs = new RSCode(numSymbols, symbolWidth, msgs, gCoeffs, fConst)
  val swSyms = rs.encode()
  for (i <- 0 until swSyms.size) {
    printf("swSyms(%d) = %d\n", i, swSyms(i))
  }

  val params = RSParams(
    n = numSymbols,
    k = msgs.size,
    symbolWidth = symbolWidth
  )

  "RSEncoder" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new RSEncoder(params)) {
      c => new RSEncoderUnitTester(c, swSyms)
    } should be(true)
  }
}
