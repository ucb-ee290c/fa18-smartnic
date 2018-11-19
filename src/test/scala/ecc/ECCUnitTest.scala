package ecc
import scala.math

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
//import interconnect._

// Software implementation of the Reed-Solomon encoder & decoder
class RSCode(numSyms: Int, numMsgs: Int, symbolWidth: Int) {
  val numRoots = BigInt(2).pow(symbolWidth).toInt
  val numPars = numSyms - numMsgs
  var Log2Val: Array[Int] = new Array[Int](numRoots)
  var Val2Log: Array[Int] = new Array[Int](numRoots)
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
      // x^10 + x^3 + 1
      case 10 => Integer.parseInt("10000001001", 2)
      // x^16 + x^12 + x^3 + x + 1
      case 16 => Integer.parseInt("10001000000001011", 2)
      case _ => 0
    }
  }

  require(fConst != 0, "Unsupported symbol width!")

  Log2Val(0) = 1
  Log2Val(1) = 2 // according to the spec, usually choose a^1 to be 2
  for (i <- 2 until numRoots) {
    Log2Val(i) = Log2Val(i - 1) << 1
    if (Log2Val(i) >= numRoots) {
      Log2Val(i) = (Log2Val(i) % numRoots) ^ (fConst % numRoots)
    }
  }

  for (i <- 0 until numRoots) {
    Val2Log(Log2Val(i)) = i
  }

  def add(a: Int, b: Int): Int = {
    a ^ b
  }

  def mul(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      0
    } else {
      Log2Val((Val2Log(a) + Val2Log(b)) % (numRoots - 1))
    }
  }

  def pow(a: Int, n: Int): Int = {
    (0 until n).foldLeft(1) { (prev, next) => mul(prev, a) }
  }

  def printValRootTable() {
    for (i <- 0 until numRoots) {
      printf("Val2Log(%d) = %d\n", i, Val2Log(i))
    }
  }

  def printLogRootTable() {
    for (i <- 0 until numRoots) {
      printf("Log2Val(%d) = %d\n", i, Log2Val(i))
    }
  }

  def inv(a: Int): Int = {
    //pow(a, numRoots - 2)
    var tmp = a
    var result = 1
    while (tmp != 1) {
      tmp = tmp << 1
      if (tmp >= numRoots) {
        tmp = (tmp % numRoots) ^ (fConst % numRoots)
      }
      result = result << 1
      if (result >= numRoots) {
        result = (result % numRoots) ^ (fConst % numRoots)
      }

    }
    result
  }

  // Generator Polynomial
  // g(X) = (X + a^1)(X + a^2)(X + a^3) ... (X + a^numPars)
  //      = gCoeffs(0) + gCoeffs(1) * X^1 + gCoeffs(2) * X^2 + ... + gCoeffs(numPars) * X^numPars
  val gCoeffs = {
    val powSets = (0 to numPars - 1).toSet[Int].subsets.map(_.toList).toList
    var coeffs = new Array[Int](numPars)

    for (i <- 0 until powSets.size) {
      val coeffIdx = numPars - powSets(i).size
      if (coeffIdx < numPars) {
        val powSum = powSets(i).reduce(_ + _) % (numRoots - 1)
        coeffs(coeffIdx) = add(coeffs(coeffIdx), Log2Val(powSum))
      }
    }
    coeffs
  }

  def encode(msgs: Seq[Int]): Seq[Int] = {
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

  // This function computes the following polynomial
  // coeffs(0) * X^(0) + coeffs(1) * X^(1) + ... + coeffs(d - 1) * X^(d - 1)
  // where X = variable, d = coeffs.size
  def evaluatePoly(coeffs: Seq[Int], variable: Int): Int = {
    val degree = coeffs.size
    var result = 0
    for (i <- 0 until degree) {
      result = result ^ mul(coeffs(i), pow(variable, i))
    }
    result
  }

  // A correct symbol sequence forms a polynomial that has roots
  // at a^0, a^1, ..., a^(numPars - 1)
  def syndromeCompute(syms: Seq[Int]): (Seq[Int], Boolean) = {
    var syndromes = List[Int]()
    val revSyms = syms.reverse
    var foundNZSym = false
    // Syndrome computation
    for (i <- 0 until numPars) {
      val res = evaluatePoly(revSyms, Log2Val(i))
      syndromes = syndromes :+ res
      if (res != 0) {
        foundNZSym = true
      }
    }
    (syndromes, foundNZSym)
  }

  def verifySyms(syms: Seq[Int]): Boolean = {
    syndromeCompute(syms)._2 == false
  }

  def findDeg(coeffs: Seq[Int]): Int = {
    var degVal = 0
    for (i <- 0 until coeffs.size) {
      if (coeffs(i) != 0) {
        degVal = i
      }
    }
    degVal
  }

  def decode(inSyms: Seq[Int]): Seq[Int] = {
    val (syndromes, foundNZSym) = syndromeCompute(inSyms)
    if (!foundNZSym) {
      // the input symbol sequence is bug-free
      inSyms
    } else {
      val size = numPars + 1
      var evaluatorsA = new Array[Int](size)
      var evaluatorsB = new Array[Int](size)
      var locatorsA = new Array[Int](size)
      var locatorsB = new Array[Int](size)

      evaluatorsA(numPars) = 1

      for (i <- 0 until numPars) {
        evaluatorsB(i) = syndromes(i)
      }
      locatorsB(0) = 1

      var degA = findDeg(evaluatorsA)
      var degB = findDeg(evaluatorsB)
      var numIters = 0

      for (i <- 0 until size) {
        printf("evalB(%d) = %d\n", i, evaluatorsB(i))
      }
      printf("[initial] degA = %d, degB = %d\n", degA, degB)

      while (degA >= numPars / 2) {
        numIters = numIters + 1

        // Let's swap!
        if (degA < degB && evaluatorsB(size - 1) != 0 &&
                           evaluatorsA(size - 1) != 0) {
          val tmpEvalA = evaluatorsA.map(x => x)
          val tmpLocA = locatorsA.map(x => x)
          for (i <- 0 until size) {
            evaluatorsA(i) = evaluatorsB(i)
            locatorsA(i) = locatorsB(i)
          }
          for (i <- 0 until size) {
            evaluatorsB(i) = tmpEvalA(i)
            locatorsB(i) = tmpLocA(i)
          }
          val tmpdeg = degA
          degA = degB
          degB = tmpdeg
        }

        val theta = evaluatorsB(size - 1)
        val gamma = evaluatorsA(size - 1)

        if (theta != 0 && gamma != 0) {
          for (i <- 0 until size) {
            evaluatorsA(i) = mul(theta, evaluatorsA(i)) ^
                             mul(gamma, evaluatorsB(i))
            locatorsA(i) = mul(theta, locatorsA(i)) ^
                             mul(gamma, locatorsB(i))

          }
        }

        if (theta == 0) {
          val tmpEvalB = evaluatorsB.map(x => x)
          evaluatorsB(0) = tmpEvalB(size - 1)
          for (i <- 0 until size - 1) {
            evaluatorsB(i + 1) = tmpEvalB(i)
          }

          val tmpLocB = locatorsB.map(x => x)
          locatorsB(0) = tmpLocB(size - 1)
          for (i <- 0 until size - 1) {
            locatorsB(i + 1) = tmpLocB(i)
          }

        }

        if (gamma == 0) {
          degA = degA - 1
          // Don't shift at the last iteration
          if (degA >= numPars / 2) {
            val tmpEvalA = evaluatorsA.map(x => x)
            evaluatorsA(0) = tmpEvalA(size - 1)
            for (i <- 0 until size - 1) {
              evaluatorsA(i + 1) = tmpEvalA(i)
            }

            val tmpLocA = locatorsA.map(x => x)
            locatorsA(0) = tmpLocA(size - 1)
            for (i <- 0 until size - 1) {
              locatorsA(i + 1) = tmpLocA(i)
            }
          }
        }
      }

      printf("Check Key Equation Solver result: done after %d iters\n", numIters)
      for (i <- 0 until size) {
        printf("[%d] eval=%d, loc=%d\n", i, evaluatorsA(i), locatorsA(i))
      }

      val locatorsDeriv = new Array[Int](numPars)
      for (i <- 0 until numPars) {
        if (i % 2 == 1) {
          locatorsDeriv(i) = 0
        }
        else {
          locatorsDeriv(i) = locatorsA(i + 1)
        }
      }

      // Chien search
      var chienRootIndices = List[Int]()
      for (i <- numSyms - 1 to 0 by - 1) {
        val res = evaluatePoly(locatorsA, Log2Val(numRoots - 1 - i))
        if (res == 0) {
          chienRootIndices = chienRootIndices :+ (numSyms - i)
          printf("found Chien root index: %d\n", numSyms - i)
        }

      }

      // Error correction
      var correctedSyms = new Array[Int](numSyms)
      for (i <- 0 until numSyms) {
        correctedSyms(i) = inSyms(i)
      }

      for (i <- 0 until chienRootIndices.size) {
        val idx = chienRootIndices(i)
        val chienRootVal = Log2Val(numRoots - 1 - (numSyms - idx))
        correctedSyms(idx - 1) = correctedSyms(idx - 1) ^
          mul(evaluatePoly(evaluatorsA, chienRootVal),
              inv(mul(chienRootVal, evaluatePoly(locatorsDeriv, chienRootVal))))
      }

      correctedSyms
    }
  }

}

// This only tests the Reed-Solomon encoder
class RSEncoderUnitTester(c: RSEncoder,
  trials: List[(Seq[Int], Array[Int], Seq[Int])]) extends PeekPokeTester(c) {

  for (i <- 0 until trials.size) {
    printf("===TRIAL %d\n", i)

    val swSyms = trials(i)._1

    var hwSyms = List[Int]()

    poke(c.io.in.valid, true)
    poke(c.io.out.ready, true)

    val maxCycles = 300

    var numCycles = 0
    var outCnt = 0
    var inCnt = 0

    while (numCycles < maxCycles && outCnt < c.p.n) {
      numCycles += 1
      if (numCycles >= maxCycles) {
        expect(false, "timeout!")
      }

      if (inCnt == c.p.k) {
        poke(c.io.in.valid, false)
      }

      if (peek(c.io.in.valid) == BigInt(1) &&
          peek(c.io.in.ready) == BigInt(1) && inCnt < c.p.k) {
        poke(c.io.in.bits, swSyms(inCnt))
        inCnt += 1
      }

      if (peek(c.io.out.valid) == BigInt(1) &&
          peek(c.io.out.ready) == BigInt(1)) {
        hwSyms = hwSyms :+ peek(c.io.out.bits).toInt
        outCnt += 1
      }

      step(1)
    }

    for (i <- 0 until c.p.n) {
      printf("swSyms(%d) = %d, hwSyms(%d) = %d\n", i, swSyms(i), i, hwSyms(i))
    }

    expect(hwSyms == swSyms, "symbols do not match!")
  }
}

class RSDecoderUnitTester(c: RSDecoder,
  trials: List[(Seq[Int], Array[Int], Seq[Int])]) extends PeekPokeTester(c) {

  for (i <- 0 until trials.size) {
    printf("===TRIAL %d\n", i)

    val inSyms = trials(i)._2
    val swCorrectedSyms = trials(i)._3

    var hwCorrectedSyms = List[Int]()

    poke(c.io.in.valid, true)
    poke(c.io.out.ready, true)

    val maxCycles = 3000

    var numCycles = 0
    var outCnt = 0
    var inCnt = 0

    while (numCycles < maxCycles && outCnt < c.p.k) {
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

      if (peek(c.io.out.valid) == BigInt(1) &&
          peek(c.io.out.ready) == BigInt(1)) {
        hwCorrectedSyms = hwCorrectedSyms :+ peek(c.io.out.bits).toInt
        outCnt += 1
      }

      step(1)
    }

    for (i <- 0 until c.p.k) {
      printf("inSyms(%d) = %d swCorrectedSyms(%d) = %d hwCorrectedSyms(%d) = %d\n",
        i, inSyms(i), i, swCorrectedSyms(i), i, hwCorrectedSyms(i))
    }

    expect(hwCorrectedSyms == swCorrectedSyms.slice(0, c.p.k),
           "symbols do not match!")
  }
}

// This will test the RSEncoder block with the CREECBus
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
class ECCEncoderTopUnitTester(c: ECCEncoderTop,
  trials: List[(Seq[Int], Array[Int], Seq[Int])]) extends PeekPokeTester(c) {

  for (i <- 0 until trials.size) {
    printf("===TRIAL %d\n", i)

    val swSyms = trials(i)._1

    var hwSyms = List[Int]()

    poke(c.io.slave.header.valid, true)
    poke(c.io.slave.data.valid, true)
    poke(c.io.master.header.ready, true)
    poke(c.io.master.data.ready, true)

    // TODO: test with multiple data beats
    poke(c.io.slave.header.bits.len, 1)

    // Pack all input symbols into a single data item
    // that has width == bus data width
    var inputBits: BigInt = 0
    for (i <- 0 until c.rsParams.k) {
      inputBits = (inputBits << c.rsParams.symbolWidth) +
                  swSyms(c.rsParams.k - i - 1)
    }
    poke(c.io.slave.data.bits.data, inputBits)

    var numCycles = 0
    val maxCycles = 300
    var outputs = List[Int]()

    // Wait until getting enough data or timeout
    while (numCycles < maxCycles && outputs.size < swSyms.size) {
      numCycles += 1
      if (numCycles >= maxCycles) {
        expect(false, "timeout!")
      }

      if (peek(c.io.master.data.valid) == BigInt(1) &&
          peek(c.io.master.data.ready) == BigInt(1)) {
        var result: BigInt = peek(c.io.master.data.bits.data)
        var mask = BigInt(2).pow(c.rsParams.symbolWidth) - 1
        for (i <- 0 until c.busParams.dataWidth / c.rsParams.symbolWidth) {
          outputs = outputs :+ (result & mask).toInt
          result = result >> c.rsParams.symbolWidth
        }
      }

      step(1)
    }

    // Be careful of the order of the bytes
    hwSyms = hwSyms ++ outputs.reverse

    for (i <- 0 until c.rsParams.n) {
      printf("swSyms(%d) = %d, hwSyms(%d) = %d\n", i, swSyms(i), i, hwSyms(i))
    }

    expect(hwSyms == swSyms, "symbols do not match!")
  }
}

class ECCDecoderTopUnitTester(c: ECCDecoderTop,
  trials: List[(Seq[Int], Array[Int], Seq[Int])]) extends PeekPokeTester(c) {

  for (i <- 0 until trials.size) {
    printf("===TRIAL %d\n", i)

    val inSyms = trials(i)._2
    val swCorrectedSyms = trials(i)._3

    var hwCorrectedSyms = List[Int]()

    poke(c.io.slave.header.valid, true)
    poke(c.io.slave.data.valid, true)
    poke(c.io.master.header.ready, true)
    poke(c.io.master.data.ready, true)

    // TODO: test with multiple data beats
    val numBeats = c.rsParams.n * c.rsParams.symbolWidth / c.busParams.dataWidth
    poke(c.io.slave.header.bits.len, numBeats)

    val r = c.rsParams.n / numBeats
    var beatCnt = 0

    var numCycles = 0
    val maxCycles = 300
    var outputs = List[Int]()

    // Wait until getting enough data or timeout
    while (numCycles < maxCycles && outputs.size < c.rsParams.k) {
      numCycles += 1
      if (numCycles >= maxCycles) {
        expect(false, "timeout!")
      }

      if (peek(c.io.slave.data.valid) == BigInt(1) &&
          peek(c.io.slave.data.ready) == BigInt(1)) {
        var inputBits: BigInt = 0
        for (j <- beatCnt * r until (beatCnt + 1) * r) {
          // Pack r input symbols into a single data item
          // that has width == bus data width
          inputBits = (inputBits << c.rsParams.symbolWidth) +
                       inSyms(c.rsParams.n - j - 1)
        }
        poke(c.io.slave.data.bits.data, inputBits)
        beatCnt = beatCnt + 1
      }

      if (peek(c.io.master.data.valid) == BigInt(1) &&
          peek(c.io.master.data.ready) == BigInt(1)) {
        var result: BigInt = peek(c.io.master.data.bits.data)
        var mask = BigInt(2).pow(c.rsParams.symbolWidth) - 1
        for (i <- 0 until c.busParams.dataWidth / c.rsParams.symbolWidth) {
          outputs = outputs :+ (result & mask).toInt
          result = result >> c.rsParams.symbolWidth
        }
      }

      step(1)
    }

    // Be careful of the order of the bytes
    hwCorrectedSyms = hwCorrectedSyms ++ outputs.reverse

    for (i <- 0 until c.rsParams.k) {
      printf("inSyms(%d) = %d swCorrectedSyms(%d) = %d hwCorrectedSyms(%d) = %d\n",
        i, inSyms(i), i, swCorrectedSyms(i), i, hwCorrectedSyms(i))
    }

    expect(hwCorrectedSyms == swCorrectedSyms.slice(0, c.rsParams.k),
           "symbols do not match!")
  }
}

/**
  * From within sbt use:
  * testOnly ecc.ECCTester
  * From a terminal shell use:
  * sbt 'testOnly ecc.ECCTester'
  */
class ECCTester extends ChiselFlatSpec {
  // RS(16, 8)
  val numSymbols = 16
  val numMsgs = 8
  val symbolWidth = 8
  val rs = new RSCode(numSymbols, numMsgs, symbolWidth)

  val numTrials = 100
  var trials: List[(Seq[Int], Array[Int], Seq[Int])] = List()

  // Generate test code sequences
  for (t <- 0 until numTrials) {
    printf("===TRIAL %d\n", t)

    var msgs = Seq.fill(numMsgs) {
      scala.util.Random.nextInt(rs.numRoots - 1)
    }

    // Running software RS Encoder
    val swSyms = rs.encode(msgs)
    for (i <- 0 until swSyms.size) {
      printf("swSyms(%d) = %d\n", i, swSyms(i))
    }

    // Need to pass this test to go further
    require(rs.verifySyms(swSyms), "Incorrect software RS encoder!")

    var buggySyms = new Array[Int](numSymbols)
    for (i <- 0 until numSymbols) {
      buggySyms(i) = swSyms(i)
    }

    // Randomly pick locations for introducing error symbols
    // It's okay if we pick the same location multiple times
    // as long as the number of errorneous locations does not
    // exceed *maxNumErrorSyms*
    val maxNumErrorSyms = (numSymbols - numMsgs) / 2
    for (i <- 0 until maxNumErrorSyms) {
      val errorIdx = scala.util.Random.nextInt(numSymbols - 1)
      buggySyms(errorIdx) = scala.util.Random.nextInt(rs.numRoots - 1)
    }

    for (i <- 0 until numSymbols) {
      printf("buggySyms(%d) = %d\n", i, buggySyms(i))
    }

    // Running software RS Decoder
    val swCorrectedSyms = rs.decode(buggySyms)
    for (i <- 0 until swCorrectedSyms.size) {
      printf("swCorrectedSyms(%d) = %d\n", i, swCorrectedSyms(i))
    }

    // Need to pass this test to go further
    require(rs.verifySyms(swCorrectedSyms), "Incorrect software RS decoder!")

    val item = (swSyms, buggySyms, swCorrectedSyms)
    trials = trials :+ item
  }

  val params = RSParams(
    n = numSymbols,
    k = numMsgs,
    symbolWidth = symbolWidth,
    gCoeffs = rs.gCoeffs,
    fConst = rs.fConst,
    rs.Log2Val,
    rs.Val2Log
  )

  "RSEncoder" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new RSEncoder(params)) {
      c => new RSEncoderUnitTester(c, trials)
    } should be(true)
  }

  "RSDecoder" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new RSDecoder(params)) {
      c => new RSDecoderUnitTester(c, trials)
    } should be(true)
  }

  "ECCEncoderTop" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new ECCEncoderTop(params)) {
      c => new ECCEncoderTopUnitTester(c, trials)
    } should be(true)
  }

  "ECCDecoderTop" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new ECCDecoderTop(params)) {
      c => new ECCDecoderTopUnitTester(c, trials)
    } should be(true)
  }
}
