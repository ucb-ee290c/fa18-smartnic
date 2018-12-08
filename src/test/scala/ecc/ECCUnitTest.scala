package ecc

import interconnect._
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester

// These PeekPoke unit testers are for simple functional verification
// For more advanced tests, check out the CREECBusECC test

class RSEncoderUnitTester(c: RSEncoder,
  trials: List[(Seq[Int], Seq[Int], Seq[Int])],
  verbose: Boolean = false) extends PeekPokeTester(c) {

  for (i <- 0 until trials.size) {
    if (verbose) {
      printf("===TRIAL %d\n", i)
    }

    val swSyms = trials(i)._1

    var hwSyms = List[Int]()

    poke(c.io.in.valid, true)
    poke(c.io.out.ready, true)

    val maxCycles = 300

    var numCycles = 0
    var outCnt = 0
    var inCnt = 0

    while (numCycles < maxCycles && outCnt < c.rsParams.n) {
      if (inCnt == c.rsParams.k) {
        poke(c.io.in.valid, false)
      }

      if (peek(c.io.in.valid) == BigInt(1) &&
          peek(c.io.in.ready) == BigInt(1) && inCnt < c.rsParams.k) {
        poke(c.io.in.bits, swSyms(inCnt))
        inCnt += 1
      }

      if (peek(c.io.out.valid) == BigInt(1) &&
          peek(c.io.out.ready) == BigInt(1)) {
        hwSyms = hwSyms :+ peek(c.io.out.bits).toInt
        outCnt += 1
      }

      numCycles += 1
      step(1)
    }

    if (numCycles >= maxCycles) {
      expect(false, "timeout!")
    }

    if (verbose) {
      for (i <- 0 until c.rsParams.n) {
        printf("swSyms(%d) = %d, hwSyms(%d) = %d\n", i, swSyms(i), i, hwSyms(i))
      }
    }

    expect(hwSyms == swSyms, "symbols do not match!")
  }
}

class RSDecoderUnitTester(c: RSDecoder,
  trials: List[(Seq[Int], Seq[Int], Seq[Int])],
  verbose: Boolean = false) extends PeekPokeTester(c) {

  for (i <- 0 until trials.size) {
    if (verbose) {
      printf("===TRIAL %d\n", i)
    }

    val inSyms = trials(i)._2
    val swCorrectedSyms = trials(i)._3

    var hwCorrectedSyms = List[Int]()

    poke(c.io.in.valid, true)
    poke(c.io.out.ready, true)

    val maxCycles = 3000

    var numCycles = 0
    var outCnt = 0
    var inCnt = 0

    while (numCycles < maxCycles && outCnt < c.rsParams.k) {
      if (inCnt == c.rsParams.n) {
        poke(c.io.in.valid, false)
      }

      if (peek(c.io.in.valid) == BigInt(1) &&
          peek(c.io.in.ready) == BigInt(1) && inCnt < c.rsParams.n) {
        poke(c.io.in.bits, inSyms(inCnt))
        inCnt += 1
      }

      if (peek(c.io.out.valid) == BigInt(1) &&
          peek(c.io.out.ready) == BigInt(1)) {
        hwCorrectedSyms = hwCorrectedSyms :+ peek(c.io.out.bits).toInt
        outCnt += 1
      }

      numCycles += 1
      step(1)
    }

    if (numCycles >= maxCycles) {
      expect(false, "timeout!")
    }

    if (verbose) {
      for (i <- 0 until c.rsParams.k) {
        printf("inSyms(%d) = %d swCorrectedSyms(%d) = %d hwCorrectedSyms(%d) = %d\n",
          i, inSyms(i), i, swCorrectedSyms(i), i, hwCorrectedSyms(i))
      }
    }

    expect(hwCorrectedSyms == swCorrectedSyms.slice(0, c.rsParams.k),
           "symbols do not match!")
  }
}

class ECCEncoderTopUnitTester(c: ECCEncoderTop,
  trials: List[(Seq[Int], Seq[Int], Seq[Int])],
  verbose: Boolean = false) extends PeekPokeTester(c) {

  for (i <- 0 until trials.size) {
    if (verbose) {
      printf("===TRIAL %d\n", i)
    }

    val swSyms = trials(i)._1

    var hwSyms = List[Int]()

    poke(c.io.slave.header.valid, true)
    poke(c.io.slave.data.valid, true)
    poke(c.io.master.header.ready, true)
    poke(c.io.master.data.ready, true)

    // TODO: test with multiple data beats
    // Note: multiple-beat test should be taken care of by the CREECBus
    // Transaction modeling test
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
      if (peek(c.io.master.data.valid) == BigInt(1) &&
          peek(c.io.master.data.ready) == BigInt(1)) {
        var result: BigInt = peek(c.io.master.data.bits.data)
        val mask = BigInt(2).pow(c.rsParams.symbolWidth) - 1
        for (i <- 0 until c.busOutParams.dataWidth / c.rsParams.symbolWidth) {
          outputs = outputs :+ (result & mask).toInt
          result = result >> c.rsParams.symbolWidth
        }
      }

      numCycles += 1
      step(1)
    }

    if (numCycles >= maxCycles) {
      expect(false, "timeout!")
    }

    hwSyms = hwSyms ++ outputs

    if (verbose) {
      for (i <- 0 until c.rsParams.n) {
        printf("swSyms(%d) = %d, hwSyms(%d) = %d\n", i, swSyms(i), i, hwSyms(i))
      }
    }

    expect(hwSyms == swSyms, "symbols do not match!")
  }
}

class ECCDecoderTopUnitTester(c: ECCDecoderTop,
  trials: List[(Seq[Int], Seq[Int], Seq[Int])],
  verbose: Boolean = false) extends PeekPokeTester(c) {

  for (i <- 0 until trials.size) {
    if (verbose) {
      printf("===TRIAL %d\n", i)
    }

    val inSyms = trials(i)._2
    val swCorrectedSyms = trials(i)._3

    var hwCorrectedSyms = List[Int]()

    poke(c.io.slave.header.valid, true)
    poke(c.io.slave.data.valid, true)
    poke(c.io.master.header.ready, true)
    poke(c.io.master.data.ready, true)

    // TODO: test with multiple data beats
    // Note: multiple-beat test should be taken care of by the CREECBus
    // Transaction modeling test

    // Pack all input symbols into a single data item
    // that has width == bus data width
    var inputBits: BigInt = 0
    for (i <- 0 until c.rsParams.n) {
      inputBits = (inputBits << c.rsParams.symbolWidth) +
                  inSyms(c.rsParams.n - i - 1)
    }
    poke(c.io.slave.data.bits.data, inputBits)

    var numCycles = 0
    val maxCycles = 300
    var outputs = List[Int]()

    // Wait until getting enough data or timeout
    while (numCycles < maxCycles && outputs.size < c.rsParams.k) {
      if (peek(c.io.master.data.valid) == BigInt(1) &&
          peek(c.io.master.data.ready) == BigInt(1)) {
        var result: BigInt = peek(c.io.master.data.bits.data)
        val mask = BigInt(2).pow(c.rsParams.symbolWidth) - 1
        for (i <- 0 until c.busOutParams.dataWidth / c.rsParams.symbolWidth) {
          outputs = outputs :+ (result & mask).toInt
          result = result >> c.rsParams.symbolWidth
        }
      }

      numCycles += 1
      step(1)
    }

    if (numCycles >= maxCycles) {
      expect(false, "timeout!")
    }

    hwCorrectedSyms = hwCorrectedSyms ++ outputs

    if (verbose) {
      for (i <- 0 until c.rsParams.k) {
        printf("inSyms(%d) = %d swCorrectedSyms(%d) = %d hwCorrectedSyms(%d) = %d\n",
          i, inSyms(i), i, swCorrectedSyms(i), i, hwCorrectedSyms(i))
      }
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
class ECCTester extends ECCSpec {
  val busParams = BusParams.creec
  val busECCParams = BusParams.ecc

  "RSEncoder" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new RSEncoder(rsParams)) {
      c => new RSEncoderUnitTester(c, trials, verbose)
    } should be(true)
  }

  "RSDecoder" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new RSDecoder(rsParams)) {
      c => new RSDecoderUnitTester(c, trials, verbose)
    } should be(true)
  }

  // Only uses these tests if RS configuration conforms nicely
  // with the bus data width
  if (busParams.dataWidth == (rsParams.symbolWidth * rsParams.k)) {
    "ECCEncoderTop" should "work" in {
      iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
      new ECCEncoderTop(rsParams, busParams, busECCParams)) {
        c => new ECCEncoderTopUnitTester(c, trials,verbose)
      } should be(true)
    }

    "ECCDecoderTop" should "work" in {
      iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
      new ECCDecoderTop(rsParams, busECCParams, busParams)) {
        c => new ECCDecoderTopUnitTester(c, trials, verbose)
      } should be(true)
    }
  }
}
