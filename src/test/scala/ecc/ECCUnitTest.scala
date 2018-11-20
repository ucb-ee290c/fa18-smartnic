package ecc
import scala.math

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

// This only tests the Reed-Solomon encoder
class RSEncoderUnitTester(c: RSEncoder,
  trials: List[(Seq[Int], Array[Int], Seq[Int])],
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

    if (verbose) {
      for (i <- 0 until c.p.n) {
        printf("swSyms(%d) = %d, hwSyms(%d) = %d\n", i, swSyms(i), i, hwSyms(i))
      }
    }

    expect(hwSyms == swSyms, "symbols do not match!")
  }
}

class RSDecoderUnitTester(c: RSDecoder,
  trials: List[(Seq[Int], Array[Int], Seq[Int])],
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

    if (verbose) {
      for (i <- 0 until c.p.k) {
        printf("inSyms(%d) = %d swCorrectedSyms(%d) = %d hwCorrectedSyms(%d) = %d\n",
          i, inSyms(i), i, swCorrectedSyms(i), i, hwCorrectedSyms(i))
      }
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
  trials: List[(Seq[Int], Array[Int], Seq[Int])],
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
  trials: List[(Seq[Int], Array[Int], Seq[Int])],
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
    val numBeats = c.rsParams.n * c.rsParams.symbolWidth / c.busParams.dataWidth
    poke(c.io.slave.header.bits.len, numBeats)

    val r = c.rsParams.n / numBeats
    var beatCnt = 1

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
        beatCnt = beatCnt - 1
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

  "ECCEncoderTop" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new ECCEncoderTop(rsParams)) {
      c => new ECCEncoderTopUnitTester(c, trials,verbose)
    } should be(true)
  }

  "ECCDecoderTop" should "work" in {
    iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () =>
    new ECCDecoderTop(rsParams)) {
      c => new ECCDecoderTopUnitTester(c, trials, verbose)
    } should be(true)
  }
}
