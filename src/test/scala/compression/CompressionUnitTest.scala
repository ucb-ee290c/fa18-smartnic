package compression

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

/*
 * Golden model for basic compression.
 */
object CompressionUtils {
  private def differential(input: List[Byte], encode: Boolean): List[Byte] = {
    var output = List[Byte]()
    var prev = 0.toByte
    for (i <- input.indices) {
      output = output :+ (if (encode) (input(i) - prev).toByte else (input(i) + prev).toByte)
      prev = if (encode) input(i) else output(i)
    }
    output
  }

  def differentialEncode(input: List[Byte]): List[Byte] = {
    differential(input, encode = true)
  }

  def differentialDecode(input: List[Byte]): List[Byte] = {
    differential(input, encode = false)
  }

  /*TODO all these functions have the form {create output, use 1 state variable,
   *  loop through input, apply some function that assigns to the output from
   *  the input based on the state, and return the output}. Generalize this.
   */

  def runLengthEcode(input: List[Byte]): List[Byte] = {
    var output = List[Byte]()
    var run = 0
    for (i <- input.indices) {
      if (input(i) == 0) {
        if (run == 0) {
          output = output :+ 0.toByte
        }
        run += 1
      } else {
        if (run != 0) {
          output = output :+ (run - 1).toByte
        }
        output = output :+ input(i)
        run = 0
      }
    }
    if (run != 0)
      output = output :+ (run - 1).toByte
    output
  }

  def runLengthDecode(input: List[Byte]): List[Byte] = {
    var output = List[Byte]()
    var expand = false
    for (i <- input.indices) {
      if (expand) {
        output = output ++ List.fill(input(i) + 1)(0.toByte)
        expand = false
      } else if (input(i) == 0) {
        expand = true
      } else {
        output = output :+ input(i)
      }
    }
    output
  }
}

object ByteUtils {
  /*
   * Compacts 8 bytes into a 64-bit int.
   */
  def squish(bytes: List[Int]): BigInt = {
    require(bytes.length == 8)
    var squished = BigInt(0)
    for (i <- 0 until 8) {
      squished |= BigInt(bytes(i)) << (8 * (8 - i - 1))
    }
    squished
  }

  /*
   * Unpacks a 64-bit int into 8 bytes.
   */
  def unsquish(squished: BigInt): List[Int] = {
    var bytes: List[Int] = List[Int]()
    for (i <- 0 until 8) {
      bytes = bytes :+ ((squished & (BigInt(0xFF) << (8 * (8 - i - 1)))) >> (8 * (8 - i - 1))).toInt
    }
    bytes
  }
}

/*
 * Tests non-timed differential coder block in the encode mode.
 */
class DifferentialEncoderTester(c: DifferentialCoder) extends PeekPokeTester(c) {
  val inputs: List[List[Byte]] = List(
    List(3, 4, 5, 6, 7, 8, 9, 9),
    List(0, 0, 0, 0, 0, 0, 0, 0),
    List(1, 1, 1, 1, 1, 1, 1, 1),
    List(1, 56, 97, 0, 100, 23, 5, 5),
    List(1, 1, 2, 2, 3, 3, 4, 4),
    List(4, 4, 3, 3, 2, 2, 1, 1),
    List(5, 4, 5, 6, 5, 4, 5, 6)
  )
  val expectedOutput: List[Byte] = CompressionUtils.differentialEncode(inputs.flatten)
  var last: Byte = 0
  for (i <- inputs.indices) {
    for (j <- 0 until 8)
      poke(c.io.input(j), inputs(i)(j))
    poke(c.io.last, last)
    step(1)
    last = inputs(i).last
    expect(peek(c.io.output).map({ x => x.toByte }).toList == expectedOutput.slice(8 * i, 8 * (i + 1)),
      "actual output did not match expected output.")
  }
}

/*
 * Tests non-timed differential coder block in the decode mode.
 */
class DifferentialDecoderTester(c: DifferentialCoder) extends PeekPokeTester(c) {
  val inputs: List[List[Byte]] = List(
    List(3, 1, 1, 1, 1, 1, 1, 0),
    List(-9, 0, 0, 0, 0, 0, 0, 0),
    List(1, 0, 0, 0, 0, 0, 0, 0),
    List(0, 55, 41, -97, 100, -77, -18, 0),
    List(-4, 0, 1, 0, 1, 0, 1, 0),
    List(0, 0, -1, 0, -1, 0, -1, 0),
    List(4, -1, 1, 1, -1, -1, 1, 1)
  )
  val expectedOutput: List[Byte] = CompressionUtils.differentialDecode(inputs.flatten)
  var last: Byte = 0
  for (i <- inputs.indices) {
    for (j <- 0 until 8)
      poke(c.io.input(j), inputs(i)(j))
    poke(c.io.last, last)
    step(1)
    last = peek(c.io.output).last.toByte
    expect(peek(c.io.output).map({ x => x.toByte }).toList == expectedOutput.slice(8 * i, 8 * (i + 1)),
      "actual output did not match expected output.")
  }
}

/*
 * Tests byte-steam run-length encoder.
 */
class RunLengthEncoderTester(c: RunLengthEncoder) extends PeekPokeTester(c) {
  val inputs: List[List[Byte]] = List(
    List(0, 0, 0, 0, 0, 0, 0, 0, 45),
    List(2, 3, 0, 0, 0, 0, 0, 5, 7, 0, 8, 9, 0, 0, 0, 1),
    List(9, 9, 8, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0),
    List(0),
    List(0, 0, 0, 0),
    List(0, 5, 0, 5, 0, 5, 0)
  )
  for (input <- inputs) {
    test(input)
  }

  def test(input: List[Byte]) = {
    var output: List[Byte] = List[Byte]()
    val expectedOutput = CompressionUtils.runLengthEcode(input)
    poke(c.io.out.ready, true)
    poke(c.io.in.valid, true)
    var i = 0
    while (i < input.length || output.length < expectedOutput.length) {
      if (i < input.length) {
        if (peek(c.io.in.ready) != BigInt(0)) {
          poke(c.io.in.valid, true)
          poke(c.io.in.bits.byte, input(i))
          poke(c.io.in.bits.flag, i == input.length - 1)
          i = i + 1
        }
      }
      if (peek(c.io.out.valid) != BigInt(0)) {
        output = output :+ peek(c.io.out.bits).toByte
      }
      step(1)
      if (i > 50)
        expect(false, "took too long.")
    }
    expect(output == expectedOutput, "actual output did not match expected output.")
  }
}

class RunLengthDecoderTester(c: RunLengthDecoder) extends PeekPokeTester(c) {
  //TODO
}

/*
 * Full CREEC-level test of differential encoding.
 */
class CREECDifferentialEncoderTester(c: CREECDifferentialCoder) extends PeekPokeTester(c) {
  val headers = List(5)
  val datas = List(
    ByteUtils.squish(List(3, 4, 5, 6, 7, 8, 9, 10)),
    ByteUtils.squish(List(0, 1, 2, 7, 8, 8, 8, 8)),
    ByteUtils.squish(List(8, 8, 9, 7, 8, 9, 7, 8)),
    ByteUtils.squish(List(1, 2, 1, 0, 0, 0, 0, 0)),
    ByteUtils.squish(List(0, 0, 1, 0, 0, 12, 0, 12))
  )
  for(header <- headers) {
    poke(c.io.in.header.bits.len, header)
    poke(c.io.in.header.bits.id, 0)
    poke(c.io.in.header.bits.addr, 611)
    poke(c.io.in.header.bits.encrypted, false)
    poke(c.io.in.header.bits.compressed, false)
    poke(c.io.in.header.bits.ecc, false)
  }
}

class CREECDifferentialDecoderTester(c: CREECDifferentialCoder) extends PeekPokeTester(c) {
  //TODO
}


/**
  * From within sbt use:
  * testOnly example.test.CompressionTester
  * From a terminal shell use:
  * sbt 'testOnly example.test.CompressionTester'
  */
class CompressionTester extends ChiselFlatSpec {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  "DifferentialCoderTester" should "encode" in {
    Driver.execute(testerArgs :+ "differential_encoder", () => new DifferentialCoder) {
      c => new DifferentialEncoderTester(c)
    } should be(true)
  }

  "DifferentialCoderTester" should "decode" in {
    Driver.execute(testerArgs :+ "differential_decoder", () => new DifferentialCoder(
      p = new CoderParams(encode = false))) {
      c => new DifferentialDecoderTester(c)
    } should be(true)
  }

  "RunLengthEncoder" should "encode" in {
    Driver.execute(testerArgs :+ "run_length_encoder", () => new RunLengthEncoder) {
      c => new RunLengthEncoderTester(c)
    } should be(true)
  }

  "RunLengthDecoder" should "decode" in {
    Driver.execute(testerArgs :+ "run_length_decoder", () => new RunLengthDecoder) {
      c => new RunLengthDecoderTester(c)
    } should be(true)
  }

  "CREECDifferentialCoder" should "encode" in {
    Driver.execute(testerArgs :+ "creec_differential_encoder", () => new CREECDifferentialCoder) {
      c => new CREECDifferentialEncoderTester(c)
    } should be(true)
  }

  "CREECDifferentialCoder" should "decode" in {
    Driver.execute(testerArgs :+ "creec_differential_decoder", () => new CREECDifferentialCoder(
      coderParams = new CoderParams(encode = false))) {
      c => new CREECDifferentialDecoderTester(c)
    } should be(true)
  }
}