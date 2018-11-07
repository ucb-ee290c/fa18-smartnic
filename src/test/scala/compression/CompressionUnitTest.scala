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
  def squish(bytes: List[Byte]): BigInt = {
    require(bytes.length == 8)
    var squished = BigInt(0)
    for (i <- 0 until 8) {
      squished |= BigInt(bytes(i)) << (8 * i)
    }
    squished
  }

  /*
   * Unpacks a 64-bit int into 8 bytes.
   */
  def unsquish(squished: BigInt): List[Byte] = {
    var bytes: List[Byte] = List[Byte]()
    for (i <- 0 until 8) {
      bytes = bytes :+ ((squished & (BigInt(0xFF) << (8 * i))) >> (8 * i)).toByte
    }
    bytes
  }
}

/*
 * Tests non-timed differential coder block.
 */
class DifferentialCoderTester(c: DifferentialCoder, encode: Boolean) extends PeekPokeTester(c) {
  val inputs: List[List[Byte]] = List(
    List(3, 4, 5, 6, 7, 8, 9, 9),
    List(0, 0, 0, 0, 0, 0, 0, 0),
    List(1, 1, 1, 1, 1, 1, 1, 1),
    List(1, 56, 97, 0, 100, 23, 5, 5),
    List(1, 1, 2, 2, 3, 3, 4, 4),
    List(4, 4, 3, 3, 2, 2, 1, 1),
    List(5, 4, 5, 6, 5, 4, 5, 6),

    List(3, 1, 1, 1, 1, 1, 1, 0),
    List(-9, 0, 0, 0, 0, 0, 0, 0),
    List(1, 0, 0, 0, 0, 0, 0, 0),
    List(0, 55, 41, -97, 100, -77, -18, 0),
    List(-4, 0, 1, 0, 1, 0, 1, 0),
    List(0, 0, -1, 0, -1, 0, -1, 0),
    List(4, -1, 1, 1, -1, -1, 1, 1)
  )
  val expectedOutput: List[Byte] = if (encode)
    CompressionUtils.differentialEncode(inputs.flatten)
  else
    CompressionUtils.differentialDecode(inputs.flatten)
  var last: Byte = 0
  for (i <- inputs.indices) {
    for (j <- 0 until 8)
      poke(c.io.input(j), inputs(i)(j))
    poke(c.io.last, last)
    step(1)
    last = if (encode)
      inputs(i).last
    else
      peek(c.io.output).last.toByte
    expect(peek(c.io.output).map({ x => x.toByte }).toList == expectedOutput.slice(8 * i, 8 * (i + 1)),
      "actual output did not match expected output.")
  }
}

/*
 * Tests byte-steam run-length coder.
 */
class RunLengthCoderTester(c: RunLengthCoder, encode: Boolean) extends PeekPokeTester(c) {
  val inputs: List[List[Byte]] = if (encode)
    List(
      List(0, 0, 0, 0, 0, 0, 0, 0, 45),
      List(2, 3, 0, 0, 0, 0, 0, 5, 7, 0, 8, 9, 0, 0, 0, 1),
      List(9, 9, 8, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0),
      List(0),
      List(0, 0, 0, 0),
      List(0, 5, 0, 5, 0, 5, 0)
    )
  else
    List(
      List(0, 7, 45),
      List(2, 3, 0, 4, 5, 7, 0, 0, 8, 9, 0, 2, 1),
      List(9, 9, 8, 7, 8, 9, 0, 6),
      List(0, 0),
      List(0, 3),
      List(0, 0, 5, 0, 0, 5, 0, 0, 5, 0, 0)
    )
  for (input <- inputs) {
    test(input)
  }

  def test(input: List[Byte]): Boolean = {
    var output: List[Byte] = List[Byte]()
    val expectedOutput = if (encode)
      CompressionUtils.runLengthEcode(input)
    else
      CompressionUtils.runLengthDecode(input)
    poke(c.io.out.ready, true)
    poke(c.io.in.valid, true)
    var i = 0
    var timeout = 0
    while (i < input.length || output.length < expectedOutput.length) {
      if (i < input.length) {
        if (peek(c.io.in.ready) != BigInt(0)) {
          poke(c.io.in.valid, true)
          if (encode)
            poke(c.io.in.bits.flag, i == input.length - 1)
          poke(c.io.in.bits.byte, input(i))
          i = i + 1
        }
      }
      if (peek(c.io.out.valid) != BigInt(0)) {
        output = output :+ peek(c.io.out.bits).toByte
      }
      step(1)
      timeout += 1
      if (timeout > 50) {
        expect(good = false, "took too long.")
        return false
      }
    }
    expect(output == expectedOutput, "actual output did not match expected output.")
  }
}

/*
 * Full CREEC-level test of differential coding.
 */
class CREECDifferentialCoderTester(c: CREECDifferentialCoder, encode: Boolean) extends PeekPokeTester(c) {
  val allTestAddrs = List(611, 612, 613)
  val allTestLens = List(5, 1, 2)
  val allTestDatas = List(
    List[Byte](
      3, 4, 5, 6, 7, 8, 9, 10,
      0, 1, 2, 7, 8, 8, 8, 8,
      8, 8, 9, 7, 8, 9, 7, 8,
      1, 2, 1, 0, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 12, 0, 12
    ),
    List[Byte](
      0, 0, 0, 0, 0, 0, 0, 0
    ),
    List[Byte](
      1, 2, 3, 4, 5, 6, 7, 8,
      9, 10, 11, 12, 13, 14, 15, 16
    )
  )

  for (i <- allTestAddrs.indices) {
    test(allTestAddrs(i), allTestLens(i), allTestDatas(i))
  }

  def test(addr: Int, len: Int, data: List[Byte]): Unit = {
    val expectedData = if (encode)
      CompressionUtils.differentialEncode(data)
    else
      CompressionUtils.differentialDecode(data)
    var header = Header(len, addr)
    var expectedHeader = Header(len, addr)
    var datas: List[Data] = List[Data]()
    for (i <- 0 until math.ceil(data.length / 8.0).toInt) {
      datas = datas :+ Data(ByteUtils.squish(data.slice(8 * i, 8 * i + 8)))
    }
    var expectedDatas: List[Data] = List[Data]()
    for (i <- 0 until math.ceil(data.length / 8.0).toInt) {
      expectedDatas = expectedDatas :+ Data(ByteUtils.squish(expectedData.slice(8 * i, 8 * i + 8)))
    }

    poke(c.io.in.header.valid, true)
    poke(c.io.in.data.valid, true)
    poke(c.io.out.header.ready, true)
    poke(c.io.out.data.ready, true)

    var i = 0
    var datasOut: List[Data] = List[Data]()
    while (i < datas.length || datasOut.length < expectedDatas.length) {
      if (peek(c.io.in.header.ready) != BigInt(0)) {
        pokeHeader(c, header)
      }
      if (i < datas.length) {
        val data = datas(i)
        if (peek(c.io.in.data.ready) != BigInt(0)) {
          pokeData(c, data)
          i = i + 1
        }
      }
      if (peek(c.io.out.header.valid) != BigInt(0)) {
        expect(peekHeader(c) == expectedHeader, "input and output headers did not match.")
      }
      if (peek(c.io.out.data.valid) != BigInt(0)) {
        datasOut = datasOut :+ peekData(c)
      }

      step(1)
      if (i > 200)
        expect(good = false, "took too long.")
    }
    expect(datasOut.flatMap({ x => ByteUtils.unsquish(x.data) }) == expectedData,
      "actual output did not match expected output.")
  }

  case class Header(len: Int, addr: Int, id: Int = 0, encrypted: Boolean = false,
                    compressed: Boolean = false, ecc: Boolean = false) {
  }

  case class Data(data: BigInt, id: Int = 0) {
    override def toString: String = {
      "%016x".format(data)
    }
  }

  def peekData(c: CREECDifferentialCoder): Data = {
    Data(
      data = peek(c.io.out.data.bits.data),
      id = peek(c.io.out.data.bits.id).toInt
    )
  }

  def peekHeader(c: CREECDifferentialCoder): Header = {
    Header(
      len = peek(c.io.out.header.bits.len).toInt,
      addr = peek(c.io.out.header.bits.addr).toInt,
      id = peek(c.io.out.header.bits.id).toInt,
      encrypted = peek(c.io.out.header.bits.encrypted) != 0,
      compressed = peek(c.io.out.header.bits.compressed) != 0,
      ecc = peek(c.io.out.header.bits.ecc) != 0
    )
  }

  def pokeHeader(c: CREECDifferentialCoder, header: Header): Unit = {
    poke(c.io.in.header.bits.len, header.len)
    poke(c.io.in.header.bits.id, header.id)
    poke(c.io.in.header.bits.addr, header.addr)
    poke(c.io.in.header.bits.encrypted, header.encrypted)
    poke(c.io.in.header.bits.compressed, header.compressed)
    poke(c.io.in.header.bits.ecc, header.ecc)
  }

  def pokeData(c: CREECDifferentialCoder, data: Data): Unit = {
    poke(c.io.in.data.bits.data, data.data)
    poke(c.io.in.data.bits.id, data.id)
  }
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

  "DifferentialCoder" should "encode" in {
    Driver.execute(testerArgs :+ "differential_encoder", () => new DifferentialCoder) {
      c => new DifferentialCoderTester(c, true)
    } should be(true)
  }

  "DifferentialCoder" should "decode" in {
    Driver.execute(testerArgs :+ "differential_decoder", () => new DifferentialCoder(
      p = CoderParams(encode = false))) {
      c => new DifferentialCoderTester(c, false)
    } should be(true)
  }

  "RunLengthEncoder" should "encode" in {
    Driver.execute(testerArgs :+ "run_length_encoder", () => new RunLengthCoder(encode = true)) {
      c => new RunLengthCoderTester(c, true)
    } should be(true)
  }

  "RunLengthDecoder" should "decode" in {
    Driver.execute(testerArgs :+ "run_length_decoder", () => new RunLengthCoder(encode = false)) {
      c => new RunLengthCoderTester(c, false)
    } should be(true)
  }

  "CREECDifferentialCoder" should "encode" in {
    Driver.execute(testerArgs :+ "creec_differential_encoder", () => new CREECDifferentialCoder) {
      c => new CREECDifferentialCoderTester(c, true)
    } should be(true)
  }

  "CREECDifferentialCoder" should "decode" in {
    Driver.execute(testerArgs :+ "creec_differential_decoder", () => new CREECDifferentialCoder(
      coderParams = CoderParams(encode = false))) {
      c => new CREECDifferentialCoderTester(c, false)
    } should be(true)
  }
}