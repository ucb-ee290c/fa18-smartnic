package compression

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import interconnect.CREECBusParams

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
  val expectedOutput: Seq[Byte] = CompressionUtils.differential(inputs.flatten, encode)
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
    val expectedOutput = CompressionUtils.runLength(input, encode)
    poke(c.io.out.ready, true)
    poke(c.io.in.valid, true)
    var i = 0
    var timeout = 0
    var done = false
    while (!done) {
      if (i < input.length) {
        if (peek(c.io.in.ready) != BigInt(0)) {
          poke(c.io.in.valid, true)
          poke(c.io.in.bits.flag, i == input.length - 1)
          poke(c.io.in.bits.byte, input(i))
          i = i + 1
        }
      }
      done = peek(c.io.out.bits.flag) != BigInt(0)
      if (peek(c.io.out.valid) != BigInt(0) && !done) {
        output = output :+ peek(c.io.out.bits.byte).toByte
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
 * Test of fifo used in RunLength coder.
 */
class BasicFIFOTester(c: BasicFIFO) extends PeekPokeTester(c) {
  val inputs = List(45, 80, 1, 12, 15, 9, 0, 0, 22)
  poke(c.io.push, true)
  //push in all inputs
  for (input <- inputs) {
    poke(c.io.in, input)
    step(1)
  }
  poke(c.io.push, false)
  poke(c.io.pop, true)
  //expect all outputs to follow
  for (input <- inputs) {
    expect(peek(c.io.out) == input, "actual output did not match expected output")
    step(1)
  }
  poke(c.io.pop, false)
  poke(c.io.reset, true)
  step(5)
  poke(c.io.reset, false)

  //push in a step ahead of pop
  poke(c.io.in, 17)
  poke(c.io.push, true)
  poke(c.io.pop, false)
  step(1)
  poke(c.io.in, 99)
  poke(c.io.push, true)
  poke(c.io.pop, true)
  expect(peek(c.io.out) == 17, "failure.")
  step(1)
  poke(c.io.in, 31)
  poke(c.io.push, true)
  poke(c.io.pop, true)
  expect(peek(c.io.out) == 99, "failure.")
  step(1)
  poke(c.io.push, false)
  poke(c.io.pop, true)
  expect(peek(c.io.out) == 31, "failure.")
}

/*
 * Test of creec-level run-length encoding
 */
class CREECCoderTester(c: CREECCoder, encode: Boolean, operation: String) extends PeekPokeTester(c) {
  require(List("differential", "runLength", "compression").contains(operation))
  val allTestAddrs = List(611, 612, 613, 614, 615, 616)
  val allTestLens = List(5, 1, 2, 9, 10, 10)
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
    ),
    List[Byte](
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0
    ),
    List[Byte](
      5, 5, 5, 5, 5, 5, 5, 5,
      5, 5, 5, 5, 5, 5, 5, 5,
      9, 7, 5, 4, 5, 4, 4, 4,
      4, 4, 4, 4, 5, 6, 7, 8,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 9, 3, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1
    ),
    List[Byte](
      0, 2, 0, 2, 0, 2, 0, 2,
      0, 2, 0, 2, 0, 2, 0, 2,
      0, 2, 0, 2, 0, 2, 0, 2,
      0, 2, 0, 2, 0, 2, 0, 2,
      0, 2, 0, 2, 0, 2, 0, 2,
      2, 0, 2, 0, 2, 0, 2, 0,
      2, 0, 2, 0, 2, 0, 2, 0,
      2, 0, 2, 0, 2, 0, 2, 0,
      2, 0, 2, 0, 2, 0, 2, 0,
      2, 0, 2, 0, 2, 0, 2, 0
    )
  )

  for (i <- allTestAddrs.indices) {
    test(allTestAddrs(i), allTestLens(i), allTestDatas(i))
  }

  def test(addr: Int, len: Int, data: List[Byte]): Boolean = {
    var expectedData = {
      if (operation == "runLength")
        CompressionUtils.runLength(data, encode)
      else if (operation == "differential")
        CompressionUtils.differential(data, encode)
      else
        CompressionUtils.runLength(CompressionUtils.differential(data, encode), encode)
    }

    while (expectedData.length % 8 != 0) {
      expectedData = expectedData :+ 0.toByte
    }
    val header = Header(len, addr)
    val expectedHeader = Header(len, addr)
    var datas: List[Data] = List[Data]()
    for (i <- 0 until math.ceil(data.length / 8.0).toInt) {
      datas = datas :+ Data(ByteUtils.squish(data.slice(8 * i, 8 * i + 8)))
    }
    var expectedDatas: List[Data] = List[Data]()
    for (i <- 0 until math.ceil(expectedData.length / 8.0).toInt) {
      expectedDatas = expectedDatas :+ Data(ByteUtils.squish(expectedData.slice(8 * i, 8 * i + 8)))
    }

    poke(c.io.in.header.valid, true)
    poke(c.io.in.data.valid, true)
    poke(c.io.out.header.ready, true)
    poke(c.io.out.data.ready, true)

    var i = 0
    var timeout = 0
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
      timeout += 1
      if (timeout > 1000) {
        expect(good = false, "took too long.")
        return false
      }
    }
    println("in :" + data)
    println("got:" + datasOut.flatMap({ x => ByteUtils.unsquish(x.data) }))
    println("exp:" + expectedData)
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

  def peekData(c: CREECCoder): Data = {
    Data(
      data = peek(c.io.out.data.bits.data),
      id = peek(c.io.out.data.bits.id).toInt
    )
  }

  def peekHeader(c: CREECCoder): Header = {
    Header(
      len = peek(c.io.out.header.bits.len).toInt,
      addr = peek(c.io.out.header.bits.addr).toInt,
      id = peek(c.io.out.header.bits.id).toInt,
      encrypted = peek(c.io.out.header.bits.encrypted) != 0,
      compressed = peek(c.io.out.header.bits.compressed) != 0,
      ecc = peek(c.io.out.header.bits.ecc) != 0
    )
  }

  def pokeHeader(c: CREECCoder, header: Header): Unit = {
    poke(c.io.in.header.bits.len, header.len)
    poke(c.io.in.header.bits.id, header.id)
    poke(c.io.in.header.bits.addr, header.addr)
    poke(c.io.in.header.bits.encrypted, header.encrypted)
    poke(c.io.in.header.bits.compressed, header.compressed)
    poke(c.io.in.header.bits.ecc, header.ecc)
  }

  def pokeData(c: CREECCoder, data: Data): Unit = {
    poke(c.io.in.data.bits.data, data.data)
    poke(c.io.in.data.bits.id, data.id)
  }
}

/*
 * Top-level test.
 */
class CompressorTester(c: Compressor) extends PeekPokeTester(c) {
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

  implicit val creecParams = new CREECBusParams

  "DifferentialCoder" should "encode" in {
    Driver.execute(testerArgs :+ "differential_encoder", () => new DifferentialCoder(
      coderParams = CoderParams(encode = true))) {
      c => new DifferentialCoderTester(c, true)
    } should be(true)
  }

  "DifferentialCoder" should "decode" in {
    Driver.execute(testerArgs :+ "differential_decoder", () => new DifferentialCoder(
      coderParams = CoderParams(encode = false))) {
      c => new DifferentialCoderTester(c, false)
    } should be(true)
  }

  "RunLengthEncoder" should "encode" in {
    Driver.execute(testerArgs :+ "run_length_encoder", () => new RunLengthCoder(
      coderParams = CoderParams(encode = true))) {
      c => new RunLengthCoderTester(c, true)
    } should be(true)
  }

  "RunLengthDecoder" should "decode" in {
    Driver.execute(testerArgs :+ "run_length_decoder", () => new RunLengthCoder(
      coderParams = CoderParams(encode = false))) {
      c => new RunLengthCoderTester(c, false)
    } should be(true)
  }

  "CREECDifferentialCoder" should "encode" in {
    Driver.execute(testerArgs :+ "creec_differential_encoder", () => new CREECCoder(
      coderParams = CoderParams(encode = true), operation = "differential")) {
      c => new CREECCoderTester(c, true, "differential")
    } should be(true)
  }

  "CREECDifferentialCoder" should "decode" in {
    Driver.execute(testerArgs :+ "creec_differential_decoder", () => new CREECCoder(
      coderParams = CoderParams(encode = false), operation = "differential")) {
      c => new CREECCoderTester(c, false, "differential")
    } should be(true)
  }

  "CREECRunLengthCoder" should "encode" in {
    Driver.execute(testerArgs :+ "creec_run_length_encoder", () => new CREECCoder(
      coderParams = CoderParams(encode = true), operation = "runLength")) {
      c => new CREECCoderTester(c, true, "runLength")
    } should be(true)
  }

  "CREECRunLengthCoder" should "decode" in {
    Driver.execute(testerArgs :+ "creec_run_length_decoder", () => new CREECCoder(
      coderParams = CoderParams(encode = false), operation = "runLength")) {
      c => new CREECCoderTester(c, false, "runLength")
    } should be(true)
  }

  "BasicFIFO" should "work" in {
    Driver.execute(testerArgs :+ "basic_fifo", () => new BasicFIFO(64, 128)) {
      c => new BasicFIFOTester(c)
    } should be(true)
  }

  //  "Compressor" should "compress" in {
  //    Driver.execute(testerArgs :+ "compressor", () => new CREECCoder(
  //      operation = "compression")) {
  //      c => new CREECCoderTester(c, true, "compression")
  //    } should be(true)
  //  }
  //
  //  "Compressor" should "decompress" in {
  //    Driver.execute(testerArgs :+ "compressor", () => new CREECCoder(
  //      coderParams = new CoderParams(encode = false), operation = "compression")) {
  //      c => new CREECCoderTester(c, false, "compression")
  //    } should be(true)
  //  }
}