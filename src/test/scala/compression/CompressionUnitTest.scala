package compression

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class CompressionUnitTester(c: Compressor) extends PeekPokeTester(c) {
  poke(c.io.in.bits, 25)
  poke(c.io.in.valid, 1)
}

object VarintUtils {
  def encode(i: BigInt): BigInt = {
    require(i >= 0)
    val s = i.toString(2).reverse // reverse so that chunking the tail of the int works
    val dataParts = s.grouped(7).toList.map(_.reverse)
    val dataPartsWithValid = dataParts.map(_ + "1")
    BigInt(dataPartsWithValid.reverse.reduce(_ + _), 2)
  }

  def decode(i: BigInt): BigInt = {
    val s = i.toString(2).reverse
    val dataParts = s.grouped(8).toList.map(_.reverse)
    val dataPartsWithoutValid = dataParts.map(_.dropRight(1))
    BigInt(dataPartsWithoutValid.reverse.reduce(_ + _), 2)
  }
}

object CompressionFunctions {
  private def differential(input: List[Byte], encode: Boolean): List[Byte] = {
    var output = List[Byte]()
    var prev = 0.toByte
    for(i <- 0 until input.length) {
      output = output :+ (if(encode) (Byte)(input(i) - prev) else (Byte)(input(i) + prev))
      prev = if(encode) input(i) else output(i)
    }
    output
  }
  def differentialEncode(input: List[Byte]): List[Byte] = {
    differential(input, true)
  }

  def differentialDecode(input: List[Byte]): List[Byte] = {
    differential(input, false)
  }

//  def runLengthEcode(input: List[Boolean]): List[Boolean] = {
//    var 
//  }
}

class DifferentialEncoderTester(c: DifferentialEncoder) extends PeekPokeTester(c) {
  val testInput = List(45, 46, 47, 45, 43, 40, 41, 41, 42, 40, 20)
  val encoded = CompressionFunctions.differentialEncode(testInput)
  val recovered = CompressionFunctions.differentialDecode(encoded)
  println(testInput.toString())
  println(encoded.toString())
  println(recovered.toString())
}

class VarintEncoderUnitTester(c: VarintEncoder) extends PeekPokeTester(c) {
  // 300 = 0b10_0101100
  poke(c.io.in, 300)
  step(1)
  // enc(300) = 0000_0101 0101_1001
  expect(c.io.out, VarintUtils.encode(300))
  // 1 = 0b1
  poke(c.io.in, 1)
  step(1)
  // enc(1) = 0000_0011
  expect(c.io.out, VarintUtils.encode(1))
}

class VarintDecoderUnitTester(c: VarintDecoder) extends PeekPokeTester(c) {
  poke(c.io.in, BigInt(1369))
  step(1)
  expect(c.io.out, VarintUtils.decode(1369))
  poke(c.io.in, BigInt(3))
  step(1)
  expect(c.io.out, VarintUtils.decode(3))
}

/**
  * From within sbt use:
  * testOnly example.test.CompressionTester
  * From a terminal shell use:
  * sbt 'testOnly example.test.CompressionTester'
  */
class CompressionTester extends ChiselFlatSpec {

  "Compression" should "work" in {
    Driver(() => new Compressor, "firrtl") {
      c => new CompressionUnitTester(c)
    } should be(true)
  }

  "VarintEncoder" should "encode" in {
    Driver(() => new VarintEncoder(), "firrtl") {
      c => new VarintEncoderUnitTester(c)
    } should be (true)
  }

  "VarintDecoder" should "decode" in {
    Driver(() => new VarintDecoder(), "firrtl") {
      c => new VarintDecoderUnitTester(c)
    } should be (true)
  }

  "DifferentialEncoderTester" should "encode" in {
    Driver(() => new DifferentialEncoder, "firrtl") {
      c => new DifferentialEncoderTester(c)
    } should be (true)
  }

  "running with --fint-write-vcd" should "create a vcd file from your test" in {
    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new Compressor) {
      c => new CompressionUnitTester(c)
    } should be(true)
  }
}