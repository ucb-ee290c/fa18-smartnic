package compression

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

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
class VarintTester extends ChiselFlatSpec {
  "VarintEncoder" should "encode" in {
    Driver(() => new VarintEncoder(), "firrtl") {
      c => new VarintEncoderUnitTester(c)
    } should be(true)
  }

  "VarintDecoder" should "decode" in {
    Driver(() => new VarintDecoder(), "firrtl") {
      c => new VarintDecoderUnitTester(c)
    } should be(true)
  }
}