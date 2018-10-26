package compression

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class CompressionUnitTester(c: Compressor) extends PeekPokeTester(c) {
  poke(c.io.in, true)
  step(1)
  expect(c.io.out, true)
  poke(c.io.in, false)
  step(1)
  expect(c.io.out, false)
}

class VarintEncoderUnitTester(c: VarintEncoder) extends PeekPokeTester(c) {
  // 300 = 0b10_0101100
  poke(c.io.in, 300)
  step(1)
  // enc(300) = 0000_0101 0101_1001
  expect(c.io.out, BigInt(1369))
  // 1 = 0b1
  poke(c.io.in, 1)
  step(1)
  // enc(1) = 0000_0011
  expect(c.io.out, BigInt(3))
}

class VarintDecoderUnitTester(c: VarintDecoder) extends PeekPokeTester(c) {
  poke(c.io.in, BigInt(1369))
  step(1)
  expect(c.io.out, 300)
  poke(c.io.in, BigInt(3))
  step(1)
  expect(c.io.out, 1)
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

  "running with --fint-write-vcd" should "create a vcd file from your test" in {
    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new Compressor) {
      c => new CompressionUnitTester(c)
    } should be(true)
  }
}