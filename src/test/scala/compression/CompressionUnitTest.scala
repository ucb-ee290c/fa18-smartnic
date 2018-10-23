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
  poke(c.io.in, 300)
  step(1)
  expect(c.io.out, 44034)
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
    Driver(() => new VarintEncoder(2), "firrtl") {
      c => new VarintEncoderUnitTester(c)
    } should be (true)
  }

  "running with --fint-write-vcd" should "create a vcd file from your test" in {
    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new Compressor) {
      c => new CompressionUnitTester(c)
    } should be(true)
  }
}