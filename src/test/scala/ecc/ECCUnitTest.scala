package ecc

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class ECCEncoderUnitTester(c: ECCEncoder) extends PeekPokeTester(c) {
  poke(c.io.in.valid, true)
  poke(c.io.out.ready, true)
  poke(c.io.in.bits, 7)
  while (peek(c.io.out.valid) == 0) {
    step(1)
  }
  expect(c.io.out.bits, 15)

  poke(c.io.in.bits, 6)
  step(1)

  while (peek(c.io.out.valid) == 0) {
    step(1)
  }
  expect(c.io.out.bits, 6)

}

/**
  * From within sbt use:
  * testOnly example.test.ECCTester
  * From a terminal shell use:
  * sbt 'testOnly example.test.ECCTester'
  */
class ECCTester extends ChiselFlatSpec {

  "ECCEncoder" should "work" in {
    Driver(() => new ECCEncoder, "firrtl") {
      c => new ECCEncoderUnitTester(c)
    } should be(true)
  }

  "running with --fint-write-vcd" should "create a vcd file from your test" in {
    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new ECCEncoder) {
      c => new ECCEncoderUnitTester(c)
    } should be(true)
  }
}
