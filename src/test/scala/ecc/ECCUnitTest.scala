package ecc

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class ECCUnitTester(c: ECC) extends PeekPokeTester(c) {
  step(1)
}

/**
  * From within sbt use:
  * testOnly example.test.ECCTester
  * From a terminal shell use:
  * sbt 'testOnly example.test.ECCTester'
  */
class ECCTester extends ChiselFlatSpec {

  "ECC" should "work" in {
    Driver(() => new ECC, "firrtl") {
      c => new ECCUnitTester(c)
    } should be(true)
  }

  "running with --fint-write-vcd" should "create a vcd file from your test" in {
    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new ECC) {
      c => new ECCUnitTester(c)
    } should be(true)
  }
}
