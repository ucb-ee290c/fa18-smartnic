package compression

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class CompressionUnitTester(c: Compressor) extends PeekPokeTester(c) {
  //for(i <- 1 to 40 by 3) {
  //  poke(gcd.io.value1, i)
  //  step(1)
  //  val (expected_gcd, steps) = computeGcd(i, j)
  //  expect(gcd.io.outputGCD, expected_gcd)
  //}
}

/**
  * From within sbt use:
  * testOnly example.test.CompressionTester
  * From a terminal shell use:
  * sbt 'testOnly example.test.CompressionTester'
  */
class CompressionTester extends ChiselFlatSpec {
  private val backendNames = if(false && firrtl.FileUtils.isCommandAvailable(Seq("verilator", "--version"))) {
    Array("firrtl", "verilator")
  }
  else {
    Array("firrtl")
  }
  for ( backendName <- backendNames ) {
    "Compression" should s"calculate proper greatest common denominator (with $backendName)" in {
      Driver(() => new Compressor, backendName) {
        c => new CompressionUnitTester(c)
      } should be (true)
    }
  }

  "running with --fint-write-vcd" should "create a vcd file from your test" in {
    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new Compressor) {
      c => new CompressionUnitTester(c)
    } should be(true)
  }
}
