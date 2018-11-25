package compression

import chisel3.tester.ChiselScalatestTester
import interconnect.CREECBusParams
import org.scalatest.FlatSpec

class CREECCompressionModuleTester extends FlatSpec with ChiselScalatestTester {
  implicit val creecParams = new CREECBusParams
  "the CREECDifferentialCoder module" should "encode and decode data" in {
    test(new CREECDifferentialCoder(coderParams = CoderParams(encode = true))) {c =>
      
    }
  }
}