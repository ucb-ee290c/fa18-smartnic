package compression

import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import interconnect.{CREECBusParams, CREECHighLevelTransaction}
import org.scalatest.FlatSpec

class CREECCompressionModuleTester extends FlatSpec with ChiselScalatestTester {
  implicit val creecParams: CREECBusParams = new CREECBusParams

  val transaction = Seq(CREECHighLevelTransaction(Seq(
    1, 2, 3, 4, 5, 6, 7, 8,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 3,
    3, 4, 4, 4, 4, 4, 4, 4
  ), 0x509))

  val differentialTransaction = Seq(CREECHighLevelTransaction(Seq(
    1, 1, 1, 1, 1, 1, 1, 1,
    -6, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    -2, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 3,
    0, 1, 0, 0, 0, 0, 0, 0
  ), 0x509))

  "the CREECDifferentialCoder module" should "encode and decode data" in {
    test(new CREECDifferentialCoder(coderParams = CoderParams(encode = true))) { c =>
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      driver.pushTransactions(transaction)
      var cycle = 0
      val timeout = 100
      while (cycle < timeout && monitor.receivedTransactions.length < differentialTransaction.length) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_=>true)
      // TODO: (testers2 usability) assert kills everything and prevents vcd generation
      assert(out == differentialTransaction)
    }
  }
}