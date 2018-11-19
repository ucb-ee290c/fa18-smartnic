package ecc
import chisel3._
import chisel3.tester._
import interconnect._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class ECCBusModelTest extends ECCSpec with ChiselScalatestTester {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  val inputs = trials.map(x =>
    Seq() ++ x._1.slice(0, rsParams.k).map(_.toByte)).flatten

  "ECCEncoderTop" should "be testable with testers2" in {
    test(new ECCEncoderTop(rsParams)) { c =>
      val tx = CREECHighLevelTransaction(inputs, 0x1000)
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(Seq(tx))

      fork {
        c.clock.step(100)
      } .join()

      println(monitor.receivedTransactions.dequeueAll(_ => true))
    }
  }

//  "ECCDecoderTop" should "be testable with testers2" in {
//    test(new ECCDecoderTop(rsParams)) { c =>
//      val tx = CREECHighLevelTransaction(trials(0)._2.map(_.toByte), 0x1000)
//      val driver = new CREECDriver(c.io.slave, c.clock)
//      val monitor = new CREECMonitor(c.io.master, c.clock)
//
//      driver.pushTransactions(Seq(tx))
//
//      fork {
//        c.clock.step(100)
//      } .join()
//
//      println(monitor.receivedTransactions.dequeueAll(_ => true))
//    }
//  }

}
