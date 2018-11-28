package ecc
import chisel3._
import chisel3.tester._
import interconnect._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class CREECBusECCTest extends ECCSpec with ChiselScalatestTester {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  val busParams = new CREECBusParams

  // Encoder testing
  val inputsEnc = trials.map(x =>
    Seq() ++ x._1.slice(0, rsParams.k).map(_.toByte)).flatten

  // Software golden model
  val txEnc = CREECHighLevelTransaction(inputsEnc, 0x1000)
  val modelEnc = new ECCEncoderTopModel(rsParams)
  val outGoldEnc = modelEnc.processTransactions(Seq(txEnc))

  "ECCEncoderTop" should "be testable with testers2" in {
    test(new ECCEncoderTop(rsParams, busParams)) { c =>
      val tx = CREECHighLevelTransaction(inputsEnc, 0x1000)
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(Seq(tx))

      var cycle = 0
      val timeout = 2000
      while (cycle < timeout &&
             monitor.receivedTransactions.length < outGoldEnc.length) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == outGoldEnc)
    }
  }

  // Decoder testing
  val inputsDec = trials.map(x =>
    Seq() ++ x._2.slice(0, rsParams.n).map(_.toByte)).flatten

  // Software golden model
  val txDec = CREECHighLevelTransaction(inputsDec, 0x1000)
  val modelDec = new ECCDecoderTopModel(rsParams, busParams)
  val outGoldDec = modelDec.processTransactions(Seq(txDec))

  "ECCDecoderTop" should "be testable with testers2" in {
    test(new ECCDecoderTop(rsParams, busParams)) { c =>
      val tx = CREECHighLevelTransaction(inputsDec, 0x1000)
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(Seq(txDec))

      var cycle = 0
      val timeout = 2000
      while (cycle < timeout &&
             monitor.receivedTransactions.length < outGoldDec.length) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == outGoldDec)
    }
  }

}
