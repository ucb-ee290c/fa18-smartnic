package ecc
import chisel3.tester._
import interconnect._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}

class CREECBusECCTest extends ECCSpec with ChiselScalatestTester {
  val busParams = BusParams.creec
  val busECCParams = BusParams.ecc

  // Encoder testing
  val inputsEnc = trials.map(x =>
    Seq() ++ x._1.slice(0, rsParams.k).map(_.toByte)).flatten

  // Range values should be multiples of 8
  val txLen1 = (0, 80)
  val txLen2 = (80, 160)
  val txLen3 = (160, 240)
  val txLen4 = (240, 320)

  // Software golden model
  val txEnc1 = CREECHighLevelTransaction(
                 inputsEnc.slice(txLen1._1, txLen1._2), 0x1000)
  val txEnc2 = CREECHighLevelTransaction(
                 inputsEnc.slice(txLen2._1, txLen2._2), 0x2000)
  val txEnc3 = CREECHighLevelTransaction(
                 inputsEnc.slice(txLen3._1, txLen3._2), 0x3000)
  val txEnc4 = CREECHighLevelTransaction(
                 inputsEnc.slice(txLen4._1, txLen4._2), 0x4000)

  val modelEnc = new ECCEncoderTopModel(rsParams)
  val outGoldEnc = modelEnc.processTransactions(
                     Seq(txEnc1, txEnc2, txEnc3, txEnc4))

  "ECCEncoderTop" should "be testable with testers2" in {
    test(new ECCEncoderTop(rsParams, busParams, busECCParams)) { c =>
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(Seq(txEnc1, txEnc2, txEnc3, txEnc4))

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
  val txDec1 = CREECHighLevelTransaction(
                 inputsDec.slice(txLen1._1, txLen1._2), 0x1000)
  val txDec2 = CREECHighLevelTransaction(
                 inputsDec.slice(txLen2._1, txLen2._2), 0x2000)
  val txDec3 = CREECHighLevelTransaction(
                 inputsDec.slice(txLen3._1, txLen3._2), 0x3000)
  val txDec4 = CREECHighLevelTransaction(
                 inputsDec.slice(txLen4._1, txLen4._2), 0x4000)

  val modelDec = new ECCDecoderTopModel(rsParams)
  val outGoldDec = modelDec.processTransactions(
                     Seq(txDec1, txDec2, txDec3, txDec4))

  "ECCDecoderTop" should "be testable with testers2" in {
    test(new ECCDecoderTop(rsParams, busECCParams, busParams)) { c =>
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(Seq(txDec1, txDec2, txDec3, txDec4))

      var cycle = 0
      val timeout = 8000
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
