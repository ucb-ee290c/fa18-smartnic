package interconnect

import aes.CREECEncryptHighModel
import chisel3.tester._
import compression.CompressorModel
import ecc.{ECCEncoderTopModel, RSParams}
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class CREECeleratorHWTest extends FlatSpec with ChiselScalatestTester {
  val transactions = Seq(
    CREECHighLevelTransaction(
      Seq(
        1, 2, 3, 4, 5, 6, 7, 8,
        2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 3,
        3, 4, 4, 4, 4, 4, 4, 4
      ), 0x509
    )
  )

  "the entire CREEC write RTL pipeline" should "match the SW model" in {
    val model =
      new CompressorModel(true) ->
      new CREECPadderModel(16) ->
      new CREECEncryptHighModel ->
      new ECCEncoderTopModel(RSParams.RS16_8_8)
      //new CompressorModel(true) ->
      //new ECCEncoderTopModel(RSParams.RS16_8_8) ->
      //new CREECPadderModel(16) ->
      //new CREECEncryptHighModel()

    val outGold = model.processTransactions(transactions)

    test(new CREECelerator) { c =>
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      driver.pushTransactions(transactions)
      var cycle = 0
      val timeout = 2000
      while (cycle < timeout && monitor.receivedTransactions.length < outGold.length) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == outGold)
    }
  }
}
