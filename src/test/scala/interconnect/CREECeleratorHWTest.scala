package interconnect

import aes.{CREECEncryptHighModel, CREECDecryptHighModel}
import chisel3.tester._
import compression.CompressorModel
import ecc.{ECCEncoderTopModel, ECCDecoderTopModel, CommChannel, RSParams}
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class CREECeleratorHWTest extends FlatSpec with ChiselScalatestTester {
  val writeTransactions = Seq(
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

  "the entire CREEC RTL pipeline" should "match the SW model" in {
    val modelWrite =
      new CompressorModel(true) ->
      new CREECPadderModel(16) ->
      new CREECEncryptHighModel ->
      new ECCEncoderTopModel(RSParams.RS16_8_8)

    val outWriteGold = modelWrite.processTransactions(writeTransactions)

    // CREEC RTL Write pipeline
    test(new CREECeleratorWrite) { c =>
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      driver.pushTransactions(writeTransactions)
      var cycle = 0
      val timeout = 2000
      while (cycle < timeout && monitor.receivedTransactions.length < outWriteGold.length) {
        c.clock.step()
        cycle += 1
      }

      val outWrite = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outWrite == outWriteGold)
    }

    val readTransactions = outWriteGold

    // Make some noise!
    val modelChannel = new CommChannel(RSParams.RS16_8_8, noiseByteLevel=4)
    val readTransactionsWithNoise = modelChannel.processTransactions(readTransactions)

    val modelRead =
      new ECCDecoderTopModel(RSParams.RS16_8_8) ->
      new CREECDecryptHighModel ->
      new CREECStripperModel ->
      new CompressorModel(false)

    val outReadGold = modelRead.processTransactions(readTransactionsWithNoise)

    // Make sure the SW model also functions correctly!
    assert(outReadGold == writeTransactions)

    // CREEC RTL Read pipeline
    test(new CREECeleratorRead) { c =>
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      driver.pushTransactions(readTransactionsWithNoise)
      var cycle = 0
      val timeout = 2000
      while (cycle < timeout && monitor.receivedTransactions.length < outReadGold.length) {
        c.clock.step()
        cycle += 1
      }

      val outRead = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outRead == outReadGold)
    }
  }
}
