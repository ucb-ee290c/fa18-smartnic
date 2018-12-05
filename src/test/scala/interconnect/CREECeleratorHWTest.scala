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
    printf("Create Write Model...\n")
    val modelWrite =
      new CompressorModel(true) ->
      new CREECPadderModel(16) ->
      new CREECEncryptHighModel ->
      new ECCEncoderTopModel(RSParams.RS16_8_8)

    val outWriteGold = modelWrite.processTransactions(writeTransactions)

    // CREEC RTL Write pipeline
    printf("Running write test...\n")
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
    printf("Create Read Model...\n")
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
    printf("Running read test...\n")
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

    printf("Running write test with full block...\n")
    test(new CREECeleratorFull) { c =>
      val write_driver = new CREECDriver(c.io.write_in, c.clock)
      val write_monitor = new CREECMonitor(c.io.write_out, c.clock)
      write_driver.pushTransactions(writeTransactions)
      var cycle = 0
      val timeout = 2000
      while (cycle < timeout && write_monitor.receivedTransactions.length < outWriteGold.length) {
        c.clock.step()
        cycle += 1
      }

      val outWrite = write_monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outWrite == outWriteGold)
    }
    printf("Running read test with full block...\n")
    test(new CREECeleratorFull) { c =>
      val read_driver = new CREECDriver(c.io.read_in, c.clock)
      val read_monitor = new CREECMonitor(c.io.read_out, c.clock)
      read_driver.pushTransactions(readTransactionsWithNoise)
      var cycle = 0
      val timeout = 2000
      while (cycle < timeout && read_monitor.receivedTransactions.length < outReadGold.length) {
        c.clock.step()
        cycle += 1
      }

      val outRead = read_monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outRead == outReadGold)
    }

    printf("Running full loop test with full block...\n")
    test(new CREECeleratorFull) { c =>
      val write_driver = new CREECDriver(c.io.write_in, c.clock)
      val write_monitor = new CREECMonitor(c.io.write_out, c.clock)
      val read_driver = new CREECDriver(c.io.read_in, c.clock)
      val read_monitor = new CREECMonitor(c.io.read_out, c.clock)
      val timeout = 2000

      //run write loop
      write_driver.pushTransactions(writeTransactions)
      var cycle = 0
      while (cycle < timeout && write_monitor.receivedTransactions.length < outWriteGold.length) {
        c.clock.step()
        cycle += 1
      }

      val outWrite = write_monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outWrite == outWriteGold)

      //run read back loop
      read_driver.pushTransactions(outWrite)
      cycle = 0
      while (cycle < timeout && read_monitor.receivedTransactions.length < outReadGold.length) {
        c.clock.step()
        cycle += 1
      }

      val outRead = read_monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outRead == outReadGold)
    }

    printf("Running simultaneous stimulus test with full block...\n")
    test(new CREECeleratorFull) { c =>
      val write_driver = new CREECDriver(c.io.write_in, c.clock)
      val write_monitor = new CREECMonitor(c.io.write_out, c.clock)
      val read_driver = new CREECDriver(c.io.read_in, c.clock)
      val read_monitor = new CREECMonitor(c.io.read_out, c.clock)
      val timeout = 2000

      //Drive both sides
      write_driver.pushTransactions(writeTransactions)
      read_driver.pushTransactions(readTransactionsWithNoise)
      var cycle = 0
      //Either timeout, or wait for both of the monitors to finish
      while (cycle < timeout && (
        write_monitor.receivedTransactions.length < outWriteGold.length
        || read_monitor.receivedTransactions.length < outReadGold.length)) {
        c.clock.step()
        cycle += 1
      }

      val outWrite = write_monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outWrite == outWriteGold)

      val outRead = read_monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outRead == outReadGold)
    }
  }
}
