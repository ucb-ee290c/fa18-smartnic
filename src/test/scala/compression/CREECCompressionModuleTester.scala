package compression

import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import interconnect.{BlockDeviceIOBusParams, CREECBusParams, CREECHighLevelTransaction}
import org.scalatest.FlatSpec

class CREECCompressionModuleTester extends FlatSpec with ChiselScalatestTester {
  implicit val creecParams: CREECBusParams = new CREECBusParams

  val transactions = Seq(
    CREECHighLevelTransaction(
      Seq(
        1, 2, 3, 4, 5, 6, 7, 8,
        2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 3,
        3, 4, 4, 4, 4, 4, 4, 4
      ),
      0x509
    ),
    CREECHighLevelTransaction(
      Seq(
        3, 4, 5, 6, 7, 8, 9, 10,
        0, 1, 2, 7, 8, 8, 8, 8,
        8, 8, 9, 7, 8, 9, 7, 8,
        1, 2, 1, 0, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 12, 0, 12
      ),
      0x510
    ),
    CREECHighLevelTransaction(
      Seq(
        0, 0, 0, 0, 0, 0, 0, 0
      ),
      0x511
    ),
    CREECHighLevelTransaction(
      Seq(
        1, 2, 3, 4, 5, 6, 7, 8,
        9, 10, 11, 12, 13, 14, 15, 16
      ),
      0x512
    ),
    CREECHighLevelTransaction(
      Seq(
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
      ),
      0x513
    ),
    CREECHighLevelTransaction(
      Seq(
        5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5,
        9, 7, 5, 4, 5, 4, 4, 4,
        4, 4, 4, 4, 5, 6, 7, 8,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 9, 3, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1
      ),
      0x514
    ),
    //TODO: figure out why this particular test doesn't work
    //    CREECHighLevelTransaction(
    //      Seq(
    //        0, 2, 0, 2, 0, 2, 0, 2,
    //        0, 2, 0, 2, 0, 2, 0, 2,
    //        0, 2, 0, 2, 0, 2, 0, 2,
    //        0, 2, 0, 2, 0, 2, 0, 2,
    //        0, 2, 0, 2, 0, 2, 0, 2,
    //        2, 0, 2, 0, 2, 0, 2, 0,
    //        2, 0, 2, 0, 2, 0, 2, 0,
    //        2, 0, 2, 0, 2, 0, 2, 0,
    //        2, 0, 2, 0, 2, 0, 2, 0,
    //        2, 0, 2, 0, 2, 0, 3, 2
    //      ),
    //      0x515
  )

  "the CREECDifferentialCoder module" should "encode and decode data" in {
    for (encode <- List(true, false)) {
      val model = new CREECDifferentialCoderModel(encode = encode)
      val outGold = model.processTransactions(transactions)

      test(new CREECDifferentialCoder(coderParams = CoderParams(encode = encode))) { c =>
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

  //TODO: don't copy and paste this just to change the module/model being tested
  "the CREECRunLengthCoder module" should "encode and decode data" in {
    for (encode <- List(true, false)) {
      val model = new CREECRunLengthCoderModel(encode = encode)
      val outGold = model.processTransactions(transactions)

      test(new CREECRunLengthCoder(coderParams = CoderParams(encode = encode))) { c =>
        val driver = new CREECDriver(c.io.in, c.clock)
        val monitor = new CREECMonitor(c.io.out, c.clock)
        driver.pushTransactions(transactions)
        var cycle = 0
        val timeout = 2000
        while (cycle <= timeout && monitor.receivedTransactions.length < outGold.length) {
          c.clock.step()
          cycle += 1
        }
        if (cycle >= timeout)
          println("Test timed out!!!!!!!!!!!!!!!!!!!!!!!!")

        val out = monitor.receivedTransactions.dequeueAll(_ => true)
        assert(out == outGold)
        // TODO: (testers2 usability) assert kills everything and prevents vcd generation
      }
    }
  }

  //TODO: don't copy and paste this just to change the module/model being tested
  "the Compressor module" should "compress and uncompress" in {
    var compress = true
    val model = new CompressorModel(compress = compress)
    val outGold = model.processTransactions(transactions)

    test(new Compressor(new BlockDeviceIOBusParams, compress)) { c =>
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      driver.pushTransactions(transactions)
      var cycle = 0
      val timeout = 2000
      while (cycle <= timeout && monitor.receivedTransactions.length < outGold.length) {
        c.clock.step()
        cycle += 1
      }
      if (cycle >= timeout)
        println("Test timed out!!!!!!!!!!!!!!!!!!!!!!!!")

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == outGold)
    }
    //TODO: no copy-paste
    compress = false
    test(new Compressor(new BlockDeviceIOBusParams, compress)) { c =>
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      driver.pushTransactions(outGold)
      var cycle = 0
      val timeout = 2000
      while (cycle <= timeout && monitor.receivedTransactions.length < transactions.length) {
        c.clock.step()
        cycle += 1
      }
      if (cycle >= timeout)
        println("Test timed out!!!!!!!!!!!!!!!!!!!!!!!!")

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out.toList == transactions.toList)
    }
  }
}