package compression

import chisel3.Module
import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import interconnect.{BlockDeviceIOBusParams, CREECBusParams, CREECHighLevelTransaction, SoftwareModel}
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

  def runTest(c: Module,
              model: SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction],
              driver: CREECDriver,
              monitor: CREECMonitor,
              tx: Seq[CREECHighLevelTransaction]): Unit = {
    val outGold = model.processTransactions(tx)
    driver.pushTransactions(tx)
    var cycle = 0
    val timeout = 2000
    while (cycle <= timeout && monitor.receivedTransactions.length < outGold.length) {
      c.clock.step()
      cycle += 1
    }
    if (cycle >= timeout)
      println("Test timed out!")
    val out = monitor.receivedTransactions.dequeueAll(_ => true)
    assert(out == outGold)
    // TODO: (testers2 usability) assert kills everything and prevents vcd generation
  }

  "the CREECDifferentialCoder module" should "encode and decode data" in {
    for (encode <- List(true, false)) {
      test(new CREECDifferentialCoder(coderParams = CoderParams(encode = encode))) { c =>
        val model = new CREECDifferentialCoderModel(encode = encode)
        val driver = new CREECDriver(c.io.in, c.clock)
        val monitor = new CREECMonitor(c.io.out, c.clock)
        runTest(c, model, driver, monitor, transactions)
      }
    }
  }

  "the CREECRunLengthCoder module" should "encode and decode data" in {
    for (encode <- List(true, false)) {
      test(new CREECRunLengthCoder(coderParams = CoderParams(encode = encode))) { c =>
        val model = new CREECRunLengthCoderModel(encode = encode)
        val driver = new CREECDriver(c.io.in, c.clock)
        val monitor = new CREECMonitor(c.io.out, c.clock)
        runTest(c, model, driver, monitor, transactions)
      }
    }
  }

  "the Compressor module" should "compress" in {
    test(new Compressor(new BlockDeviceIOBusParams, true)) { c =>
      val model = new CompressorModel(compress = true)
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      runTest(c, model, driver, monitor, transactions)
    }
  }

  "the Cmpressor module" should "uncompress" in {
    val m = new CompressorModel(compress = true)
    val compressedTransactions = m.processTransactions(transactions)
    test(new Compressor(new BlockDeviceIOBusParams, false)) { c =>
      val model = new CompressorModel(compress = false)
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      runTest(c, model, driver, monitor, compressedTransactions)
    }
  }
}