package compression

import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import interconnect._
import org.scalatest.FlatSpec

class CREECCompressionModuleTester extends FlatSpec with ChiselScalatestTester {
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
    CREECHighLevelTransaction(
      (Seq(3, 4, 5) ++ (0 until 308).map(_ => 0) ++ Seq(7)).map(_.toByte), 0x69
    ),
    CREECHighLevelTransaction(
      Seq(
        0, 2, 0, 2, 0, 2, 0, 2,
        0, 2, 0, 2, 0, 2, 0, 2,
        0, 2, 0, 2, 0, 2, 0, 2,
        0, 2, 0, 2, 0, 2, 0, 2,
        0, 2, 0, 2, 0, 2, 0, 2,
        2, 0, 2, 0, 2, 0, 2, 0,
        2, 0, 2, 0, 2, 0, 2, 0,
        2, 0, 2, 0, 2, 0, 2, 0,
        2, 0, 2, 0, 2, 0, 2, 0,
        2, 0, 2, 0, 2, 0, 3, 2
      ),
      0x515
    )
  )



  "the CREECDifferentialCoder module" should "encode and decode data" in {
    for (encode <- List(true, false)) {
      test(new CREECDifferentialCoder(coderParams = CoderParams(encode = encode), BusParams.creec)) { c =>
        val model = new CREECDifferentialCoderModel(encode = encode)
        val driver = new CREECDriver(c.io.in, c.clock)
        val monitor = new CREECMonitor(c.io.out, c.clock)
        TesterUtils.runTest(c, model, driver, monitor, transactions)
      }
    }
  }

  "the CREECRunLengthCoder module" should "encode and decode data" in {
    for (encode <- List(true)) {
      test(new CREECRunLengthCoder(coderParams = CoderParams(encode = encode), BusParams.creec)) { c =>
        val model = new CREECRunLengthCoderModel(encode = encode)
        val driver = new CREECDriver(c.io.in, c.clock)
        val monitor = new CREECMonitor(c.io.out, c.clock)
        TesterUtils.runTest(c, model, driver, monitor, transactions)
      }
    }
  }

  "the Compressor module" should "compress" in {
    test(new Compressor(BusParams.blockDev, true)) { c =>
      val model = new CompressorModel(compress = true)
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      TesterUtils.runTest(c, model, driver, monitor, transactions)
    }
  }

  "the Cmpressor module" should "uncompress" in {
    val m = new CompressorModel(compress = true)
    val compressedTransactions = m.processTransactions(transactions)
    test(new Compressor(BusParams.blockDev, false)) { c =>
      val model = new CompressorModel(compress = false)
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      TesterUtils.runTest(c, model, driver, monitor, compressedTransactions)
    }
  }
}