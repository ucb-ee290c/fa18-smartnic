package interconnect

import chisel3.tester._

import org.scalatest.FlatSpec
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}

class CREECWidthConverterTest extends FlatSpec with ChiselScalatestTester {
  val busParamsBase = BusParams.creec
  val busParamsExpand2 = BusParams.aes

  val testTx = Seq(
    CREECHeaderBeat(len = 1, id = 0x0, addr = 0x0)(busParamsBase),
    CREECHeaderBeat(len = 1, id = 0x1, addr = 0x0)(busParamsBase),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), id = 0x0)(busParamsBase),
    CREECDataBeat(Seq(9, 10, 11, 12, 13, 14, 15, 16), id = 0x0)(busParamsBase),
    CREECDataBeat(Seq(1, 1, 1, 1, 1, 1, 1, 1), id = 0x1)(busParamsBase),
    CREECDataBeat(Seq(2, 2, 2, 2, 2, 2, 2, 2), id = 0x1)(busParamsBase)
  )

  val outGoldExpand2 = Seq(
    CREECHeaderBeat(len = 0, id = 0x0, addr = 0x0)(busParamsExpand2),
    CREECHeaderBeat(len = 0, id = 0x1, addr = 0x0)(busParamsExpand2),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), id = 0x0)(busParamsExpand2),
    CREECDataBeat(Seq(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2), id = 0x1)(busParamsExpand2)
  )

  val testTxUnaligned = Seq(
    CREECHeaderBeat(len = 2, id = 0x0, addr = 0x0)(busParamsBase),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), id = 0x0)(busParamsBase),
    CREECDataBeat(Seq(9, 10, 11, 12, 13, 14, 15, 16), id = 0x0)(busParamsBase),
    CREECDataBeat(Seq(17, 18, 19, 20, 21, 22, 23, 24), id = 0x0)(busParamsBase),
  )

  val outGoldExpand2Unaligned = Seq(
    CREECHeaderBeat(len = 1, id = 0x0, addr = 0x0, encryptionPadBytes = 8)(busParamsExpand2),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), id = 0x0)(busParamsExpand2),
    CREECDataBeat(Seq(17, 18, 19, 20, 21, 22, 23, 24, 0, 0, 0, 0, 0, 0, 0, 0), id = 0x0)(busParamsExpand2),
  )

  behavior of "SW Width Converter"
  it should "pass through transactions when ratio = 1" in {
    val model = new CREECWidthConverterModel(busParamsBase, busParamsBase)
    val out = model.processTransactions(testTx)
    assert(out == testTx)
  }

  it should "expand by a factor of 2 when ratio = 2" in {
    val model = new CREECWidthConverterModel(busParamsBase, busParamsExpand2)
    val out = model.processTransactions(testTx)
    assert(out == outGoldExpand2)
  }

  it should "error out on unaligned inputs when ratio = 2" in {
    val model = new CREECWidthConverterModel(busParamsBase, busParamsExpand2)
    assertThrows[AssertionError] {
      model.processTransactions(testTxUnaligned)
    }
  }

  it should "contract by a factor of 2 when ratio = 2" in {
    val model = new CREECWidthConverterModel(busParamsExpand2, busParamsBase)
    val out = model.processTransactions(outGoldExpand2)
    println(out)
    assert(out == testTx)
  }

  // Convert test data to High-level format for RTL testing
  val modelBase = new CREECLowToHighModel(busParamsBase)
  val testTxHigh = modelBase.processTransactions(testTx)
  val modelExpand2 = new CREECLowToHighModel(busParamsExpand2)
  val outGoldExpand2High = modelExpand2.processTransactions(outGoldExpand2)

  behavior of "CREECWidthConverter"
  it should "pass through transactions when ratio = 1" in {
    test(new CREECWidthConverter(busParamsBase, busParamsBase)) { c =>
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(testTxHigh)
      var cycle = 0
      val timeout = 2000
      while (cycle < timeout &&
             monitor.receivedTransactions.length < testTxHigh.length) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == testTxHigh)
    }
  }

  it should "expand by a factor of 2 when ratio = 2" in {
    test(new CREECWidthConverter(busParamsBase, busParamsExpand2)) { c =>
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(testTxHigh)
      var cycle = 0
      val timeout = 2000
      while (cycle < timeout &&
             monitor.receivedTransactions.length < outGoldExpand2High.length) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == outGoldExpand2High)
    }
  }

  it should "contract by a factor of 2 when ratio = 2" in {
    test(new CREECWidthConverter(busParamsExpand2, busParamsBase)) { c =>
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(outGoldExpand2High)
      var cycle = 0
      val timeout = 2000
      while (cycle < timeout &&
             monitor.receivedTransactions.length < testTxHigh.length) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      println(out)
      assert(out == testTxHigh)
    }
  }


}
