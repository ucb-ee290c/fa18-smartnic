package interconnect
import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class CREECPassthroughTest extends FlatSpec with ChiselScalatestTester {
  val busParams = new CREECBusParams

  // Test high-level transaction (unified stimulus)
  val highTx = Seq(CREECHighLevelTransaction(Seq(
    1, 2, 3, 4, 5, 6, 7, 8,
    0, 0, 0, 0, 0, 0, 0, 1, // test MSB data
    1, 0, 0, 0, 0, 0, 0, 0, // test LSB data
    0, 0, 0, 0, 0, 0, 0, 0,
    0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8,
    255, 255, 255, 255, 255, 255, 255, 255, // test overflow
    255, 0, 0, 0, 0, 0, 0, 0, // test byte-level overflow
    0, 0, 0, 0, 0, 0, 0, 255
  ).map(_.asInstanceOf[Byte]), 0x1000))

  // Expected high-level output transaction (+1 on LSB of data) (assuming 64-bit dataWidth bus)
  val highTxOutGold = Seq(CREECHighLevelTransaction(Seq(
    2, 2, 3, 4, 5, 6, 7, 8,
    1, 0, 0, 0, 0, 0, 0, 1,
    2, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0,
    0xa2, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 255
  ).map(_.asInstanceOf[Byte]), 0x1000))

  // Expected high-level output transaction (+3 on LSB of data) (3 chained passthrough modules)
  val highTxChainedPassthroughsOutGold = Seq(CREECHighLevelTransaction(Seq(
    4, 2, 3, 4, 5, 6, 7, 8,
    3, 0, 0, 0, 0, 0, 0, 1,
    4, 0, 0, 0, 0, 0, 0, 0,
    3, 0, 0, 0, 0, 0, 0, 0,
    0xa4, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8,
    2, 0, 0, 0, 0, 0, 0, 0,
    2, 1, 0, 0, 0, 0, 0, 0,
    3, 0, 0, 0, 0, 0, 0, 255
  ).map(_.asInstanceOf[Byte]), 0x1000))

  // First test the software model with hand written stimulus and expected output
  "the CREECPassthrough SW model" should "pass transactions through with +1 on data" in {
    val model =
      new CREECHighToLowModel(busParams) ->
      new CREECPassthroughModel(busParams) ->
      new CREECLowToHighModel(busParams)
    val out = model.processTransactions(highTx)
    assert(out == highTxOutGold)
  }

  "multiple CREECPassthrough SW models" should "compose together with +3 on data" in {
    val model =
      new CREECHighToLowModel(busParams) ->
      new CREECPassthroughModel(busParams) ->
      new CREECPassthroughModel(busParams) ->
      new CREECPassthroughModel(busParams) ->
      new CREECLowToHighModel(busParams)
    val out = model.processTransactions(highTx)
    assert(out == highTxChainedPassthroughsOutGold)
  }

  // Then use the software model to generate golden output for the RTL model
  "the CREECPassthrough RTL model" should "behave identically to the SW model" in {
    // Software golden model
    val model =
      new CREECHighToLowModel(busParams) ->
      new CREECPassthroughModel(busParams) ->
      new CREECLowToHighModel(busParams)
    val outGold = model.processTransactions(highTx)

    // RTL model
    test(new CREECPassthrough(busParams)) { c =>
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(highTx)

      // Time advancement (once 100 cycles are up, this main thread will exit, and all threads spawned in the driver
      // and monitor will be killed)
      // c.clock.step(100)

      // More intelligent technique
      var cycle = 0
      val timeout = 20
      while (cycle < timeout && monitor.receivedTransactions.length < outGold.length) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      // Compare the software golden output transaction to the RTL output transaction
      assert(out == outGold)
    }
  }
}

// Example of using uTest
/*
object CREECPassthroughTest extends TestSuite {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  val tests = Tests {
    'produceOutput - {
      val model = new CREECPassthroughModel(new CREECBusParams)
      println(model)
    }
    'failForFun - {
      assert(1 == 0)
    }
    'passForFun - {
      assert(1 == 1)
      assert("asdf".length == 4)
    }
    'compressionTest - {
      assert(Driver.execute(testerArgs :+ "differential_encoder", () => new DifferentialCoder) {
        c => new DifferentialCoderTester(c, true)
      })
    }
  }
}
*/
