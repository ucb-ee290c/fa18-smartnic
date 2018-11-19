package interconnect
import chisel3._
import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class CREECPassthroughTest extends FlatSpec with ChiselScalatestTester {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  "the High2Low model" should "translate correctly" in {
    implicit val busParams: BusParams = new CREECBusParams
    val model = new CREECHighToLowModel(busParams)
    val out = model.pushTransactions(Seq(
      CREECHighLevelTransaction(Seq(
        12, 13, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 26, 27
      ), 0x0),
      CREECHighLevelTransaction(Seq(
        1, 2, 3, 4, 5, 6, 7, 8
      ), 0x1000)
    )).advanceSimulation(true).pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(1, 0, 0x0),
      CREECDataBeat(Seq(12, 13, 14, 15, 16, 17, 18, 19), 0),
      CREECDataBeat(Seq(20, 21, 22, 23, 24, 25, 26, 27), 0),
      CREECHeaderBeat(0, 0, 0x1000),
      CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), 0)
    )
    assert(outGold == out)
  }

  "the CREECPassthrough model" should "pass transactions through with +1 on data" in {
    implicit val busParams: BusParams = new CREECBusParams
    val model = new CREECPassthroughModel(busParams)
    val out = model.pushTransactions(Seq(
      CREECHeaderBeat(3, 0, 0x0),
      CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), 0),
      CREECDataBeat(Seq(255, 255, 255, 255, 255, 255, 255, 255).map(_.asInstanceOf[Byte]), 0),  // test overflow
      CREECDataBeat(Seq(0, 0, 0, 0, 0, 0, 0, 1).map(_.asInstanceOf[Byte]), 0),  // test MSB data
      CREECDataBeat(Seq(1, 0, 0, 0, 0, 0, 0, 0).map(_.asInstanceOf[Byte]), 0)   // test small data
    )).advanceSimulation(true).pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(3, 0, 0x0),
      CREECDataBeat(Seq(2, 2, 3, 4, 5, 6, 7, 8), 0),
      CREECDataBeat(Seq(0, 0, 0, 0, 0, 0, 0, 0), 0),
      CREECDataBeat(Seq(1, 0, 0, 0, 0, 0, 0, 1), 0),
      CREECDataBeat(Seq(2, 0, 0, 0, 0, 0, 0, 0), 0)
    )
    assert(outGold == out)
  }

  "multiple High2Low and Passthrough models" should "compose" in {
    implicit val busParams: BusParams = new CREECBusParams
    val composedModel =
      new CREECHighToLowModel(busParams) ->
        new CREECPassthroughModel(busParams) ->
          new CREECPassthroughModel(busParams) ->
            new CREECPassthroughModel(busParams)

    val out = composedModel.pushTransactions(Seq(
      CREECHighLevelTransaction(Seq(
        1, 2, 3, 4, 5, 6, 7, 8
      ), 0x1000)
    )).advanceSimulation(true).pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(0, 0, 0x1000),
      CREECDataBeat(Seq(4, 2, 3, 4, 5, 6, 7, 8), 0)
    )
    assert(outGold == out)
  }

  "the High2Low -> passthrough -> Low2High chain" should "work together" in {
    implicit val busParams: BusParams = new CREECBusParams
    val composedModel =
      new CREECHighToLowModel(busParams) ->
        new CREECPassthroughModel(busParams) ->
          new CREECLowToHighModel(busParams)
    val out = composedModel.pushTransactions(Seq(
      CREECHighLevelTransaction(Seq(
        1, 2, 3, 4, 5, 6, 7, 8,
        10, 11, 12, 13, 14, 15, 16, 17
      ), 0x1000)
    )).advanceSimulation(true).pullTransactions()
    val outGold = Seq(
      CREECHighLevelTransaction(Seq(
        2, 2, 3, 4, 5, 6, 7, 8,
        11, 11, 12, 13, 14, 15, 16, 17
      ), 0x1000)
    )
    assert(outGold == out)
  }

  "the passthrough module" should "be testable with testers2" in {
    val busParams = new CREECBusParams

    // Unified stimulus
    val tx = CREECHighLevelTransaction(Seq(
      1, 2, 3, 4, 5, 6, 7, 8,
      255, 255, 255, 255, 255, 255, 255, 255,
      0, 0, 0, 0, 0, 0, 0, 1,
      1, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 255
    ).map(_.asInstanceOf[Byte]), 0x1000)

    // Software golden model
    val model =
      new CREECHighToLowModel(busParams) ->
        new CREECPassthroughModel(busParams) ->
          new CREECLowToHighModel(busParams)
    val outGold = model.pushTransactions(Seq(tx)).advanceSimulation(true).pullTransactions()

    // RTL model
    test(new CREECPassthrough(busParams)) { c =>
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(Seq(tx))

      // Time advancement thread
      fork {
        c.clock.step(100)
      }.join() // join enforces that this thread must complete

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
