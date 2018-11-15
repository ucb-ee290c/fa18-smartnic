package interconnect
import chisel3.iotesters.{ChiselFlatSpec, Driver}
import compression.{DifferentialCoder, DifferentialCoderTester}
import utest._

class CREECPassthroughTest extends ChiselFlatSpec {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  "the High2Low model" should "translate correctly" in {
    implicit val busParams: BusParams = new CREECBusParams
    val model = new CREECHighToLowModel(busParams)
    model.pushTransactions(Seq(
      CREECHighLevelTransaction(Seq(
        12, 13, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 26, 27
      ), 0x0),
      CREECHighLevelTransaction(Seq(
        1, 2, 3, 4, 5, 6, 7, 8
      ), 0x1000)
    ))
    println("LAUNCHING MODEL SIMULATION")
    model.advanceSimulation()
    val out = model.pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(1, 0, 0x0),
      CREECDataBeat(Seq(12, 13, 14, 15, 16, 17, 18, 19), 0),
      CREECDataBeat(Seq(20, 21, 22, 23, 24, 25, 26, 27), 0),
      CREECHeaderBeat(0, 0, 0x1000),
      CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), 0)
    )
    println("OUTPUT TRANSACTIONS PULLED")
    println(out)
    assert(outGold == out)
  }

  "the CREECPassthrough model" should "pass transactions through with +1 on data" in {
    implicit val busParams: BusParams = new CREECBusParams
    val model = new CREECPassthroughModel(busParams)
    model.pushTransactions(Seq(
      CREECHeaderBeat(0, 0, 0x0),
      CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), 0),
      CREECDataBeat(Seq(255, 255, 255, 255, 255, 255, 255, 255).map(_.asInstanceOf[Byte]), 0),  // test overflow
      CREECDataBeat(Seq(0, 0, 0, 0, 0, 0, 0, 1).map(_.asInstanceOf[Byte]), 0)   // test small data
    ))
    println("LAUNCHING MODEL SIMULATION")
    model.advanceSimulation()
    val out = model.pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(0, 0, 0x0),
      CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 9), 0),
      CREECDataBeat(Seq(0, 0, 0, 0, 0, 0, 0, 0), 0),
      CREECDataBeat(Seq(0, 0, 0, 0, 0, 0, 0, 2), 0)
    )
    println("OUTPUT TRANSACTIONS PULLED")
    println(out)
    assert(outGold == out)
  }

  "multiple High2Low and Passthrough models" should "compose" in {
    implicit val busParams: BusParams = new CREECBusParams
    val composedModel =
      new CREECHighToLowModel(busParams)
        .compose(new CREECPassthroughModel(busParams))
        .compose(new CREECPassthroughModel(busParams))
        .compose(new CREECPassthroughModel(busParams))

    composedModel.pushTransactions(Seq(
      CREECHighLevelTransaction(Seq(
        1, 2, 3, 4, 5, 6, 7, 8
      ), 0x1000)
    ))
    println("LAUNCHING MODEL SIMULATION")
    composedModel.advanceSimulation()
    val out = composedModel.pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(0, 0, 0x1000),
      CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 11), 0)
    )
    println("OUTPUT TRANSACTIONS PULLED")
    println(out)
    assert(outGold == out)
  }
}

// Example of using uTest
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
