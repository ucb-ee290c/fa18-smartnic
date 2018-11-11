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
/*
  "the High2Low model" should "translate correctly" in {
    val model = new CREECHighToLowModel
    model.pushTransactions(Seq(
      CREECHighLevelTransaction(Seq(
        1000, 2000, 3000, 4000
      ), 0x0),
      CREECHighLevelTransaction(Seq(
        1, 2
      ), 0x1000)
    ))
    println("LAUNCHING MODEL SIMULATION")
    while (!model.nothingToProcess) model.tick()
    val out = model.pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(4, 0, 0x0),
      CREECDataBeat(1000, 0), CREECDataBeat(2000, 0), CREECDataBeat(3000, 0), CREECDataBeat(4000, 0),
      CREECHeaderBeat(2, 0, 0x1000),
      CREECDataBeat(1, 0), CREECDataBeat(2, 0)
    )
    println("OUTPUT TRANSACTIONS PULLED")
    println(out)
    assert(outGold == out)
  }

  "the CREECPassthrough model" should "pass transactions through with +1 on data" in {
    val model = new CREECPassthroughModel
    model.pushTransactions(Seq(
      CREECHeaderBeat(4, 0, 0x0),
      CREECDataBeat(1000, 0),
      CREECDataBeat(2000, 0),
      CREECDataBeat(3000, 0),
      CREECDataBeat(4000, 0)
    ))
    println("LAUNCHING MODEL SIMULATION")
    while (!model.nothingToProcess) model.tick()
    val out = model.pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(4, 0, 0x0),
      CREECDataBeat(1001, 0), CREECDataBeat(2001, 0), CREECDataBeat(3001, 0), CREECDataBeat(4001, 0)
    )
    println("OUTPUT TRANSACTIONS PULLED")
    println(out)
    assert(outGold == out)
  }
*/
  "the High2Low and Passthrough models" should "compose" in {
    val high2LowModel = new CREECHighToLowModel
    val passthroughModel = new CREECPassthroughModel
    val composedModel: SoftwareModel[CREECHighLevelTransaction, CREECLowLevelTransaction] =
      high2LowModel.compose(passthroughModel)
    composedModel.pushTransactions(Seq(
      CREECHighLevelTransaction(Seq(
        1000, 2000, 3000, 4000
      ), 0x0)
    ))
    while (!composedModel.nothingToProcess) composedModel.tick()
    //composedModel.tick()
    val out = composedModel.pullTransactions()
    println(out)
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
      val model = new CREECPassthroughModel
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
