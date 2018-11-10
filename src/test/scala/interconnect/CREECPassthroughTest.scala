package interconnect
import chisel3.iotesters.Driver
import compression.{DifferentialCoder, DifferentialCoderTester}
import utest._

object CREECPassthroughTest extends TestSuite {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  val tests = Tests {
    'high2Low - {
      val model = new CREECHighToLowModel
      model.pushTransactions(Seq(
        CREECHighLevelTransaction(Seq(
          1000, 2000, 3000, 4000, 5000, 6000
        ), 0x0),
        CREECHighLevelTransaction(Seq(
          1, 2, 3, 4, 5, 6
        ), 0x1000),
      ))
      while (!model.nothingToProcess) model.tick()
      val out = model.pullTransactions()

      out.foreach { _ =>
        print(_)
      }
    }
    /*
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
    */
  }
}
