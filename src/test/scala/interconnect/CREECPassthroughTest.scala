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
