package interconnect

import chisel3.Module
import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class TesterUtils extends FlatSpec with ChiselScalatestTester {

}

object TesterUtils {
  def runTest(c: Module,
              model: SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction],
              driver: CREECDriver,
              monitor: CREECMonitor,
              tx: Seq[CREECHighLevelTransaction],
              timeout: Int = 2000): Unit = {
    val outGold = model.processTransactions(tx)
    driver.pushTransactions(tx)
    var cycle = 0
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
}