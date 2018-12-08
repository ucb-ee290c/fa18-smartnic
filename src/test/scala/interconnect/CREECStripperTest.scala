package interconnect

import chisel3.tester.ChiselScalatestTester
import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class CREECStripperTest extends FlatSpec with ChiselScalatestTester {
  implicit val busParams: BusParams = BusParams.creec

  val testTxAligned = Seq(
    CREECHighLevelTransaction(Seq(
      1, 2, 3, 4, 5, 6, 7, 8,
      9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24,
      17, 18, 19, 20, 21, 22, 23, 24,
      17, 18, 19, 20, 21, 22, 23, 24,
      0, 0, 0, 0, 0, 0, 0, 0
    ), addr = 0x0, encryptionPadBytes = 8)
  )

  val outGoldUnaligned = Seq(
    CREECHighLevelTransaction(Seq(
      1, 2, 3, 4, 5, 6, 7, 8,
      9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24,
      17, 18, 19, 20, 21, 22, 23, 24,
      17, 18, 19, 20, 21, 22, 23, 24
    ), addr = 0x0)
  )

  behavior of "CREECStripper SW Model"
  it should "add zero padding data beats to unaligned transactions" in {
    val model = new CREECStripperModel
    val out = model.processTransactions(testTxAligned)
    assert(out == outGoldUnaligned)
  }

  behavior of "CREECStripper RTL"
  it should "add zero padding data beats to unaligned transactions" in {
    test(new CREECStripper(busParams)) { c =>
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)

      driver.pushTransactions(testTxAligned)
      var cycle = 0
      val timeout = 1000
      while (cycle < timeout) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == outGoldUnaligned)
    }
  }
}
