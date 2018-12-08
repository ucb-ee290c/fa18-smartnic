package interconnect

import chisel3.tester.ChiselScalatestTester
import chisel3.tester._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class CREECPadderTest extends FlatSpec with ChiselScalatestTester {
  implicit val busParams: BusParams = BusParams.creec

  val testTxUnaligned = Seq(
    CREECHighLevelTransaction(Seq(
      1, 2, 3, 4, 5, 6, 7, 8,
      9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24,
      17, 18, 19, 20, 21, 22, 23, 24,
      17, 18, 19, 20, 21, 22, 23, 24
    ), addr = 0x0)
  )

  val outGoldAligned = Seq(
    CREECHighLevelTransaction(Seq(
      1, 2, 3, 4, 5, 6, 7, 8,
      9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24,
      17, 18, 19, 20, 21, 22, 23, 24,
      17, 18, 19, 20, 21, 22, 23, 24,
      0, 0, 0, 0, 0, 0, 0, 0
    ), addr = 0x0, encryptionPadBytes = 8)
  )

  behavior of "CREECPadder SW Model"
  it should "add zero padding data beats to unaligned transactions" in {
    val model = new CREECPadderModel(16)
    val out = model.processTransactions(testTxUnaligned)
    assert(out == outGoldAligned)
  }

  behavior of "CREECPadder RTL"
  it should "add zero padding data beats to unaligned transactions" in {
    test(new CREECPadder(busParams, 16)) { c =>
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)

      driver.pushTransactions(testTxUnaligned)
      var cycle = 0
      val timeout = 1000
      while (cycle < timeout) {
        c.clock.step()
        cycle += 1
      }

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == outGoldAligned)
    }
  }
}
