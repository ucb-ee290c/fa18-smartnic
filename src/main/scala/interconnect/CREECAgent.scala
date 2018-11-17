package interconnect

import chisel3._
import chisel3.tester.TestAdapters.{ReadyValidSink, ReadyValidSource}
import chisel3.tester._

import scala.collection.mutable

// TODO get rid of this boilerplate
import chisel3.internal.firrtl.{LitArg, ULit, SLit}

package object CREECAgent {
  class CREECDriver(x: CREECBus, clk: Clock) {
    val high2LowModel = new CREECHighToLowModel(x.p)
    // TODO: fork off internal threads to process the data from pushTransactions() (which should be asynchronous)
    //val transactionsToDrive = mutable.Queue[CREECLowLevelTransaction]()
    val headerDriver = new ReadyValidSource(x.header, clk)
    val dataDriver = new ReadyValidSource(x.data, clk)

    def pushTransactions(ts: Seq[CREECHighLevelTransaction]): Unit = {
      high2LowModel.pushTransactions(ts)
      high2LowModel.advanceSimulation()
      val lowTransactions = high2LowModel.pullTransactions()
      lowTransactions.foreach { t =>
        t match {
          case t: CREECHeaderBeat => headerDriver.enqueue(new TransactionHeader().Lit(t.len.U, t.id.U, false.B, false.B, false.B, t.addr.U))
          case t: CREECDataBeat => dataDriver.enqueue(new TransactionData().Lit(BigInt((0.asInstanceOf[Byte] +: t.data.reverse).toArray).U, t.id.U))
        }
      }
    }
  }

  class CREECMonitor(x: CREECBus, clk: Clock) {
    implicit val busParams = x.p
    val low2HighModel = new CREECLowToHighModel(x.p)
    val receivedTransactions = mutable.Queue[CREECHighLevelTransaction]()
    val headerMonitor = new ReadyValidSink(x.header, clk)
    val dataMonitor = new ReadyValidSink(x.data, clk)
    fork {
      while (true) {
        timescope {
          x.header.ready.poke(true.B)
          // Header monitor thread
          headerMonitor.waitForValid()
          // TODO: integrate monitor-like functions in TestAdapters
          x.header.ready.poke(true.B)
          val len = x.header.bits.len.peek().litValue().toInt
          val id = x.header.bits.id.peek().litValue().toInt
          val addr = x.header.bits.addr.peek().litValue()
          println(len, id, addr)
          low2HighModel.pushTransactions(Seq(CREECHeaderBeat(len, id, addr)))
          low2HighModel.advanceSimulation()
          receivedTransactions.enqueue(low2HighModel.pullTransactions():_*)
          clk.step()
        }
      }
    }
    fork {
      while (true) {
        // TODO: why is this timescope needed here?
        timescope {
          x.data.ready.poke(true.B)
          // Data monitor thread
          dataMonitor.waitForValid()
          // TODO: integrate monitor-like functions in TestAdapters
          x.data.ready.poke(true.B)
          // TODO: usability bug, if data.peek().litValue() is replaced with
            // data.litValue(), you get a weird looking error message
          val data = x.data.bits.data.peek().litValue()
          val id = x.data.bits.id.peek().litValue().toInt
          println(data, id)
          low2HighModel.pushTransactions(Seq(CREECDataBeat(data.toByteArray, id)))
          low2HighModel.advanceSimulation()
          receivedTransactions.enqueue(low2HighModel.pullTransactions():_*)
          clk.step()
        }
      }
    }
  }
}
