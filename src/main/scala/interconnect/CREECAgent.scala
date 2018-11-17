package interconnect

import chisel3._
import chisel3.tester.TestAdapters.ReadyValidSource
import chisel3.tester._

import scala.collection.mutable

object CREECAgent {
  // TODO: usability bug in testers... binding ReadyValidSource to c.io.master.header compiled even though the directionality is wrong
  class CREECDriver(x: CREECBus, clk: Clock) {
    val high2LowModel = new CREECHighToLowModel(x.p)
    val transactionsToDrive = mutable.Queue[CREECLowLevelTransaction]()
    val headerDriver = new ReadyValidSource(x.header, clk)
    val dataDriver = new ReadyValidSource(x.data, clk)

    def pushTransactions(ts: Seq[CREECHighLevelTransaction]): Unit = {
      high2LowModel.pushTransactions(ts)
      high2LowModel.advanceSimulation()
      val lowTransactions = high2LowModel.pullTransactions()
      transactionsToDrive.enqueue(lowTransactions:_*)
    }

    // Header driver thread
    fork {
      while (true) {
        timescope {
          // TODO: this looks like a hack
          val headerTx = transactionsToDrive.dequeueFirst {
            case _: CREECHeaderBeat => true
            case _ => false
          }.map(_.asInstanceOf[CREECHeaderBeat])
          headerTx.foreach { t =>
            headerDriver.enqueue(new TransactionHeader().Lit(t.len.U, t.id.U, false.B, false.B, false.B, t.addr.U))
          }
          clk.step()
        }
      }
    }

    // Data driver thread
    fork {
      while (true) {
        timescope {
          // TODO: this looks like a hack
          val dataTx = transactionsToDrive.dequeueFirst {
            case _: CREECDataBeat => true
            case _ => false
          }.map(_.asInstanceOf[CREECDataBeat])
          dataTx.foreach { t =>
            dataDriver.enqueue(new TransactionData().Lit(BigInt((0.asInstanceOf[Byte] +: t.data.reverse).toArray).U, t.id.U))
          }
          clk.step()
        }
      }
    }
  }

  class CREECMonitor(x: CREECBus, clk: Clock) {
    implicit val busParams = x.p
    val low2HighModel = new CREECLowToHighModel(x.p)
    val receivedTransactions = mutable.Queue[CREECHighLevelTransaction]()
    // Header monitor thread
    fork {
      while (true) {
        // TODO: why is this timescope needed here?
        timescope {
          x.header.ready.poke(true.B)
          while (!x.header.valid.peek().litToBoolean) {
            clk.step()
          }
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

    // Data monitor thread
    fork {
      while (true) {
        timescope {
          x.data.ready.poke(true.B)
          while (!x.data.valid.peek().litToBoolean) {
            clk.step()
          }
          // TODO: usability bug, if data.peek().litValue() is replaced with data.litValue(), you get a get None error
          // TODO: potential bug if data.toByteArray isn't padded appropriately (if 'smaller' than databus width)
          val data = x.data.bits.data.peek().litValue()
          val id = x.data.bits.id.peek().litValue().toInt
          println(data, id)
          // all peeked values are read as BigInt (MSB -> LSB byte format), so reverse is needed
          low2HighModel.pushTransactions(Seq(
            CREECDataBeat(data.toByteArray.reverse.padTo(busParams.bytesPerBeat, 0.asInstanceOf[Byte]), id)))
          low2HighModel.advanceSimulation()
          receivedTransactions.enqueue(low2HighModel.pullTransactions():_*)
          clk.step()
        }
      }
    }
  }
}
