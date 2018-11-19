package interconnect

import chisel3._
import chisel3.tester.TestAdapters.ReadyValidSource
import chisel3.tester._

import scala.collection.mutable

object CREECAgent {
  // TODO: usability bug in testers... binding ReadyValidSource to c.io.master.header compiled even though the directionality is wrong
  class CREECDriver(x: CREECBus, clk: Clock) {
    val high2LowModel = new CREECHighToLowModel(x.p)
    // TODO: this queue is being read from 2 threads, it should be synchronized (message passing looks like a much better idea now)
      // but testers2 prevents multiple threads executing simultaneously so data races aren't a problem, but off-by-one-clock queue latency is
    val transactionsToDrive = mutable.Queue[CREECLowLevelTransaction]()
    val headerDriver = new ReadyValidSource(x.header, clk)
    val dataDriver = new ReadyValidSource(x.data, clk)
    // We need a way to sequence data beats to only appear 1 or more cycles after the respective header beat
    val inFlight = mutable.Map[Int, CREECHeaderBeat]()

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
            inFlight.update(t.id, t)
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
            // Only fetch a data beat whose header has already been accepted by the slave
            case t: CREECDataBeat => inFlight.contains(t.id)
            case _ => false
          }.map(_.asInstanceOf[CREECDataBeat])
          dataTx.foreach { t =>
            dataDriver.enqueue(new TransactionData().Lit(BigInt((0.asInstanceOf[Byte] +: t.data.reverse).toArray).U, t.id.U))
            // Update the inflight transaction and remove it from inFlight if it is done now
            val header = inFlight(t.id)
            if (header.len == 0) {
              inFlight.remove(t.id)
            } else {
              inFlight.update(t.id, CREECHeaderBeat(header.len - 1, header.id, header.addr)(x.p))
            }
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
          val data = x.data.bits.data.peek().litValue()
          val id = x.data.bits.id.peek().litValue().toInt
          println(data, id)
          // all peeked values are read as BigInt (MSB -> LSB byte format), so reverse is needed
          // also, since data is unsigned, additional unwanted zero
          // may be generated for the sign
          low2HighModel.pushTransactions(Seq(
            CREECDataBeat(data.toByteArray.reverse.padTo(busParams.bytesPerBeat, 0.asInstanceOf[Byte]).slice(0, busParams.bytesPerBeat), id)))
          low2HighModel.advanceSimulation()
          receivedTransactions.enqueue(low2HighModel.pullTransactions():_*)
          clk.step()
        }
      }
    }
  }
}
