package interconnect

import chisel3._
import chisel3.tester.TestAdapters.ReadyValidSource
import chisel3.tester._

import scala.collection.mutable

object CREECAgent {
  /**
    * Converts a Seq[Byte] in [LS-byte, ..., MS-byte] format to an unsigned BitInt
    * @param arr The byte array to convert
    * @return An BigInt constructed assuming the Seq[Byte] is interpreted as unsigned
    */
  def bytesToBigInt(arr: Seq[Byte]): BigInt = {
    // BigInt decodes byte arrays as big-endian (MSB -> LSB)
    //  so, prepend a 0 byte on the MSB side to make sure arr is decoded as unsigned
    // Note: BigInt(Array(0, 0, 1)) = 1
    // Note: BigInt(Array(0, 1, 1)) = 257
    // Note: BigInt(Array(255, 255, 255)) = -1 (not what we wanted)
    // Note: BigInt(Array(0, 255, 255, 255)) = 16777215 (this is better)
    BigInt((0.asInstanceOf[Byte] +: arr.reverse).toArray)
  }

  /**
    * Converts a BigInt to a byte array in [LS-byte, ..., MS-byte] format
    * @param bint The BigInt to convert
    * @param numBytes The returned byte array will contain this many bytes
    * @return
    */
  def bigIntToBytes(bint: BigInt, numBytes: Int = 8): Seq[Byte] = {
    assert(bint <= BigInt(2).pow(numBytes*8) - 1, s"Can't construct byte array of length $numBytes from BigInt $bint")
    // BigInt.toByteArray returns a byteArray in MS-byte -> LS-byte format, so reverse is needed
    bint.toByteArray.reverse.padTo(numBytes, 0.asInstanceOf[Byte]).slice(0, numBytes)
  }

  // TODO: usability bug in testers... binding ReadyValidSource to c.io.master.header compiled even though the directionality is wrong
  class CREECDriver(x: CREECBus, clk: Clock) {
    val high2LowModel = new CREECHighToLowModel(x.p)
    val headersToDrive = mutable.Queue[CREECHeaderBeat]()
    val dataToDrive = mutable.Queue[CREECDataBeat]()
    val headerDriver = new ReadyValidSource(x.header, clk)
    // We need a way to sequence data beats to only appear 1 or more cycles after the respective header beat
    val inFlight = mutable.Map[Int, CREECHeaderBeat]()

    def pushTransactions(ts: Seq[CREECHighLevelTransaction]): Unit = {
      val lowTransactions = high2LowModel.pushTransactions(ts).advanceSimulation().pullTransactions()
      lowTransactions.foreach {
        case t: CREECHeaderBeat => headersToDrive.enqueue(t)
        case t: CREECDataBeat => dataToDrive.enqueue(t)
      }
    }

    // Header driver thread
    fork {
      while (true) {
        timescope {
          val header = headersToDrive.dequeueFirst(_ => true)
          header.foreach { t =>
            headerDriver.enqueue(new TransactionHeader().Lit(
              t.len.U, t.id.U, t.addr.U,
              t.compressed.B, t.encrypted.B, t.ecc.B,
              t.compressionPadBytes.U, t.eccPadBytes.U, t.encryptionPadBytes.U))
            inFlight.update(t.id, t)
          }
          clk.step()
        }
      }
    }

    // Data driver thread
    fork {
      timescope {
        x.data.valid.poke(false.B)
        x.data.bits.poke(new TransactionData().Lit(0.U, 0.U))
        while (true) {
          val data = dataToDrive.dequeueFirst(t => inFlight.contains(t.id))
          data.foreach { t =>
            timescope {
              x.data.bits.poke(new TransactionData().Lit(bytesToBigInt(t.data).U, t.id.U))
              x.data.valid.poke(true.B)
              while (!x.data.ready.peek().litToBoolean) {
                clk.step()
              }
              clk.step()
            }
            // Update the inflight transaction and remove it from inFlight if it is done now
            val header = inFlight(t.id)
            if (header.len == 0) {
              inFlight.remove(t.id)
            } else {
              inFlight.update(t.id, CREECHeaderBeat(header.len - 1, header.id, header.addr)(x.p))
            }
          }
          if (data.isEmpty) {
            timescope {
              clk.step(1)
            }
          }
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
          val receivedHeader = x.header.bits
          low2HighModel.pushTransactions(Seq(CREECHeaderBeat(
            len = receivedHeader.len.peek().litValue().toInt,
            id = receivedHeader.id.peek().litValue().toInt,
            addr = receivedHeader.addr.peek().litValue(),
            compressed = receivedHeader.compressed.peek().litToBoolean,
            encrypted = receivedHeader.encrypted.peek().litToBoolean,
            ecc = receivedHeader.ecc.peek().litToBoolean,
            compressionPadBytes = receivedHeader.compressionPadBytes.peek().litValue().toInt,
            eccPadBytes = receivedHeader.eccPadBytes.peek().litValue().toInt,
            encryptionPadBytes = receivedHeader.encryptionPadBytes.peek().litValue().toInt
          )))
          low2HighModel.advanceSimulation()
          receivedTransactions.enqueue(low2HighModel.pullTransactions():_*)
          clk.step()
        }
      }
    }

    // Data monitor thread
    fork {
      timescope {
        x.data.ready.poke(true.B)
        while (true) {
          while (!x.data.valid.peek().litToBoolean) {
            clk.step()
          }
          // TODO: usability bug, if data.peek().litValue() is replaced with data.litValue(), you get a get None error
          val data = x.data.bits.data.peek().litValue()
          val id = x.data.bits.id.peek().litValue().toInt
          low2HighModel.pushTransactions(Seq(
            CREECDataBeat(bigIntToBytes(data, x.p.bytesPerBeat), id)))
          low2HighModel.advanceSimulation()
          receivedTransactions.enqueue(low2HighModel.pullTransactions():_*)
          clk.step()
        }
      }
    }
  }
}
