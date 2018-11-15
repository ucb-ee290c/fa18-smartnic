package interconnect

import scala.collection.mutable
import java.nio._

/**
  * A Transaction is an abstract notion of a chunk of data and control that a block can process at a time
  */
trait Transaction

abstract class CREECTransaction extends Transaction

/**
  * A HighLevelTransaction represents a full sector write request or read response with all
  * the control and data bundled together. It is generic to any CREECBus parameterization.
  */
case class CREECHighLevelTransaction(data: Seq[Byte], addr: Long) extends CREECTransaction {
  assert(data.length % 8 == 0, "CREEC high level transaction must have data with length = data bus width (multiple of 8B) * numBeats")

  // TODO: Print bytes as unsigned
  override def toString: String = super.toString
    //s"CREECHighLevelTransaction("
  //}
}

/**
  * A HighLevelTransaction can be decomposed into 1 CREECHeaderBeat and multiple CREECDataBeats.
  * These LowLevelTransactions are used to drive the physical wires of the CREECBus.
  * LowLevelTransactions are CREECBus parameter specific.
  */
abstract class CREECLowLevelTransaction extends CREECTransaction

case class CREECHeaderBeat(len: Int, id: Int, addr: BigInt)(implicit p: BusParams) extends CREECLowLevelTransaction {
  require(len <= p.maxBeats)
  require(id <= p.maxInFlight)
}
case class CREECDataBeat(data: Seq[Byte], id: Int)(implicit p: BusParams) extends CREECLowLevelTransaction {
  require(id <= p.maxBeats)
  // data.length = 64 bits, 128 bits, 256 bits, etc... = data width of CREECBus
  require((data.length * 8) == p.dataWidth)
}

// Can define custom transactions below the CREECBus level for each block's testing and
// software model in isolation of the bus integration. Coming soon...
//case class ECCEncodeCommand(message: Int) extends Transaction
//case class ECCEncodedData() extends Transaction

/**
  * An abstract software model for a DUT which pulls a input transaction and produces output transactions on each tick.
  * At any tick, there may or may not be an input transaction to process. Several output transactions can be written each tick.
  * A concrete model should implement process().
  * @tparam I type of input transactions
  * @tparam O type of output transactions
  */
// TODO: this whole API should be based on streams with a synchronization API, not on ticks and processing
// TODO: but this requires we first go through the struggle with this API and learn
abstract class SoftwareModel[I <: Transaction, O <: Transaction] { self =>
  val inputQueue: mutable.Queue[I] = mutable.Queue[I]()
  val outputQueue: mutable.Queue[O] = mutable.Queue[O]()
  val childModels: mutable.ListBuffer[SoftwareModel[_,_]] = mutable.ListBuffer()
  var tickNum = 0

  def pushTransactions(ts: Seq[I]): Unit = {
    ts.foreach { t => inputQueue.enqueue(t) }
  }

  def pullTransactions(): Seq[O] = {
    outputQueue.dequeueAll(_ => true)
  }

  def nothingToProcess: Boolean = {
    inputQueue.isEmpty && childModels.forall(m => m.nothingToProcess)
  }

  def advanceSimulation(): Unit = {
    while (!nothingToProcess) {
      println(s"TICK $tickNum")
      self.tick()
      tickNum += 1
    }
  }

  def process(in: I) : Seq[O]

  // TODO: This def should ideally be final
  def tick(): Unit = {
    val thisClass = this.getClass.getSimpleName
    if (inputQueue.nonEmpty) {
      val in = inputQueue.dequeue()
      println(s"$thisClass Received Transaction $in")
      val out = process(in)
      out.foreach { t =>
        println(s"$thisClass Sent Transaction $t")
        outputQueue.enqueue(t)
      }
    }
  }

  final def compose[O2 <: Transaction](s: SoftwareModel[O, O2]): SoftwareModel[I, O2] = {
    // In this context:
    // this = ILLEGAL to use without violating "early definition" syntax
      // so inputQueue and outputQueue on their own refer to the members of ComposedModel
    // self = the first model
    // s = the second model being chained to the first model's output
    class ComposedModel extends SoftwareModel[I, O2] {
      childModels += s
      childModels += self
      override def tick(): Unit = {
        if (inputQueue.nonEmpty) self.inputQueue.enqueue(inputQueue.dequeueAll(_ => true):_*)
        self.tick()
        if (self.outputQueue.nonEmpty) s.inputQueue.enqueue(self.outputQueue.dequeueAll(_ => true):_*)
        s.tick()
        if (s.outputQueue.nonEmpty) outputQueue.enqueue(s.outputQueue.dequeueAll(_ => true):_*)
      }
      override def process(in: I): Seq[O2] = Seq()
    }
    new ComposedModel
  }
}

/**
  * A software model for turning CREEC HighLevelTransactions into CREEC LowLevelTransactions
  */
class CREECHighToLowModel(p: BusParams) extends SoftwareModel[CREECHighLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: CREECHighLevelTransaction) : Seq[CREECLowLevelTransaction] = {
    //BigInt(0x1000L).toByteArray.reverse.padTo(8, 0).reverse
    assert(in.data.length % (p.dataWidth / 8) == 0, "CREEC high transaction must have data with length = multiple of bus width")
    val beats = in.data.grouped(p.dataWidth / 8).toSeq
    assert((beats.length - 1) <= p.maxBeats, "CREEC high transaction has more beats than bus can support")
    val header = Seq(CREECHeaderBeat(beats.length - 1, 0, in.addr)(p))
    val dataBeats = beats.map(dataBeat => CREECDataBeat(dataBeat, 0)(p))
    header ++ dataBeats
  }
}
