package interconnect

import scala.collection.mutable

/**
  * A Transaction is an abstract notion of a chunk of data and control that a block can process at a time
  */
trait Transaction

abstract class CREECTransaction extends Transaction

/**
  * A HighLevelTransaction represents a full sector write request or read response with all the control and data bundled together
  */
case class CREECHighLevelTransaction(data: Seq[BigInt], addr: BigInt) extends CREECTransaction

/**
  * A HighLevelTransaction can be decomposed into 1 CREECHeaderBeat and multiple CREECDataBeats.
  * These LowLevelTransactions are used to drive the physical wires of the CREECBus
  */
abstract class CREECLowLevelTransaction extends CREECTransaction

case class CREECHeaderBeat(len: Int, id: Int, addr: BigInt) extends CREECLowLevelTransaction
case class CREECDataBeat(data: BigInt, id: Int) extends CREECLowLevelTransaction

/**
  * An abstract software model for a DUT which pulls a input transaction and produces output transactions on each tick.
  * At any tick, there may or may not be an input transaction to process. Several output transactions can be written each tick.
  * A concrete model should implement process().
  * @tparam I type of input transactions
  * @tparam O type of output transactions
  */
// TODO: this whole API should be based on streams with a synchronization API, not on ticks and processing
// TODO: but this requires we first go through the struggle with this API and learn
// TODO: this class should be abstract, but that breaks easy composition
abstract class SoftwareModel[I <: Transaction, O <: Transaction] { self =>
  val inputQueue: mutable.Queue[I] = mutable.Queue[I]()
  val outputQueue: mutable.Queue[O] = mutable.Queue[O]()
  val childModels: mutable.ListBuffer[SoftwareModel[_,_]] = mutable.ListBuffer()
  var cycle = 0

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
      println(s"CYCLE $cycle")
      self.tick()
      cycle += 1
    }
  }

  // TODO: This function should be abstract
  def process(in: I) : Seq[O]

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
class CREECHighToLowModel extends SoftwareModel[CREECHighLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: CREECHighLevelTransaction) : Seq[CREECLowLevelTransaction] = {
      val header = Seq(CREECHeaderBeat(in.data.length, 0, in.addr))
      val dataBeats = in.data.map(dataBeat => CREECDataBeat(dataBeat, 0))
      header ++ dataBeats
  }
}
