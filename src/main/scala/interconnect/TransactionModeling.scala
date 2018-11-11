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
class SoftwareModel[I <: Transaction, O <: Transaction] { self =>
  val inputQueue: mutable.Queue[I] = mutable.Queue[I]()
  val outputQueue: mutable.Queue[O] = mutable.Queue[O]()
  val childModels: List[SoftwareModel[Transaction, Transaction]] = List()

  def pushTransactions(ts: Seq[I]): Unit = {
    ts.foreach { t => inputQueue.enqueue(t) }
  }

  def pullTransactions() : Seq[O] = {
    outputQueue.dequeueAll(_ => true)
  }

  def nothingToProcess : Boolean = {
    inputQueue.isEmpty && childModels.forall(_.inputQueue.isEmpty)
  }

  // TODO: This function should be abstract
  def process(in: Option[I]) : Option[Seq[O]] = None

  def tick(): Unit = {
    val thisClass = this.getClass.getSimpleName
    val in = if (inputQueue.nonEmpty) Some(inputQueue.dequeue()) else None
    in.foreach {t => println(s"$thisClass Received Transaction $t")}
    val out = process(in)
    out match {
      case Some(transactions) =>
        transactions.foreach(t => {
          println(s"$thisClass Sent Transaction $t")
          outputQueue.enqueue(t)
        })
      case None =>
    }
  }

  final def compose[O2 <: Transaction](s: SoftwareModel[O, O2]): SoftwareModel[I, O2] = {
    // In this context:
    // this = ILLEGAL to use without violating "early definition" syntax
      // so inputQueue and outputQueue on their own refer to the members of ComposedModel
    // self = the first model
    // s = the second model being chained to the first model's output
    class ComposedModel extends SoftwareModel[I, O2] {
      override def tick(): Unit = {
        if (inputQueue.nonEmpty) self.inputQueue.enqueue(inputQueue.dequeue())
        self.tick()
        if (self.outputQueue.nonEmpty) s.inputQueue.enqueue(self.outputQueue.dequeue())
        s.tick()
        if (s.outputQueue.nonEmpty) outputQueue.enqueue(s.outputQueue.dequeue)
      }
    }
    new ComposedModel
  }
}

/**
  * A software model for turning CREEC HighLevelTransactions into CREEC LowLevelTransactions
  */
class CREECHighToLowModel extends SoftwareModel[CREECHighLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: Option[CREECHighLevelTransaction]) : Option[Seq[CREECLowLevelTransaction]] = {
    in match {
      case Some(t) =>
        val header = Seq(CREECHeaderBeat(t.data.length, 0, t.addr))
        val dataBeats = t.data.map(dataBeat => CREECDataBeat(dataBeat, 0))
        Some(header ++ dataBeats)
      case None => None
    }
  }
}
