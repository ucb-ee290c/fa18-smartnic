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
// TODO: this whole API should be based on (fs2) streams with a synchronization API, not on ticks and processing
// TODO: but this requires we first go through the struggle with this API and learn
abstract class SoftwareModel[I <: Transaction, O <: Transaction] {
  val inputQueue = mutable.Queue[I]()
  val outputQueue = mutable.Queue[O]()

  def process(in: Option[I]) : Option[Seq[O]]

  final def tick: Unit = {
    val in = if (inputQueue.nonEmpty) Some(inputQueue.dequeue()) else None
    val out = process(in)
    out match {
      case Some(transactions) => transactions.foreach(outputQueue.enqueue(_))
      case None =>
    }
  }
}

/**
  * A software model for turning CREEC HighLevelTransactions into CREEC LowLevelTransactions
  */
class CREECHighToLowModel extends SoftwareModel[CREECHighLevelTransaction, CREECLowLevelTransaction] {
  def process(in: Option[CREECHighLevelTransaction]) : Option[Seq[CREECLowLevelTransaction]] = {
    in match {
      case Some(t) =>
        val header = Seq(CREECHeaderBeat(t.data.length, 0, t.addr))
        val dataBeats = t.data.map(dataBeat => CREECDataBeat(dataBeat, 0))
        Some(header ++ dataBeats)
      case None => None
    }
  }
}
