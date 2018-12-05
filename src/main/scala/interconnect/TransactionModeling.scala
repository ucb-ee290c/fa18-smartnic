package interconnect

import scala.collection.mutable

/**
  * A Transaction is an abstract notion of a chunk of data and control that a block can process at a time
  */
trait Transaction

abstract class CREECTransaction extends Transaction

/**
  * A HighLevelTransaction represents a full sector write request or read response with all
  * the control and data bundled together. It is generic to any CREECBus parameterization.
  */
case class CREECHighLevelTransaction(
  data: Seq[Byte],
  // TODO: all these are metadata fields, they don't really belong here
  // TODO: having to pass on addr inside every software model is verbose/boilerplate and not extensible to many additional fields
  addr: BigInt,
  compressed: Boolean = false,
  encrypted: Boolean = false,
  ecc: Boolean = false,
  compressionPadBytes: Int = 0,
  eccPadBytes: Int = 0,
  encryptionPadBytes: Int = 0) extends CREECTransaction {
  // TODO: find a way to guarantee these types of constraints in the type system (Seq length with dependent types)
    // after more analysis, list length dependent typing breaks down after length > 50 or so
  assert(data.length % 8 == 0,
    s"CREEC high level transaction must have data with length = data bus width (multiple of 8B) * numBeats. Got $data")

  // TODO: Print bytes as unsigned
  //override def toString: String = super.toString
}

/**
  * A HighLevelTransaction can be decomposed into 1 CREECHeaderBeat and multiple CREECDataBeats.
  * These LowLevelTransactions are used to drive the physical wires of the CREECBus.
  * LowLevelTransactions are CREECBus parameter specific.
  */
abstract class CREECLowLevelTransaction extends CREECTransaction

case class CREECHeaderBeat(
  len: Int,
  id: Int,
  addr: BigInt,
  compressed: Boolean = false,
  encrypted: Boolean = false,
  ecc: Boolean = false,
  compressionPadBytes: Int = 0,
  eccPadBytes: Int = 0,
  encryptionPadBytes: Int = 0)(implicit p: BusParams) extends CREECLowLevelTransaction {
  require(len <= (p.maxBeats - 1))
  require(id <= p.maxInFlight)
}
case class CREECDataBeat(data: Seq[Byte], id: Int)(implicit p: BusParams) extends CREECLowLevelTransaction {
  require(id <= p.maxInFlight)
  // data.length = 64 bits, 128 bits, 256 bits, etc... = data width of CREECBus
  require(data.length == p.bytesPerBeat)
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
// TODO: we need a way to allow multiple typed input and output ports and have time synchronization between them
  // for metadata purposes (e.g. paddedByte count on a padding model)
abstract class SoftwareModel[I <: Transaction, O <: Transaction] { self =>
  val inputQueue: mutable.Queue[I] = mutable.Queue[I]()
  val outputQueue: mutable.Queue[O] = mutable.Queue[O]()
  val childModels: mutable.ListBuffer[SoftwareModel[_,_]] = mutable.ListBuffer()
  var tickNum = 0
  val thisClass: String = this.getClass.getSimpleName

  /**
    * Queue up transactions internally, but don't process them.
    * @param ts Input transactions to queue
    * @return This software model to chain with other functions
    */
  final def pushTransactions(ts: Seq[I]): SoftwareModel[I, O] = {
    inputQueue.enqueue(ts:_*)
    self
  }

  /**
    * Pull output transactions that have been processed by this software model.
    * @return Output transactions
    */
  final def pullTransactions(): Seq[O] = {
    outputQueue.dequeueAll(_ => true)
  }

  /**
    * Process all the input transactions queued up in this software model and write to the outputQueue
    * @param print Prints out tick numbers and received/sent transactions for all childModels
    * @return This software model to chain with other functions
    */
  final def advanceSimulation(print: Boolean = false): SoftwareModel[I, O] = {
    while (!nothingToProcess) {
      if (print) println(s"TICK $tickNum")
      self.tick(print)
      tickNum += 1
    }
    self
  }

  def nothingToProcess: Boolean = {
    inputQueue.isEmpty && childModels.forall(m => m.nothingToProcess)
  }

  /**
    * Shorthand for pushTransactions -> advanceSimulation -> pullTransactions
    * @return The output transactions produced by the model after queuing $ts and advancing simulation
    */
  final def processTransactions(ts: Seq[I], print: Boolean = false): Seq[O] = {
    self.pushTransactions(ts).advanceSimulation(print).pullTransactions()
  }

  /**
    * Implement this function to create a concrete SoftwareModel
    * A software model should accept an input transaction of type I, perform some computation.
    * and return a sequence of none, one, or many output transactions that were produced on this tick.
    * It is acceptable to maintain internal mutable state in a SoftwareModel
    * @param in The input transaction to process
    * @return The processed output of your model
    */
  protected def process(in: I) : Seq[O]

  // TODO: This def should ideally be final
  def tick(print: Boolean = false): Unit = {
    if (inputQueue.nonEmpty) {
      val in = inputQueue.dequeue()
      if (print) println(s"$thisClass received $in")
      val out = process(in)
      out.foreach { t =>
        if (print) println(s"$thisClass sent $t")
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
      override def tick(print: Boolean = false): Unit = {
        if (inputQueue.nonEmpty) self.inputQueue.enqueue(inputQueue.dequeueAll(_ => true):_*)
        self.tick(print)
        if (self.outputQueue.nonEmpty) s.inputQueue.enqueue(self.outputQueue.dequeueAll(_ => true):_*)
        s.tick(print)
        if (s.outputQueue.nonEmpty) outputQueue.enqueue(s.outputQueue.dequeueAll(_ => true):_*)
      }
      override def process(in: I): Seq[O2] = Seq()
    }
    new ComposedModel
  }

  // Shorthand for compose
  final def ->[O2 <: Transaction](s: SoftwareModel[O, O2]): SoftwareModel[I, O2] = compose(s)
}

/**
  * A software model for turning CREEC HighLevelTransactions into CREEC LowLevelTransactions
  */
class CREECHighToLowModel(p: BusParams) extends SoftwareModel[CREECHighLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: CREECHighLevelTransaction) : Seq[CREECLowLevelTransaction] = {
    assert(in.data.length % p.bytesPerBeat == 0, "CREEC high transaction must have data with length = multiple of bus width")
    val beats = in.data.grouped(p.bytesPerBeat).toSeq
    assert((beats.length - 1) <= p.maxBeats, "CREEC high transaction has more beats than bus can support")
    val header = Seq(CREECHeaderBeat(
      len = beats.length - 1,
      id = 0,
      addr = in.addr,
      compressed = in.compressed,
      encrypted = in.encrypted,
      ecc = in.ecc,
      compressionPadBytes = in.compressionPadBytes,
      eccPadBytes = in.eccPadBytes,
      encryptionPadBytes = in.encryptionPadBytes)(p))
    val dataBeats = beats.map(dataBeat => CREECDataBeat(dataBeat, 0)(p))
    header ++ dataBeats
  }
}

class CREECLowToHighModel(p: BusParams) extends SoftwareModel[CREECLowLevelTransaction, CREECHighLevelTransaction] {
  val dataRepack = mutable.Map[Int, Seq[Byte]]()
  val inFlight = mutable.Map[Int, CREECHeaderBeat]()

  override def process(in: CREECLowLevelTransaction): Seq[CREECHighLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat =>
        assert(!inFlight.contains(t.id), s"$thisClass received a header beat with id ${t.id} which was in flight and not completed.")
        inFlight.update(t.id, t)
        dataRepack.update(t.id, Seq())
        Seq()
      case t: CREECDataBeat =>
        assert(inFlight.contains(t.id), s"$thisClass received a data beat with id ${t.id} which didn't have an associated header beat.")
        val storedData = dataRepack.getOrElse(t.id, Seq())
        val newData = storedData ++ t.data
        dataRepack.update(t.id, newData)
        if (newData.length / p.bytesPerBeat == (inFlight(t.id).len + 1)) {
          val savedHeader = inFlight(t.id)
          inFlight.remove(t.id)
          dataRepack.remove(t.id)
          Seq(CREECHighLevelTransaction(
            data = newData,
            addr = savedHeader.addr,
            compressed = savedHeader.compressed,
            encrypted = savedHeader.encrypted,
            ecc = savedHeader.ecc,
            compressionPadBytes = savedHeader.compressionPadBytes,
            eccPadBytes = savedHeader.eccPadBytes,
            encryptionPadBytes = savedHeader.encryptionPadBytes
          ))
        } else {
          Seq()
        }
    }
  }
}

