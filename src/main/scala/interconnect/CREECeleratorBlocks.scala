package interconnect

import chisel3._
import chisel3.util._
import dspblocks._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.BaseSubsystem

/**
  * The memory interface writes entries into the queue.
  * They stream out the streaming interface
  * @param depth number of entries in the queue
  * @param streamParameters parameters for the stream node
  * @param p
  */
abstract class WriteQueue
(
  val depth: Int = 16,
  val streamParameters: AXI4StreamMasterParameters = AXI4StreamMasterParameters()
)(implicit p: Parameters) extends LazyModule with HasCSR {
  // stream node, output only
  val streamNode = AXI4StreamMasterNode(streamParameters)

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.out.length == 1)

    // get the output bundle associated with the AXI4Stream node
    val out = streamNode.out(0)._1
    // width (in bits) of the output interface
    val width = out.params.n * 8
    // instantiate a queue
    val queue = Module(new Queue(UInt(out.params.dataBits.W), depth))
    // connect queue output to streaming output
    out.valid := queue.io.deq.valid
    out.bits.data := queue.io.deq.bits
    // don't use last
    out.bits.last := false.B
    queue.io.deq.ready := out.ready

    regmap(
      // each write adds an entry to the queue
      0x0 -> Seq(RegField.w(width, queue.io.enq)),
      // read the number of entries in the queue
      (width+7)/8 -> Seq(RegField.r(width, queue.io.count)),
    )
  }
}

/**
  * TLDspBlock specialization of WriteQueue
  * @param depth number of entries in the queue
  * @param csrAddress address range for peripheral
  * @param beatBytes beatBytes of TL interface
  * @param p
  */
class TLWriteQueue
(
  depth: Int = 16,
  csrAddress: AddressSet = AddressSet(0x2000, 0xff),
  beatBytes: Int = 8,
)(implicit p: Parameters) extends WriteQueue(depth) with TLHasCSR {
  val devname = "tlQueueIn"
  val devcompat = Seq("ucb-art", "dsptools")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem = Some(TLRegisterNode(address = Seq(csrAddress), device = device, beatBytes = beatBytes))
}

/**
  * The streaming interface adds elements into the queue.
  * The memory interface can read elements out of the queue.
  * @param depth number of entries in the queue
  * @param streamParameters parameters for the stream node
  * @param p
  */
abstract class ReadQueue
(
  val depth: Int = 16,
  val streamParameters: AXI4StreamSlaveParameters = AXI4StreamSlaveParameters()
)(implicit p: Parameters) extends LazyModule with HasCSR {
  val streamNode = AXI4StreamSlaveNode(streamParameters)

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)

    // TODO
    // get the input bundle associated with the AXI4Stream node
    val in = streamNode.in(0)._1
    // width (in bits) of the input interface
    val width = in.params.n * 8
    // instantiate a queue
    val queue = Module(new Queue(UInt(in.params.dataBits.W), depth))
    // connect streaming input to queue input
    queue.io.enq.valid := in.valid
    queue.io.enq.bits := in.bits.data
    // don't use last
    in.bits.last := false.B
    in.ready := queue.io.enq.ready

    regmap(
      // each read removes an entry from the queue
      0x0 -> Seq(RegField.r(width, queue.io.deq)),
      // read the number of entries in the queue
      (width+7)/8 -> Seq(RegField.r(width, queue.io.count)),
    )

  }
}

/**
  * TLDspBlock specialization of ReadQueue
  * @param depth number of entries in the queue
  * @param csrAddress address range
  * @param beatBytes beatBytes of TL interface
  * @param p
  */
class TLReadQueue
(
  depth: Int = 16,
  csrAddress: AddressSet = AddressSet(0x2100, 0xff),
  beatBytes: Int = 8
)(implicit p: Parameters) extends ReadQueue(depth) with TLHasCSR {
  val devname = "tlQueueOut"
  val devcompat = Seq("ucb-art", "dsptools")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem = Some(TLRegisterNode(address = Seq(csrAddress), device = device, beatBytes = beatBytes))

}

/**
  * Make DspBlock wrapper for CREECelerator
  * @param creecParams parameters for creec
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam D
  * @tparam U
  * @tparam EO
  * @tparam EI
  * @tparam B
  * @tparam T Type parameter for creec, i.e. FixedPoint or DspReal
  */
abstract class CREECeleratorBlock[D, U, EO, EI, B <: Data, T]
(
)(implicit p: Parameters) extends DspBlock[D, U, EO, EI, B] {
  val streamNode = AXI4StreamIdentityNode()
  val mem = None

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    require(streamNode.out.length == 1)

    val in = streamNode.in.head._1
    val out = streamNode.out.head._1

    // TODO
    in.bits.last := false.B
    out.bits.last := false.B

    val creecW = Module(new CREECeleratorWrite())
    val busParams = creecW.creecBusParams
    require(busParams.dataWidth <= in.params.n * 8,
            "Streaming interface too small")

    // There are three states:
    //   - sSendHeader: for sending the header to the creecW
    //   - sSendData: for sending the data to the creecW
    //   - sSendOut: for waiting until the CREEC computation finish and
    //               sending the result to the StreamNode out
    val sSendHeader :: sSendData :: sSendOut :: Nil = Enum(3)
    val state = RegInit(sSendHeader)

    val beatCnt = RegInit(0.U(32.W))
    val beatLen = 6.U//Reg(UInt(32.W))
    val newBeatLen = RegInit(0.U(32.W))


    val headerBeat = new TransactionHeader(busParams).Lit(
                       len = 0.U,
                       id = 0.U,
                       addr = 0.U,
                       compressed = false.B,
                       encrypted = false.B,
                       ecc = false.B,
                       compressionPadBytes = 0.U,
                       eccPadBytes = 0.U,
                       encryptionPadBytes = 0.U)
    val dataBeat = new TransactionData(busParams).Lit(
                     data = 0.U,
                     id = 0.U)

    when (creecW.io.out.header.fire()) {
      newBeatLen := creecW.io.out.header.bits.len + 1.U
    }

    creecW.io.in.header.bits := headerBeat
    // override len field
    creecW.io.in.header.bits.len := beatLen - 1.U
    creecW.io.in.header.valid := (state === sSendHeader)

    creecW.io.in.data.bits := dataBeat
    // override data field
    creecW.io.in.data.bits.data := in.bits.data
    creecW.io.in.data.valid := (state === sSendData) && in.valid

    // FIXME: being true all the time?
    creecW.io.out.header.ready := true.B
    creecW.io.out.data.ready := (state === sSendOut) && out.ready
    out.bits.data := creecW.io.out.data.bits.data

    // We need to take into account of the back-pressure from Streamnode in
    // and from Streamnode out as well
    in.ready := (state === sSendData) && creecW.io.in.data.ready
    out.valid := (state === sSendOut) && creecW.io.out.data.valid

    switch (state) {
      is (sSendHeader) {
        when (creecW.io.in.header.fire()) {
          state := sSendData
        }
      }

      is (sSendData) {
        when (creecW.io.in.data.fire()) {
          when (beatCnt === beatLen - 1.U) {
            state := sSendOut
            beatCnt := 0.U
          }
          .otherwise {
            beatCnt := beatCnt + 1.U
          }
        }
      }

      is (sSendOut) {
        when (creecW.io.out.data.fire()) {
          when (beatCnt === newBeatLen - 1.U) {
            state := sSendHeader
            beatCnt := 0.U
          }
          .otherwise {
            beatCnt := beatCnt + 1.U
          }
        }
      }
    }
//    // TODO: figure out how to set beatLen via MMIO of this block
//    regmap(
//      0x0 -> Seq(RegField.r(32, beatLen)),
//    )

  }
}

/**
  * TLDspBlock specialization of CREECeleratorBlock
  * @param creecParams parameters for creec
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam T Type parameter for creec data type
  */
class TLCREECeleratorBlock[T]
(
)(implicit p: Parameters) extends
  CREECeleratorBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T]
  with TLDspBlock

/**
  * TLChain is the "right way" to do this, but the dspblocks library seems to be broken.
  * In the interim, this should work.
  * @param creecParams parameters for creec
  * @param depth depth of queues
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam T Type parameter for creec, i.e. FixedPoint or DspReal
  */
class CREECeleratorThing[T]
(
  val depth: Int = 8,
)(implicit p: Parameters) extends LazyModule {
  // instantiate lazy modules
  val writeQueue = LazyModule(new TLWriteQueue(depth))
  val creec = LazyModule(new TLCREECeleratorBlock())
  val readQueue = LazyModule(new TLReadQueue(depth))

  // connect streamNodes of queues and creec
  readQueue.streamNode := creec.streamNode := writeQueue.streamNode

  lazy val module = new LazyModuleImp(this)
}

/**
  * Mixin for top-level rocket to add a CREECelerator
  *
  */
trait HasPeripheryCREECelerator extends BaseSubsystem {
  // instantiate creec chain
  val creecChain = LazyModule(new CREECeleratorThing())

  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("creecWrite")) { creecChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("creecRead")) { creecChain.readQueue.mem.get }
}

