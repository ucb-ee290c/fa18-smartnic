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
  isWrite: Boolean = true
)(implicit p: Parameters) extends DspBlock[D, U, EO, EI, B] with HasCSR {
  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    require(streamNode.out.length == 1)

    val in = streamNode.in.head._1
    val out = streamNode.out.head._1

    // TODO
    in.bits.last := false.B
    out.bits.last := false.B

    // MMIO Registers
    val creecEnable = RegInit(false.B)
    // In header info
    val numBeatsIn = RegInit(0.U(32.W))
    val crIn = RegInit(false.B)
    val eIn = RegInit(false.B)
    val eccIn = RegInit(false.B)
    val crPadBytesIn = RegInit(0.U(32.W))
    val ePadBytesIn = RegInit(0.U(32.W))
    val eccPadBytesIn = RegInit(0.U(32.W))
    // Out header info
    val numBeatsOut = RegInit(0.U(32.W))
    val crOut = RegInit(false.B)
    val eOut = RegInit(false.B)
    val eccOut = RegInit(false.B)
    val crPadBytesOut = RegInit(0.U(32.W))
    val ePadBytesOut = RegInit(0.U(32.W))
    val eccPadBytesOut = RegInit(0.U(32.W))

    val creec = if (isWrite) {
      Module(new CREECeleratorWrite())
    }
    else {
      Module(new CREECeleratorRead())
    }

    // There are three states:
    //   - sSendHeader: for sending the header to the creec
    //   - sSendData: for sending the data to the creec
    //   - sSendOut: for waiting until the CREEC computation finish and
    //               sending the result to the StreamNode out
    val sSendHeader :: sSendData :: sSendOut :: Nil = Enum(3)
    val state = RegInit(sSendHeader)

    val beatCnt = RegInit(0.U(32.W))

    when (creec.io.out.header.fire()) {
      numBeatsOut := creec.io.out.header.bits.len + 1.U
      crOut := creec.io.out.header.bits.compressed
      eOut := creec.io.out.header.bits.encrypted
      eccOut := creec.io.out.header.bits.ecc
      crPadBytesOut := creec.io.out.header.bits.compressionPadBytes
      ePadBytesOut := creec.io.out.header.bits.encryptionPadBytes
      eccPadBytesOut := creec.io.out.header.bits.eccPadBytes
    }

    creec.io.in.header.bits.len := numBeatsIn - 1.U
    creec.io.in.header.valid := (state === sSendHeader) && creecEnable

    // header beat
    creec.io.in.header.bits.addr := 0.U
    creec.io.in.header.bits.id := 0.U
    creec.io.in.header.bits.compressed := crIn
    creec.io.in.header.bits.encrypted := eIn
    creec.io.in.header.bits.ecc := eccIn
    creec.io.in.header.bits.compressionPadBytes := crPadBytesIn
    creec.io.in.header.bits.encryptionPadBytes := ePadBytesIn
    creec.io.in.header.bits.eccPadBytes := eccPadBytesIn

    // data beat
    creec.io.in.data.bits.data := in.bits.data
    creec.io.in.data.bits.id := 0.U

    creec.io.in.data.valid := (state === sSendData) && in.valid

    // FIXME: being true all the time?
    creec.io.out.header.ready := true.B
    creec.io.out.data.ready := (state === sSendOut) && out.ready
    out.bits.data := creec.io.out.data.bits.data

    // We need to take into account of the back-pressure from Streamnode in
    // and from Streamnode out as well
    in.ready := (state === sSendData) && creec.io.in.data.ready
    out.valid := (state === sSendOut) && creec.io.out.data.valid

    switch (state) {
      is (sSendHeader) {
        when (creec.io.in.header.fire()) {
          state := sSendData
        }
      }

      is (sSendData) {
        when (creec.io.in.data.fire()) {
          when (beatCnt === numBeatsIn - 1.U) {
            state := sSendOut
            beatCnt := 0.U
          }
          .otherwise {
            beatCnt := beatCnt + 1.U
          }
        }
      }

      is (sSendOut) {
        when (creec.io.out.data.fire()) {
          when (beatCnt === numBeatsOut - 1.U) {
            state := sSendHeader
            beatCnt := 0.U
          }
          .otherwise {
            beatCnt := beatCnt + 1.U
          }
        }
      }
    }

    // We ignore addr and id fields as of now
    // since they are not relevant to what we
    // want to test
    regmap(
      0x00 -> Seq(RegField.w(1,  creecEnable)),
      0x04 -> Seq(RegField.w(32, numBeatsIn)),
      0x0c -> Seq(RegField.r(1,  crIn)),
      0x10 -> Seq(RegField.r(1,  eIn)),
      0x14 -> Seq(RegField.r(1,  eccIn)),
      0x18 -> Seq(RegField.r(32, crPadBytesIn)),
      0x1c -> Seq(RegField.r(32, ePadBytesIn)),
      0x20 -> Seq(RegField.r(32, eccPadBytesIn)),
      0x24 -> Seq(RegField.r(32, numBeatsOut)),
      0x28 -> Seq(RegField.r(1,  crOut)),
      0x2c -> Seq(RegField.r(1,  eOut)),
      0x30 -> Seq(RegField.r(1,  eccOut)),
      0x34 -> Seq(RegField.r(32, crPadBytesOut)),
      0x38 -> Seq(RegField.r(32, ePadBytesOut)),
      0x3c -> Seq(RegField.r(32, eccPadBytesOut))
    )

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
  depth: Int = 16,
  csrAddress: AddressSet = AddressSet(0x2200, 0xff),
  beatBytes: Int = 8,
  isWrite: Boolean = true
)(implicit p: Parameters) extends
  CREECeleratorBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T](isWrite)
  with TLDspBlock with TLHasCSR {

  val devname = "creecW"
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
  val writeQueueW = LazyModule(new TLWriteQueue(
                      depth, csrAddress = AddressSet(0x2000, 0xff)))
  val readQueueW = LazyModule(new TLReadQueue(
                     depth, csrAddress = AddressSet(0x2100, 0xff)))
  val writeQueueR = LazyModule(new TLWriteQueue(
                      depth, csrAddress = AddressSet(0x2200, 0xff)))
  val readQueueR = LazyModule(new TLReadQueue(
                     depth, csrAddress = AddressSet(0x2300, 0xff)))

  val creecW = LazyModule(new TLCREECeleratorBlock(
                 isWrite = true, csrAddress = AddressSet(0x2400, 0xff)))
  val creecR = LazyModule(new TLCREECeleratorBlock(
                 isWrite = false, csrAddress = AddressSet(0x2500, 0xff)))

  // connect streamNodes of queues and creecelerators
  // separate {read, write} queues for creecR and creecW
  readQueueW.streamNode := creecW.streamNode := writeQueueW.streamNode
  readQueueR.streamNode := creecR.streamNode := writeQueueR.streamNode

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
  pbus.toVariableWidthSlave(Some("writeQueueW")) {
    creecChain.writeQueueW.mem.get
  }
  pbus.toVariableWidthSlave(Some("readQueueW")) {
    creecChain.readQueueW.mem.get
  }
  pbus.toVariableWidthSlave(Some("creecW")) {
    creecChain.creecW.mem.get
  }
  pbus.toVariableWidthSlave(Some("writeQueueR")) {
    creecChain.writeQueueR.mem.get
  }
  pbus.toVariableWidthSlave(Some("readQueueR")) {
    creecChain.readQueueR.mem.get
  }
  pbus.toVariableWidthSlave(Some("creecR")) {
    creecChain.creecR.mem.get
  }
}

