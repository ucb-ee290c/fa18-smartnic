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
abstract class CREECeleratorReadBlock[D, U, EO, EI, B <: Data, T]
(
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
    val enable = RegInit(false.B)
    val numBeatsIn = RegInit(0.U(32.W))
    // The header beat info needs to be passed to the C host
    // so that the Read path knows how to decode
    val numBeatsOut = RegInit(0.U(32.W))
    val crIn = RegInit(false.B)
    val eIn = RegInit(false.B)
    val eccIn = RegInit(false.B)
    val crPadBytesIn = RegInit(0.U(32.W))
    val ePadBytesIn = RegInit(0.U(32.W))
    val eccPadBytesIn = RegInit(0.U(32.W))

    val creecR = Module(new CREECeleratorRead())
    val busParams = creecR.creecBusParams
    require(busParams.dataWidth <= in.params.n * 8,
            "Streaming interface too small")

    // There are three states:
    //   - sSendHeader: for sending the header to the creecR
    //   - sSendData: for sending the data to the creecR
    //   - sSendOut: for waiting until the CREEC computation finish and
    //               sending the result to the StreamNode out
    val sSendHeader :: sSendData :: sSendOut :: Nil = Enum(3)
    val state = RegInit(sSendHeader)

    val beatCnt = RegInit(0.U(32.W))

    //Most of this is bypassed below
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

    when (creecR.io.out.header.fire()) {
      numBeatsIn := creecR.io.out.header.bits.len + 1.U
      /*crIn := creecR.io.out.header.bits.compressed
      eIn := creecR.io.out.header.bits.encrypted
      eccIn := creecR.io.out.header.bits.ecc
      crPadBytesIn := creecR.io.out.header.bits.compressionPadBytes
      ePadBytesIn := creecR.io.out.header.bits.encryptionPadBytes
      eccPadBytesIn := creecR.io.out.header.bits.eccPadBytes*/
    }


    creecR.io.in.header.bits := headerBeat
    // override len field
    creecR.io.in.header.bits.len := numBeatsIn - 1.U

    creecR.io.in.header.bits.compressed := crIn
    creecR.io.in.header.bits.encrypted := eIn
    creecR.io.in.header.bits.ecc := eccIn
    creecR.io.in.header.bits.compressionPadBytes := crPadBytesIn
    creecR.io.in.header.bits.encryptionPadBytes := ePadBytesIn
    creecR.io.in.header.bits.eccPadBytes := eccPadBytesIn

    creecR.io.in.header.valid := (state === sSendHeader) && enable

    creecR.io.in.data.bits := dataBeat
    // override data field
    creecR.io.in.data.bits.data := in.bits.data
    creecR.io.in.data.valid := (state === sSendData) && in.valid

    // FIXME: being true all the time?
    creecR.io.out.header.ready := true.B
    creecR.io.out.data.ready := (state === sSendOut) && out.ready
    out.bits.data := creecR.io.out.data.bits.data

    // We need to take into account of the back-pressure from Streamnode in
    // and from Streamnode out as well
    in.ready := (state === sSendData) && creecR.io.in.data.ready
    out.valid := (state === sSendOut) && creecR.io.out.data.valid

    switch (state) {
      is (sSendHeader) {
        when (creecR.io.in.header.fire()) {
          state := sSendData
        }
      }

      is (sSendData) {
        when (creecR.io.in.data.fire()) {
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
        when (creecR.io.out.data.fire()) {
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
      0x00 -> Seq(RegField.w(1,  enable)),
      0x04 -> Seq(RegField.w(32, numBeatsIn)),
      0x08 -> Seq(RegField.r(32, numBeatsOut)),
      0x0c -> Seq(RegField.w(1,  crIn)),
      0x10 -> Seq(RegField.w(1,  eIn)),
      0x14 -> Seq(RegField.w(1,  eccIn)),
      0x18 -> Seq(RegField.w(32, crPadBytesIn)),
      0x1c -> Seq(RegField.w(32, ePadBytesIn)),
      0x20 -> Seq(RegField.w(32, eccPadBytesIn)),
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
class TLCREECeleratorReadBlock[T]
(
  depth: Int = 16,
  csrAddress: AddressSet = AddressSet(0x2200, 0xff),
  beatBytes: Int = 8
)(implicit p: Parameters) extends
  CREECeleratorReadBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T]
  with TLDspBlock with TLHasCSR {

  val devname = "creecR"
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
class CREECeleratorReadThing[T]
(
  val depth: Int = 8,
)(implicit p: Parameters) extends LazyModule {
  // instantiate lazy modules
  val writeQueue = LazyModule(new TLWriteQueue(depth))
  val creec = LazyModule(new TLCREECeleratorReadBlock())
  val readQueue = LazyModule(new TLReadQueue(depth))

  // connect streamNodes of queues and creec
  readQueue.streamNode := creec.streamNode := writeQueue.streamNode

  lazy val module = new LazyModuleImp(this)
}

/**
  * Mixin for top-level rocket to add a CREECelerator
  *
  */
trait HasPeripheryCREECeleratorRead extends BaseSubsystem {
  // instantiate creec chain
  val creecChain = LazyModule(new CREECeleratorReadThing())

  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("writeQueue")) { creecChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("readQueue")) { creecChain.readQueue.mem.get }
  pbus.toVariableWidthSlave(Some("creecR")) { creecChain.creec.mem.get }

}