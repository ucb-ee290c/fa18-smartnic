package interconnect

import aes.{AESBusParams, AESTopCREECBus}
import chisel3._
import compression.Compressor
import ecc.{ECCEncoderTop, RSParams}

class CREECelerator extends Module {
  val creecBusParams = new CREECBusParams
  val aesBusParams = new AESBusParams

  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(new BlockDeviceIOBusParams))
    val out = new CREECBus(creecBusParams)
  })

  // TODO: this implicit is a bad idea
  implicit val compressorBus: BusParams = creecBusParams

  val compressor = Module(new Compressor(io.in.p, true))
  val padder = Module(new CREECPadder(creecBusParams, 16))
  val widthExpander = Module(new CREECWidthConverter(p1 = creecBusParams, p2 = aesBusParams))
  val aes = Module(new AESTopCREECBus(aesBusParams))
  val widthContractor = Module(new CREECWidthConverter(p1 = aesBusParams, p2 = creecBusParams))
  val eccEncoder = Module(new ECCEncoderTop(RSParams.RS16_8_8, creecBusParams))

  compressor.io.in <> io.in
  padder.io.in <> compressor.io.out
  widthExpander.io.slave <> padder.io.out
  aes.io.encrypt_slave <> widthExpander.io.master
  widthContractor.io.slave <> aes.io.encrypt_master
  eccEncoder.io.slave <> widthContractor.io.master
  io.out <> eccEncoder.io.master

  aes.io.decrypt_slave.header.noenq()
  aes.io.decrypt_slave.data.noenq()
  aes.io.decrypt_master.header.nodeq()
  aes.io.decrypt_master.data.nodeq()
}
