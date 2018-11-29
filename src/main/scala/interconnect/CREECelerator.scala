package interconnect

import aes.{AESBusParams, AESTopCREECBus}
import chisel3._
import compression.Compressor
import ecc.{ECCEncoderTop, RSParams}

class CREECelerator extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(new BlockDeviceIOBusParams))
    val out = new CREECBus(new AESBusParams)
  })
  // TODO: this implicit is a bad idea
  implicit val compressorBus: BusParams = new CREECBusParams
  val creecBusParams = new CREECBusParams
  val aesBusParams = new AESBusParams

  val compressor = Module(new Compressor(io.in.p, true))
  val eccEncoder = Module(new ECCEncoderTop(RSParams.RS16_8_8, creecBusParams))
  val widthExpander = Module(new CREECWidthConverter(p1 = creecBusParams, p2 = aesBusParams))
  val aes = Module(new AESTopCREECBus(aesBusParams))
  //val deCompressor = Module(new Compressor(compressorBus, false))

  compressor.io.in <> io.in
  eccEncoder.io.slave <> compressor.io.out
  widthExpander.io.slave <> eccEncoder.io.master
  aes.io.encrypt_slave <> widthExpander.io.master

  aes.io.decrypt_slave.header.noenq()
  aes.io.decrypt_slave.data.noenq()
  aes.io.decrypt_master.header.nodeq()
  aes.io.decrypt_master.data.nodeq()

  io.out <> aes.io.encrypt_master
}
