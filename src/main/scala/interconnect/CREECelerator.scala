package interconnect

import aes.{AESBusParams, AESTopCREECBus}
import chisel3._
import compression.Compressor
import ecc.{ECCEncoderTop, RSParams, ECCBusParams}

class CREECelerator extends Module {
  val creecBusParams = new CREECBusParams
  val aesBusParams = new AESBusParams
  val eccBusParams = new ECCBusParams

  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(BusParams.blockDev))
    val out = new CREECBus(BusParams.creec)
  })

  // TODO: this implicit is a bad idea
  implicit val compressorBus: BusParams = creecBusParams

  val compressor = Module(new Compressor(io.in.p, true))
  val padder = Module(new CREECPadder(creecBusParams, 16))
  val widthExpander = Module(new CREECWidthConverter(p1 = creecBusParams, p2 = aesBusParams))
  val aes = Module(new AESTopCREECBus(aesBusParams))
  val widthContractor1 = Module(new CREECWidthConverter(p1 = aesBusParams, p2 = creecBusParams))
  val eccEncoder = Module(new ECCEncoderTop(RSParams.RS16_8_8,
                     creecBusParams, eccBusParams))
  val widthContractor2 = Module(new CREECWidthConverter(p1 = eccBusParams, p2 = creecBusParams))

  compressor.io.in <> io.in
  padder.io.in <> compressor.io.out
  widthExpander.io.slave <> padder.io.out
  aes.io.encrypt_slave <> widthExpander.io.master
  widthContractor1.io.slave <> aes.io.encrypt_master
  eccEncoder.io.slave <> widthContractor1.io.master
  widthContractor2.io.slave <> eccEncoder.io.master
  io.out <> widthContractor2.io.master

  aes.io.decrypt_slave.header.noenq()
  aes.io.decrypt_slave.data.noenq()
  aes.io.decrypt_master.header.nodeq()
  aes.io.decrypt_master.data.nodeq()
}

object CREECeleratorApp extends App {
  Driver.execute(Array("None"), () => new CREECelerator())
}
