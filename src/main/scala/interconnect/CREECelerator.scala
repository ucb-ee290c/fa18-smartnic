package interconnect

import aes.{AESBusParams, AESTopCREECBus}
import chisel3._
import compression.Compressor
import ecc.{ECCEncoderTop, ECCDecoderTop, RSParams, ECCBusParams}

class CREECeleratorWrite extends Module {
  val creecBusParams = new CREECBusParams
  val aesBusParams = new AESBusParams
  val eccBusParams = new ECCBusParams

  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(BusParams.blockDev))
    val out = new CREECBus(BusParams.creec)
  })

  implicit val compressorBus: BusParams = creecBusParams

  val compressor = Module(new Compressor(io.in.p, true))
  val padder = Module(new CREECPadder(creecBusParams, 16))
  val widthExpander = Module(new CREECWidthConverter(p1 = creecBusParams,
                                                     p2 = aesBusParams))
  val aes = Module(new AESTopCREECBus(aesBusParams))
  val widthContractor1 = Module(new CREECWidthConverter(p1 = aesBusParams,
                                                        p2 = creecBusParams))
  val eccEncoder = Module(new ECCEncoderTop(RSParams.RS16_8_8,
                                            creecBusParams,
                                            eccBusParams))
  val widthContractor2 = Module(new CREECWidthConverter(p1 = eccBusParams,
                                                        p2 = creecBusParams))

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

class CREECeleratorRead extends Module {
  val creecBusParams = new CREECBusParams
  val aesBusParams = new AESBusParams
  val eccBusParams = new ECCBusParams

  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(BusParams.creec))
    // FIXME: BusParams.blockDev?
    val out = new CREECBus(BusParams.creec)
  })

  implicit val compressorBus: BusParams = creecBusParams

  val widthExpander1 = Module(new CREECWidthConverter(p1 = creecBusParams,
                                                      p2 = eccBusParams))

  val eccDecoder = Module(new ECCDecoderTop(RSParams.RS16_8_8,
                                            eccBusParams,
                                            creecBusParams))

  val widthExpander2 = Module(new CREECWidthConverter(p1 = creecBusParams,
                                                      p2 = aesBusParams))

  val aesDecryption = Module(new AESTopCREECBus(aesBusParams))

  val widthContractor = Module(new CREECWidthConverter(p1 = aesBusParams,
                                                       p2 = creecBusParams))

  val stripper = Module(new CREECStripper(creecBusParams))

  val decompressor = Module(new Compressor(io.out.p, false))

  widthExpander1.io.slave <> io.in
  eccDecoder.io.slave <> widthExpander1.io.master
  widthExpander2.io.slave <> eccDecoder.io.master
  aesDecryption.io.decrypt_slave <> widthExpander2.io.master
  widthContractor.io.slave <> aesDecryption.io.decrypt_master
  stripper.io.slave <> widthContractor.io.master
  decompressor.io.in <> stripper.io.master
  io.out <> decompressor.io.out

  aesDecryption.io.encrypt_slave.header.noenq()
  aesDecryption.io.encrypt_slave.data.noenq()
  aesDecryption.io.encrypt_master.header.nodeq()
  aesDecryption.io.encrypt_master.data.nodeq()
}

object CREECeleratorApp extends App {
  Driver.execute(Array("None"), () => new CREECeleratorWrite())
}
