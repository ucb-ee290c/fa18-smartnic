package interconnect

import aes.AESTopCREECBus
import chisel3._
import compression.Compressor
import ecc.{ECCEncoderTop, ECCDecoderTop, RSParams}

class CREECeleratorWrite extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(BusParams.blockDev))
    val out = new CREECBus(BusParams.creec)
  })
  val compressor = Module(new Compressor(io.in.p, compress = true))

  val widthExpander = Module(new CREECWidthConverter(p1 = BusParams.creec,
                                                     p2 = BusParams.aes))

  val aes = Module(new AESTopCREECBus(BusParams.aes))

  val widthContractor1 = Module(new CREECWidthConverter(p1 = BusParams.aes,
                                                        p2 = BusParams.creec))

  val eccEncoder = Module(new ECCEncoderTop(RSParams.RS16_8_8,
                                            BusParams.creec,
                                            BusParams.ecc))

  val widthContractor2 = Module(new CREECWidthConverter(p1 = BusParams.ecc,
                                                        p2 = BusParams.creec))

  compressor.io.in <> io.in
  widthExpander.io.slave <> compressor.io.out
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
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(BusParams.creec))
    val out = new CREECBus(BusParams.creec)
  })
  val widthExpander1 = Module(new CREECWidthConverter(p1 = BusParams.creec,
                                                      p2 = BusParams.ecc))

  val eccDecoder = Module(new ECCDecoderTop(RSParams.RS16_8_8,
                                            BusParams.ecc,
                                            BusParams.creec))

  val widthExpander2 = Module(new CREECWidthConverter(p1 = BusParams.creec,
                                                      p2 = BusParams.aes))

  val aes = Module(new AESTopCREECBus(BusParams.aes))

  val widthContractor = Module(new CREECWidthConverter(p1 = BusParams.aes,
                                                       p2 = BusParams.creec))

  val stripper = Module(new CREECStripper(BusParams.creec))

  val decompressor = Module(new Compressor(io.out.p, compress = false))

  widthExpander1.io.slave <> io.in
  eccDecoder.io.slave <> widthExpander1.io.master
  widthExpander2.io.slave <> eccDecoder.io.master
  aes.io.decrypt_slave <> widthExpander2.io.master
  widthContractor.io.slave <> aes.io.decrypt_master
  stripper.io.slave <> widthContractor.io.master
  decompressor.io.in <> stripper.io.master
  io.out <> decompressor.io.out

  aes.io.encrypt_slave.header.noenq()
  aes.io.encrypt_slave.data.noenq()
  aes.io.encrypt_master.header.nodeq()
  aes.io.encrypt_master.data.nodeq()
}

class CREECeleratorFull extends Module {
  val io = IO(new Bundle {
    val write_in = Flipped(new CREECBus(BusParams.blockDev))
    val write_out = new CREECBus(BusParams.creec)

    val read_in = Flipped(new CREECBus(BusParams.creec))
    val read_out = new CREECBus(BusParams.creec)
  })
  val writePath = Module(new CREECeleratorWrite)
  io.write_in <> writePath.io.in
  io.write_out <> writePath.io.out

  val readPath = Module(new CREECeleratorRead)
  io.read_in <> readPath.io.in
  io.read_out <> readPath.io.out
}

object CREECeleratorApp extends App {
  Driver.execute(Array("None"), () => new CREECeleratorWrite())
  Driver.execute(Array("None"), () => new CREECeleratorRead())
  Driver.execute(Array("None"), () => new CREECeleratorFull())
}
