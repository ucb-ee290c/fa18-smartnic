package interconnect

import aes.AESTopCREECBus
import chisel3._
import compression.Compressor
import ecc.{ECCEncoderTop, ECCDecoderTop, RSParams, ECCBusParams}

class CREECeleratorWrite extends Module {
  val eccBusParams = new ECCBusParams

  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(BusParams.blockDev))
    val out = new CREECBus(BusParams.creec)
  })

  // TODO: remove this
  implicit val compressorBus: BusParams = BusParams.creec

  val compressor = Module(new Compressor(io.in.p, compress = true))

  val widthExpander = Module(new CREECWidthConverter(p1 = BusParams.creec,
                                                     p2 = BusParams.aes))

  val aes = Module(new AESTopCREECBus(BusParams.aes))

  val widthContractor1 = Module(new CREECWidthConverter(p1 = BusParams.aes,
                                                        p2 = BusParams.creec))

  val eccEncoder = Module(new ECCEncoderTop(RSParams.RS16_8_8,
                                            BusParams.creec,
                                            eccBusParams))

  val widthContractor2 = Module(new CREECWidthConverter(p1 = eccBusParams,
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
  val creecBusParams = new CREECBusParams
  val eccBusParams = new ECCBusParams

  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(BusParams.creec))
    val out = new CREECBus(BusParams.creec)
  })

  implicit val compressorBus: BusParams = creecBusParams

  val widthExpander1 = Module(new CREECWidthConverter(p1 = creecBusParams,
                                                      p2 = eccBusParams))

  val eccDecoder = Module(new ECCDecoderTop(RSParams.RS16_8_8,
                                            eccBusParams,
                                            creecBusParams))

  val widthExpander2 = Module(new CREECWidthConverter(p1 = creecBusParams,
                                                      p2 = BusParams.aes))

  val aes = Module(new AESTopCREECBus(BusParams.aes))

  val widthContractor = Module(new CREECWidthConverter(p1 = BusParams.aes,
                                                       p2 = creecBusParams))

  val stripper = Module(new CREECStripper(creecBusParams))

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

//----------------

// TODO avoid copying what we have above
class CREECeleratorFull extends Module {
  val creecBusParams = new CREECBusParams
  val eccBusParams = new ECCBusParams

  val io = IO(new Bundle {
    val write_in = Flipped(new CREECBus(BusParams.blockDev))
    val write_out = new CREECBus(BusParams.creec)

    val read_in = Flipped(new CREECBus(BusParams.creec))
    val read_out = new CREECBus(BusParams.creec)
  })

  implicit val compressorBus: BusParams = creecBusParams

  //---------

  val compressor = Module(new Compressor(io.write_in.p, compress = true))

  val widthConvertWrite1 = Module(new CREECWidthConverter(p1 = creecBusParams,
    p2 = BusParams.aes))

  val aes = Module(new AESTopCREECBus(BusParams.aes))

  val widthConvertWrite2 = Module(new CREECWidthConverter(p1 = BusParams.aes,
    p2 = creecBusParams))

  val eccEncoder = Module(new ECCEncoderTop(RSParams.RS16_8_8,
    creecBusParams,
    eccBusParams))

  val widthConvertWrite3 = Module(new CREECWidthConverter(p1 = eccBusParams,
    p2 = creecBusParams))

  compressor.io.in <> io.write_in
  widthConvertWrite1.io.slave <> compressor.io.out
  aes.io.encrypt_slave <> widthConvertWrite1.io.master
  widthConvertWrite2.io.slave <> aes.io.encrypt_master
  eccEncoder.io.slave <> widthConvertWrite2.io.master
  widthConvertWrite3.io.slave <> eccEncoder.io.master
  io.write_out <> widthConvertWrite3.io.master

  //---------

  //Expand to ECC
  val widthConvertRead1 = Module(new CREECWidthConverter(p1 = creecBusParams,
    p2 = eccBusParams))

  val eccDecoder = Module(new ECCDecoderTop(RSParams.RS16_8_8,
    eccBusParams,
    creecBusParams))

  val widthConvertRead2 = Module(new CREECWidthConverter(p1 = creecBusParams,
    p2 = BusParams.aes))

  val widthConvertRead3 = Module(new CREECWidthConverter(p1 = BusParams.aes,
    p2 = creecBusParams))

  val stripper = Module(new CREECStripper(creecBusParams))

  val decompressor = Module(new Compressor(io.read_out.p, compress = false))

  widthConvertRead1.io.slave <> io.read_in
  eccDecoder.io.slave <> widthConvertRead1.io.master
  widthConvertRead2.io.slave <> eccDecoder.io.master
  aes.io.decrypt_slave <> widthConvertRead2.io.master
  widthConvertRead3.io.slave <> aes.io.decrypt_master
  stripper.io.slave <> widthConvertRead3.io.master
  decompressor.io.in <> stripper.io.master
  io.read_out <> decompressor.io.out

  //------

}

object CREECeleratorApp extends App {
  Driver.execute(Array("None"), () => new CREECeleratorWrite())
  Driver.execute(Array("None"), () => new CREECeleratorRead())
  Driver.execute(Array("None"), () => new CREECeleratorFull())
}
