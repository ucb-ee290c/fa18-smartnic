package interconnect

import aes.AESTopCREECBus
import chisel3._
import compression.Compressor
import ecc.{ECCEncoderTop, RSParams}

class CREECelerator extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(new BlockDeviceIOBusParams))
    val out = new CREECBus(new CREECBusParams)
  })
  // TODO: this implicit is a bad idea
  implicit val compressorBus: BusParams = new CREECBusParams

  val compressor = Module(new Compressor(io.in.p, true))
  val eccEncoder = Module(new ECCEncoderTop(RSParams.RS16_8_8, new CREECBusParams))
  //val aes = Module(new AESTopCREECBus()())
  //val deCompressor = Module(new Compressor(compressorBus, false))

  compressor.io.in <> io.in
  eccEncoder.io.slave <> compressor.io.out
  io.out <> eccEncoder.io.master
}
