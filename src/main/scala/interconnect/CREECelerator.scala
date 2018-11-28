package interconnect

import chisel3._
import compression.Compressor

class CREECelerator extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(new BlockDeviceIOBusParams))
    val out = new CREECBus(new CREECBusParams)
  })
  val compressorOut = new CREECBus(new CREECBusParams)
  // TODO: this implicit is a bad idea
  implicit val compressorBus: BusParams = new CREECBusParams
  val compressor = Module(new Compressor(io.in.p, true))
  val deCompressor = Module(new Compressor(compressorBus, false))

  compressor.io.in <> io.in
  deCompressor.io.in <> compressor.io.out
  io.out <> deCompressor.io.out
}
