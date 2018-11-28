package interconnect

import chisel3._
import compression.Compressor

class CREECelerator extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(new BlockDeviceIOBusParams))
    val out = new CREECBus(new CREECBusParams)
  })
  val compressorOut = new CREECBus(new CREECBusParams)
  // TODO: this is a bad idea
  implicit val compressorBus = new CREECBusParams
  val compressor = new Compressor(io.in.p, true)

  io.out.header <> Reg(io.in.header)
  io.out.data <> Reg(io.in.data)
}
