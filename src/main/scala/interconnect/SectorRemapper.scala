package interconnect
import chisel3._

/**
  *
  */
class SectorRemapper extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(new CREECBusParams))
    val master = new CREECBus(new BlockDeviceIOBusParams)
  })
}
