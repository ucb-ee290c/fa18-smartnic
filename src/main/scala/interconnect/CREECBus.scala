package interconnect

import chisel3._
import chisel3.util.{Decoupled, log2Ceil}

case class BusParams(addrBits: Int, maxBeats: Int, maxInFlight: Int, dataWidth: Int) {
  require(addrBits > 0)
  require(maxBeats >= 1)
  val beatBits = log2Ceil(maxBeats - 1)
  require(maxInFlight >= 0)
  val idBits = log2Ceil(maxInFlight)
  require(dataWidth > 0)
  require(dataWidth % 8 == 0)
}

// All addresses are BYTE addresses (not sector, not page)
// All addresses must land on a 64b boundary

// From the block device IO (master) to the compression block (slave) (address on 512B boundary, 64 beats per req guaranteed)
// Also from the MMU/remapper (master) to the block device model (slave) (address on 512B, 64 beats required)
class BlockDeviceIOBusParams extends BusParams(64, 64, 1, 64)

// Used internally to connect (compression -> parity/ECC -> encryption -> mapping/MMU unit)
class CREECBusParams extends BusParams(64, 128, 1, 64)

class WriteRequest(val p: BusParams) extends Bundle {
  val addr = UInt(p.addrBits.W)
  val len = UInt(p.beatBits.W)
  val id = UInt(p.maxInFlight.W)
}

class WriteData(val p: BusParams) extends Bundle {
  val id = UInt(p.maxInFlight.W)
  val data = UInt(p.dataWidth.W)
}

class ReadRequest(val p: BusParams) extends Bundle {
  val addr = UInt(p.addrBits.W)
  val len = UInt(p.beatBits.W)
  val id = UInt(p.maxInFlight.W)
}

class ReadData(val p: BusParams) extends Bundle {
  val id = UInt(p.maxInFlight.W)
  val data = UInt(p.dataWidth.W)
}

//TODO: shouldn't wrReq and wrData be Flipped, and rdReq and rdData not?
class CREECBus(val p: BusParams) extends Bundle {
  val wrReq = Decoupled(new WriteRequest(p))
  val wrData = Decoupled(new WriteData(p))
  val rdReq = Decoupled(new ReadRequest(p))
  val rdData = Flipped(Decoupled(new ReadData(p)))
}
