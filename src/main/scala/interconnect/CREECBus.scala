package interconnect

import chisel3._
import chisel3.util.{Decoupled, log2Ceil}

case class BusParams(maxBeats: Int, maxInFlight: Int, dataWidth: Int) {
  require(maxBeats >= 1)
  val beatBits = log2Ceil(maxBeats - 1)
  require(maxInFlight >= 0)
  val idBits = log2Ceil(maxInFlight)
  require(dataWidth > 0)
  require(dataWidth % 8 == 0)
}

// From the block device IO (master) to the compression block (slave) (64 beats per req guaranteed)
// Also from the MMU/remapper (master) to the block device model (slave) (address on 512B, 64 beats required)
class BlockDeviceIOBusParams extends BusParams(64, 1, 64)

// Used internally to connect (compression -> parity/ECC -> encryption -> mapping/MMU unit)
class CREECBusParams extends BusParams(128, 1, 64)

// TODO: traits can't take parameters in Scala
trait BusAddress {
  // Sector (512B) address (2TB addressable)
  val addr = UInt(32.W)
}

/**
  * This CREECMetadata struct will be written in the sector mapping table
  */
trait CREECMetadata {
  // Indicate whether compression, encryption, ECC was applied to this transaction
  val compressed = Bool()
  val encrypted = Bool()
  val ecc = Bool()
}

class CREECMetadataBundle extends Bundle with CREECMetadata

class TransactionHeader(val p: BusParams) extends Bundle {
  val len = UInt(p.beatBits.W)
  val id = UInt(p.maxInFlight.W)
}

class TransactionData(val p: BusParams) extends Bundle {
  val data = UInt(p.dataWidth.W)
  val id = UInt(p.maxInFlight.W)
}

abstract class CREECBus(val p: BusParams) extends Bundle {
  val header = Decoupled(new TransactionHeader(p) with BusAddress with CREECMetadata)
  val data = Decoupled(new TransactionData(p))
}

class CREECWriteBus(val p: BusParams) extends CREECBus(p) {
}

class CREECReadBus(val p: BusParams) extends CREECBus(p) {
}
