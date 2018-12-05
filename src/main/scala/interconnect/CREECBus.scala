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
  val bytesPerBeat: Int = dataWidth / 8
}

object BusParams {
  // From the block device IO (master) to the compression block (slave) (64 beats per req guaranteed)
  // Also from the MMU/remapper (master) to the block device model (slave) (address on 512B, 64 beats required)
  val blockDev = BusParams(64, 1, 64)

  // Used internally to connect (compression -> parity/ECC -> encryption -> mapping/MMU unit)
  val creec = BusParams(128, 1, 64)
  val creecInterleave = BusParams(128, 32, 64)

  // Wider bus interface for AES (aligned to block size)
  val aes = BusParams(128, 1, 128)
}

class CREECBusParams extends BusParams(128, 1, 64)

class TransactionHeader(val p: BusParams) extends Bundle {
  val len = UInt(p.beatBits.W)
  val id = UInt(p.maxInFlight.W)
  // Sector (512B) address (2TB addressable)
  val addr = UInt(32.W)

  // Indicate whether compression, encryption, ECC was applied to this transaction
  // This CREECMetadata struct will be written in the sector mapping table
  val compressed = Bool()
  val encrypted = Bool()
  val ecc = Bool()
  val compressionPadBytes = UInt(log2Ceil(p.bytesPerBeat).W)
  val eccPadBytes = UInt(log2Ceil(p.bytesPerBeat).W)
  // AES block size = 128 bits = 16 bytes = maximum padding required
  val encryptionPadBytes = UInt(log2Ceil(16).W)

  def Lit(len: UInt, id: UInt, addr: UInt,
          compressed: Bool, encrypted: Bool, ecc: Bool,
          compressionPadBytes: UInt, eccPadBytes: UInt,
          encryptionPadBytes: UInt)
          : TransactionHeader.this.type = {
    import chisel3.core.BundleLitBinding
    val clone = cloneType
    clone.selfBind(BundleLitBinding(Map(
      clone.len -> litArgOfBits(len),
      clone.id -> litArgOfBits(id),
      clone.addr -> litArgOfBits(addr),
      clone.compressed -> litArgOfBits(compressed),
      clone.encrypted -> litArgOfBits(encrypted),
      clone.ecc -> litArgOfBits(ecc),
      clone.compressionPadBytes -> litArgOfBits(compressionPadBytes),
      clone.eccPadBytes-> litArgOfBits(eccPadBytes),
      clone.encryptionPadBytes -> litArgOfBits(encryptionPadBytes)
    )))
    clone
  }
}

class TransactionData(val p: BusParams) extends Bundle {
  val data = UInt(p.dataWidth.W)
  val id = UInt(p.maxInFlight.W)

  def Lit(data: UInt, id: UInt): TransactionData.this.type = {
    import chisel3.core.BundleLitBinding
    val clone = cloneType
    clone.selfBind(BundleLitBinding(Map(
      clone.data -> litArgOfBits(data),
      clone.id -> litArgOfBits(id)
    )))
    clone
  }
}

class CREECBus(val p: BusParams) extends Bundle {
  val header = Decoupled(new TransactionHeader(p))
  val data = Decoupled(new TransactionData(p))
}

