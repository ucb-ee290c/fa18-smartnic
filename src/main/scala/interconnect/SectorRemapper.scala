package interconnect
import chisel3._
import chisel3.util.log2Ceil

/**
  * This module takes 'block device' writes (sector aligned) from the CREEC pipeline that may have
  * more or less than 512B (512 bytes per sector) of data and buffers them to be written
  * to the actual block device, along with a mapping table.
  */
class SectorRemapper(val maxSectors: Int = 2048*1024*1024*1024 / 512,
                     val mapTableOffset: Int = 1024,
                     val maxBlockLength: Int = 2,
                     val sectorSize: Int = 512) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(new CREECBusParams))
    val master = new CREECBus(new BlockDeviceIOBusParams)
  })

  // Round up numToRound to a multiple of 'multiple'
  def roundUp(numtoRound: Int, multiple: Int): Int = {
    require(multiple > 0)
    ((numtoRound + multiple - 1) / multiple) * multiple
  }

  when (io.slave.wrReq.fire()) {
    // Write address must be sector aligned, and represents 1 sector of data
    assert(io.slave.wrReq.bits.addr % sectorSize.U == 0.U)
  }

  /**
    * Mapping Table Structure
    * Original Sector | Mapped Sector | Mapped Sector Offset (in 8B multiples) |
    * Mapped Length (in 8B multiples, can cross physical sectors) |
    * Valid Bit |
    * Metadata (from CREEC pipeline components, compressed?, encrypted?, parity added?)
    */
  // TODO: trailing +8 should come from the number of bits in a metadata bundle
  // TODO: parameterize the inter-sector access granularity (currently hardcoded to 8B -> 64 bits)
  val mapTableEntryRawBits: Int = 2*log2Ceil(maxSectors) + log2Ceil(sectorSize/8) + log2Ceil(maxBlockLength*sectorSize/8) + 1 + 8
  val mapTableEntryRawBytes: Int  = roundUp(mapTableEntryRawBits, 8) / 8
  // TODO: this is a hack, find a mathematical solution (round up mapTableWidth until it evenly divides sectorSize
  val mapTableEntryBytes: Int = mapTableEntryRawBytes.until(mapTableEntryRawBytes*4).filter(sectorSize % _ == 0).head
  val mapTableEntriesPerSector: Int = mapTableEntryBytes / sectorSize
  require(mapTableEntriesPerSector > 0)
}
