package interconnect
import chisel3._
import chisel3.util._
import squants.information.{Bytes, Information}

/**
  * This module takes 'block device' writes (sector aligned) from the CREEC pipeline that may have
  * more or less than 512B (512 bytes per sector) of data and buffers them to be written
  * to the actual block device, along with a mapping table.
  */
// TODO: type safe arguments to avoid potential byte/sector/64b offset conversion bugs
// TODO: squants doesn't support .toBytes on Information to Int type natively (use implicit conversion)
  // TODO: this support ought to be added upstream
case class Sectors(nSectors: Int, sectorSize: Information)

class SectorRemapper(val maxSectors: Int = 2048*1024*1024*1024 / 512,
                     val mapTableOffset: Int = 1024,
                     val maxBlockLength: Int = 2,
                     val sectorSize: Int = 512,
                     val nBuffers: Int = 8,
                     val nBlockDeviceTrackers: Int = 1) extends Module {
  val io = IO(new Bundle {
    val wrSlave = new CREECBus(new CREECBusParams)
    val wrMaster = new CREECBus(BusParams.blockDev)

    val rdSlave = new CREECBus(BusParams.blockDev)
    val rdMaster = new CREECBus(new CREECBusParams)
  })

  //class DataBuffer extends Module {
    //val mem = SyncReadMem(maxBlockLength * sectorSize, )
    //val buffers: Seq[SyncReadMem[UInt]] = Seq.fill(nBuffers)(SyncReadMem(maxBlockLength*sectorSize*8 / io.slave.p.dataWidth, UInt(io.slave.p.dataWidth.W)))
  //}

  // Parameterization requirements
  require(isPow2(maxSectors))
  require(isPow2(sectorSize))

  // Staic assertions
  when (io.wrSlave.header.fire()) {
    //assert((io.slave.wrReq.bits.addr % sectorSize.U) === 0.U)
    // TODO: assert that beats per write transaction don't exceed maxBlockLength that can be stored per sector
  }
  when (io.rdSlave.header.fire()) {
    //assert((io.master.wrReq.bits.addr % sectorSize.U) === 0.U)
  }

  // Helper Classes/Functions
  class TableEntry extends Bundle {
    val originalSector = UInt(log2Ceil(maxSectors).W)
    val mappedSector = UInt(log2Ceil(maxSectors).W)
    val mappedOffset = UInt(log2Ceil(sectorSize/8).W)
    val mappedLength = UInt((maxBlockLength*sectorSize / 8).W)
    val entryValid = Bool()
    //val metadata = new CREECMetadataBundle
  }

  // Round up numToRound to a multiple of 'multiple'
  def roundUp(numtoRound: Int, multiple: Int): Int = {
    require(multiple > 0)
    ((numtoRound + multiple - 1) / multiple) * multiple
  }

  /**
    * Mapping Table Structure:
    * Original Sector | Mapped Sector | Mapped Sector Offset (in 8B multiples) |
    * Mapped Length (in 8B multiples, can cross physical sectors) |
    * Valid Bit |
    * Metadata (from CREEC pipeline components, compressed?, encrypted?, parity added?)
    */
  // TODO: parameterize the inter-sector access granularity (currently hardcoded to 8B -> 64 bits)
  val mapTableEntryRawBits: Int = (new TableEntry).getWidth
  val mapTableEntryRawBytes: Int  = roundUp(mapTableEntryRawBits, 8) / 8
  // TODO: this is a hack, find a mathematical solution (round up mapTableWidth until it evenly divides sectorSize, just LCM(x, y) / x)
  val mapTableEntryBytes: Int = mapTableEntryRawBytes.until(mapTableEntryRawBytes*4).filter(sectorSize % _ == 0).head
  val mapTableEntriesPerSector: Int = mapTableEntryBytes / sectorSize

  // Local table cache
  // TODO: support multiple cached tables
  val tableValid = RegInit(false.B)
  val tableOffset = RegInit(0.U(log2Ceil(maxSectors).W))
  val table = SyncReadMem(mapTableEntriesPerSector, new TableEntry)

  // Data Buffers (to hold write data for each sector before the data are packed)
  // TODO: support different data widths from CREECBus and to BlockDeviceIOBus (width converter required)
  //val buffers: Seq[SyncReadMem[UInt]] = Seq.fill(nBuffers)(SyncReadMem(maxBlockLength*sectorSize*8 / io.slave.p.dataWidth, UInt(io.slave.p.dataWidth.W)))

  // Global FSM
  /** First figure out how to handle writes
    * 1. When the slave port receives a write request, fetch the table corresponding to the sector that should be written
    *   a. Send a read request from the master port
    *   b. For each beat read, route the data into the cached table
    * 2. Inspect the write request and figure out which buffer to fill
    *   a. TODO: if all the buffers are full, then they need to be written to disk
    *   b. Fill the buffer with the write data
    */
  val sIdle :: sFetchCacheReq :: sFetchCacheFilling :: sSendToBuffer :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val workingWrite = Reg(new TransactionHeader(io.wrSlave.p))

  io.wrSlave.header.ready := state === sIdle
  io.wrSlave.data.ready := state === sSendToBuffer

  io.rdMaster.header.bits.addr := workingWrite.addr
  io.rdMaster.header.bits.id := 0.U // TODO: threaded reads from BlockDevice
  io.rdMaster.header.bits.len := 64.U
  io.rdMaster.header.valid := state === sFetchCacheReq
  val cacheFetchCounter = Reg(UInt(log2Ceil(io.wrSlave.p.maxBeats).W))

  switch (state) {
    is (sIdle) {
      // TODO: don't always fetch cache
      when (io.wrSlave.header.fire()) {
        workingWrite := io.wrSlave.header
        //when (tableValid && tableOffset === io.slave.wrReq.bits.addr)
        state := sFetchCacheReq
      }
    }
    is (sFetchCacheReq) {
      when (io.rdMaster.header.fire()) {
        cacheFetchCounter := 63.U
        state := sFetchCacheFilling
      }.otherwise {
        state := sFetchCacheReq
      }
    }
    is (sFetchCacheFilling) {
      when (io.rdMaster.data.fire()) {
        cacheFetchCounter := cacheFetchCounter - 1.U
        when (cacheFetchCounter === 0.U) {
          state := sSendToBuffer
        }
        table(63.U - cacheFetchCounter) := io.rdMaster.data.bits.data.asTypeOf(new TableEntry)
      }
    }
    is (sSendToBuffer) {

    }
  }
}
