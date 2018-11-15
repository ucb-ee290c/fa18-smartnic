package interconnect

import chisel3._

/**
  * The RTL CREEC passthrough. Adds one to any data beat as it passes through.
  */
class CREECPassthrough(p: BusParams) extends Module {
  val io = IO(new Bundle {
    val slave = new CREECWriteBus(p)
    val master = new CREECWriteBus(p)
  })
  io.master.header <> Reg(io.slave.header)
  io.master.data <> Reg(io.slave.data)
  // Let's add a little logic here
  io.master.data.bits.data := Reg(io.slave.data.bits.data + 1.U)
}

class CREECPassthroughModel(p: BusParams) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat => Seq(t)
      // TODO: this won't work if t.data is all ones since it will overflow
      case t: CREECDataBeat =>
        // Prepend a 0 byte to make sure t.data is decoded as unsigned
        // Note: BigInt(Array(0, 0, 1) = 1
        // Note: BigInt(Array(0, 1, 1) = 257
        // Note: BigInt(Array(255, 255, 255) = -1 (not what we wanted)
        // Note: BigInt(Array(0, 255, 255, 255) = 16777215 (this is better)
        val dataAsInt = BigInt((0.asInstanceOf[Byte] +: t.data).toArray)
        val modifiedData = (dataAsInt + 1).toByteArray
        // We may have overflow (in which case there is 1 extra byte), or fewer than 8 bytes if the original data was small
        val refinedData = if (modifiedData.length > p.dataWidth/8) {
          modifiedData.reverse.slice(0, p.dataWidth/8).reverse // truncate overflow
        } else if (modifiedData.length < p.dataWidth / 8) {
          modifiedData.reverse.padTo(p.dataWidth/8, 0.asInstanceOf[Byte]).reverse // pad with zero bytes
        } else {
          modifiedData
        }
        Seq(CREECDataBeat(refinedData, 0)(p))
    }
  }
}
