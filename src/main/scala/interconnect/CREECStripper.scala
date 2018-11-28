package interconnect

import chisel3._

class CREECStripper(busParams: BusParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(busParams))
    val out = new CREECBus(busParams)
  })

}

// This is a hack to strip padding which may have been added prior to the AES block
class CREECStripperModel extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    val strippedData = in.data.dropRight(in.encryptionPadBytes)
    Seq(in.copy(data = strippedData, encryptionPadBytes = 0))
  }
}
