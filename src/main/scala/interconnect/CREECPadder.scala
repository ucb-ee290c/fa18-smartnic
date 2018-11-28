package interconnect

import chisel3._

/**
  * Pads incoming high-level transactions a multiple of padBytes bytes with zero bytes
  * Modifies encryptionPadBytes field as a hack to handle block size alignment
  * TODO: generalize what field in the input bundle is touched when padding takes place
  * @param padBytes Pad output data to a multiple of padBytes
  */
class CREECPadder(busParams: BusParams, padBytes: Int = 8) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new CREECBus(busParams))
    val out = new CREECBus(busParams)
  })

}

class CREECPadderModel(padBytes: Int = 8) extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    val paddedData = in.data.padTo(math.ceil(in.data.length / padBytes.toFloat).toInt * padBytes, 0.asInstanceOf[Byte])
    Seq(in.copy(data = paddedData.toList, encryptionPadBytes = paddedData.length - in.data.length))
  }
}
