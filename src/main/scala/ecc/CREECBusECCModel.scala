// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._
import interconnect._

class ECCEncoderTopModel(val rsParams: RSParams = new RSParams(),
                         val busParams: CREECBusParams = new CREECBusParams()
  ) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {
    val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)
    val numItems = rsParams.n * rsParams.symbolWidth / busParams.dataWidth

    in match {
      case t: CREECHeaderBeat =>
        val newLen = (t.len + 1) * numItems - 1
        Seq(CREECHeaderBeat(newLen, t.id, t.addr)(busParams))
      case t: CREECDataBeat =>
        // Convert signed to unsigned.
        // FIXME: Is there a better way?
        val inputMsgs = t.data.map(x => {
          val e = if (x.toInt < 0) { x.toInt + 256 } else { x.toInt }
            e
        })

        // FIXME: why is 'reverse' needed here?
        val encodedMsgs = rs.encode(inputMsgs).map(_.toByte).reverse
        val encodedMsgsGrp = encodedMsgs.grouped(busParams.bytesPerBeat).toSeq
        encodedMsgsGrp.map (x => CREECDataBeat(x, 0)(busParams)).toSeq
    }
  }
}

class ECCDecoderTopModel(val rsParams: RSParams = new RSParams(),
                         val busParams: CREECBusParams = new CREECBusParams()
  ) extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)
    val inputMsgs = in.data.map(_.toInt)
    val decodedMsgs = rs.decode(inputMsgs).map(_.toByte)
    Seq(CREECHighLevelTransaction(decodedMsgs, in.addr))
  }
}
