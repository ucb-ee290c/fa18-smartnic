// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._
import interconnect._

class ECCEncoderTopModel(val rsParams: RSParams = new RSParams(),
                         val busParams: CREECBusParams = new CREECBusParams()
  ) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)
  val numItems = rsParams.n / busParams.bytesPerBeat

  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {

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
  ) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)
  val numItems = rsParams.n / busParams.bytesPerBeat
  var data: Seq[Byte] = Seq()

  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {

    in match {
      case t: CREECHeaderBeat =>
        val newLen = (t.len + 1) / numItems - 1
        Seq(CREECHeaderBeat(newLen, t.id, t.addr)(busParams))
      case t: CREECDataBeat =>
        data = data ++ t.data

        // Wait until getting enough data
        if (data.size == rsParams.n) {
          // Convert signed to unsigned.
          // FIXME: Is there a better way?
          val inputMsgs = data.map(x => {
            val e = if (x.toInt < 0) { x.toInt + 256 } else { x.toInt }
              e
          })

          // Clear data to process next input
          data = Nil

          // FIXME: why is 'reverse' needed here?
          val decodedMsgs = rs.decode(inputMsgs).slice(0, rsParams.k).
            map(_.toByte).reverse
          val decodedMsgsGrp = decodedMsgs.grouped(busParams.bytesPerBeat).toSeq
          decodedMsgsGrp.map (x => CREECDataBeat(x, 0)(busParams)).toSeq
        }
        else {
          Seq()
        }
    }

  }
}
