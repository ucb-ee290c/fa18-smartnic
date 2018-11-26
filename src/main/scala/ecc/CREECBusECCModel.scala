// See README.md for license details.

package ecc

import interconnect._

class ECCEncoderTopModel(val rsParams: RSParams = new RSParams()) extends
  SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)

  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {

    val inGrp = in.data.grouped(rsParams.k).toSeq
    val outputs = inGrp.map {x =>
      val inputMsgs = x.map(m => {
        val e = if (m.toInt < 0) { m.toInt + 256 } else { m.toInt }
        e
      })

      val encodedMsgs = rs.encode(inputMsgs).map(_.toByte)
      encodedMsgs
    }.flatten

    Seq(in.copy(data = outputs, ecc = true))
  }
}

class ECCDecoderTopModel(val rsParams: RSParams = new RSParams(),
                         val busParams: CREECBusParams = new CREECBusParams()
  ) extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)

  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {

    val inGrp = in.data.grouped(rsParams.n).toSeq
    val outputs = inGrp.map {x =>
      val inputMsgs = x.map(m => {
        val e = if (m.toInt < 0) { m.toInt + 256 } else { m.toInt }
        e
      })

      val decodedMsgs = rs.decode(inputMsgs).map(_.toByte).slice(0, rsParams.k)
      decodedMsgs
    }.flatten

    Seq(in.copy(data = outputs, ecc = false))
  }
}
