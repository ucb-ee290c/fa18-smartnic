// See README.md for license details.

package ecc

import interconnect._

class CommChannel(val rsParams: RSParams, val noiseByteLevel: Int) extends
  SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)

  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {

    // Group the data beats into chunks of n symbols,
    // and introduce noise to (up to) *noiseByteLevel* symbols
    val inGrp = in.data.grouped(rsParams.n).toSeq
    val output = inGrp.map{m =>
      val noisySyms = rs.noiseGen(m.map(_.toInt), noiseByteLevel)
      noisySyms.map(_.toByte)
    }.flatten

    Seq(in.copy(data = output))
  }
}

class ECCEncoderTopModel(val rsParams: RSParams) extends
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

class ECCDecoderTopModel(val rsParams: RSParams) extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
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
