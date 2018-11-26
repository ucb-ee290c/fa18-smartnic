package interconnect

import aes.{AESBusParams, CREECEncryptHighModel}
import compression.CompressorModel
import ecc.{ECCEncoderTopModel, RSCode, RSParams}
import org.scalatest.FlatSpec

class CREECeleratorSWTest extends FlatSpec {
  val highTx = Seq(CREECHighLevelTransaction(Seq(
    1, 2, 3, 4, 5, 6, 7, 8,
    0, 0, 0, 0, 0, 0, 0, 1, // test MSB data
    1, 0, 0, 0, 0, 0, 0, 0, // test LSB data
    0, 0, 0, 0, 0, 0, 0, 0,
    0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8,
    255, 255, 255, 255, 255, 255, 255, 255, // test overflow
    255, 0, 0, 0, 0, 0, 0, 0, // test byte-level overflow
    0, 0, 0, 0, 0, 0, 0, 255
  ).map(_.asInstanceOf[Byte]), 0x1000))

  "CREECPadder" should "pad to 8 bytes" in {
    val compressionLoop =
      new CompressorModel(true) ->
      new CREECPadder(8)
    val tx = Seq(CREECHighLevelTransaction(Seq(1, 1, 1, 1, 1, 6, 7, 8), 0x1000))
    // 6 bytes should come out from the CompressorModel
    val out = compressionLoop.processTransactions(tx, false)
    assert(out.head.data.length == 8)
  }

  "testing various model orderings" should "reveal an ideal creec pipeline ordering" in {
    val numSymbols = 16
    val numMsgs = 8
    val symbolWidth = 8
    val rs = new RSCode(numSymbols, numMsgs, symbolWidth)
    val rsParams = RSParams(
      n = numSymbols,
      k = numMsgs,
      symbolWidth = symbolWidth,
      gCoeffs = rs.gCoeffs,
      fConst = rs.fConst,
      rs.Log2Val,
      rs.Val2Log
    )

    import scala.io.Source
    val theRepublic = Source.fromResource("The_Republic_Plato.txt").toList.map(_.asInstanceOf[Byte])
    val theRepublicPadded = theRepublic.padTo(math.ceil(theRepublic.length / 16.0).toInt * 16, 0.asInstanceOf[Byte])
    val dataPackets = theRepublicPadded.grouped(512)
    val tx = dataPackets.map(CREECHighLevelTransaction(_, 0x0)).toSeq

    val models = Seq(
      new CompressorModel(true),
      new CREECEncryptHighModel(new AESBusParams),
      new ECCEncoderTopModel(rsParams)
    )
    val orderings = models.permutations.toList
    for (ordering <- orderings) {
      println(ordering)
      val model = ordering.reduce(_.compose(new CREECPadder(16)).compose(_))
      val out = model.processTransactions(tx)
      println(out.head.data.length)
    }
  }
}
