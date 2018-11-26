package interconnect

import aes.{CREECEncryptHighModel}
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
    0, 0, 0, 0, 0, 0, 0, 255,
    0, 0, 0, 0, 0, 0, 0, 255 // add another beat to force compression padding
  ).map(_.asInstanceOf[Byte]), 0x1000))

  "compression -> decompression loop" should "work" in {
    val compressionLoop =
      new CompressorModel(true) ->
      new CompressorModel(false)
    val out = compressionLoop.processTransactions(highTx)
    assert(out.head.data == highTx.head.data)
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
      // Simulate a width converter that can add the right amount of padding for the AES block size
      new CREECPadder(16) -> new CREECEncryptHighModel,
      new ECCEncoderTopModel(rsParams)
    )
    val orderings = models.permutations.toList
    for (ordering <- orderings) {
      println(ordering)
      val model = ordering.reduce(_ -> _)
      val out = model.processTransactions(highTx)
      println(out.head.data.length)
    }
  }
}
