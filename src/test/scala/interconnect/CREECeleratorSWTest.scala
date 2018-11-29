package interconnect

import aes.{CREECDecryptHighModel, CREECEncryptHighModel}
import compression.CompressorModel
import ecc.{ECCDecoderTopModel, ECCEncoderTopModel, RSParams, CommChannel}
import org.scalatest.FlatSpec

class CREECeleratorSWTest extends FlatSpec {
  val testTx = Seq(CREECHighLevelTransaction(Seq(
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

  def readTxFromFile(file: String): Seq[CREECHighLevelTransaction] = {
    import scala.io.Source
    val fileBytes = Source.fromResource(file).toList.map(_.asInstanceOf[Byte])
    val fileBytesPadded = fileBytes.padTo(math.ceil(fileBytes.length / 8.0).toInt * 8, 0.asInstanceOf[Byte])
    val dataPackets = fileBytesPadded.grouped(512)
    dataPackets.zipWithIndex.map {
      case (data, i) => CREECHighLevelTransaction(data = data, addr = i)
    }.toSeq
  }
  val theRepublic = readTxFromFile("The_Republic_Plato.txt")

  "compression -> decompression loop" should "work" in {
    val compressionLoop =
      new CompressorModel(true) ->
      new CompressorModel(false)
    val out = compressionLoop.processTransactions(testTx)
    assert(out == testTx)
  }

  "testing various model orderings" should "reveal an ideal creec pipeline ordering" in {
    val models = Seq(
      new CompressorModel(true),
      // Simulate a width converter that can add the right amount of padding for the AES block size
      new CREECPadderModel(16) -> new CREECEncryptHighModel,
      new ECCEncoderTopModel(RSParams.RS16_8_8)
    )
    val orderings = models.permutations.toList
    for (ordering <- orderings) {
      println(ordering)
      val model = ordering.reduce(_ -> _)
      val out = model.processTransactions(theRepublic)
      println(out.map(_.data.length).sum)
    }
  }

  "compressor -> encrypter -> eccEncoder -> eccDecode -> decrypter -> decompressor" should "be an identity transform" in {
    val model =
      new CompressorModel(true) ->
      new CREECPadderModel(16) ->
      new CREECEncryptHighModel ->
      new ECCEncoderTopModel(RSParams.RS16_8_8) ->
      // With RS(16,8,8), we can tolerate up to (16 - 8) / 2 = 4 (8-bit) symbols
      // per each group of 16 (8-bit) symbols. It means that we can correct up to
      // 4B out of 16B. Therefore, setting the *noiseByteLevel* to 5 or higher
      // will fail
      new CommChannel(RSParams.RS16_8_8, noiseByteLevel=4) ->
      new ECCDecoderTopModel(RSParams.RS16_8_8) ->
      new CREECDecryptHighModel ->
      new CREECStripperModel ->
      new CompressorModel(false)
    val out = model.processTransactions(theRepublic)
    assert(out == theRepublic)
  }
}
