package compression

import chisel3.tester.ChiselScalatestTester
import interconnect.{BusParams, CREECBusParams, CREECHighLevelTransaction}
import org.scalatest.FlatSpec

class CREECCompressionTest extends FlatSpec with ChiselScalatestTester {
  "the CREECDifferentialCoderModel" should "encode and decode data" in {
    implicit val busParams: BusParams = new CREECBusParams
    val encodeModel = new CREECDifferentialCoderModel(true)
    val decodeModel = new CREECDifferentialCoderModel(false)
    val uncodedGold = Seq(
      CREECHighLevelTransaction(Seq(
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
        11, 12, 13, 14, 15, 16, 17, 18,
        19, 20, 21, 22, 23, 24
      ), 0x501
      ),
      CREECHighLevelTransaction(Seq(
        0, 0, 0, 0, 0, 0, 54, 16, 27, 83,
        83, 83, 83, 83, 83, 83
      ), 0x1
      )
    )
    val encoded = encodeModel.pushTransactions(uncodedGold)
      .advanceSimulation(true).pullTransactions()
    val encodedGold = Seq(
      CREECHighLevelTransaction(Seq(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1
      ), 0x501
      ),
      CREECHighLevelTransaction(Seq(
        0, 0, 0, 0, 0, 0, 54, -38, 11, 56,
        0, 0, 0, 0, 0, 0
      ), 0x1
      )
    )
    val decoded = decodeModel.pushTransactions(encodedGold)
      .advanceSimulation(true).pullTransactions()
    assert(encodedGold == encoded)
    assert(decoded == uncodedGold)
  }

  "the CREECRunLengthCoderModel" should "encode and decode data" in {
    implicit val busParams: BusParams = new CREECBusParams
    val encodeModel = new CREECRunLengthCoderModel(true)
    val decodeModel = new CREECRunLengthCoderModel(false)
    val uncodedGold = Seq(
      CREECHighLevelTransaction(Seq(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1
      ), 0x501
      ),
      CREECHighLevelTransaction(Seq(
        0, 0, 0, 0, 0, 0, 54, -38, 11, 56,
        0, 0, 0, 0, 0, 0
      ), 0x1
      )
    )
    val encoded = encodeModel.pushTransactions(uncodedGold)
      .advanceSimulation(true).pullTransactions()
    val encodedGold = Seq(
      CREECHighLevelTransaction(Seq(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1
      ), 0x501
      ),
      CREECHighLevelTransaction(Seq(
        0, 5, 54, -38, 11, 56,
        0, 5
      ), 0x1
      )
    )
    val decoded = decodeModel.pushTransactions(encodedGold)
      .advanceSimulation(true).pullTransactions()
    assert(encodedGold == encoded)
    assert(decoded == uncodedGold)
  }

  "the CompressorModel" should "compress and uncompress data" in {
    implicit val busParams: BusParams = new CREECBusParams
    val compressModel = new CompressorModel(true)
    val decompressModel = new CompressorModel(false)
    val uncompressedGold = Seq(
      CREECHighLevelTransaction(Seq(
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
        11, 12, 13, 14, 15, 16, 17, 18,
        19, 20, 21, 22, 23, 24
      ), 0x501
      ),
      CREECHighLevelTransaction(Seq(
        0, 0, 0, 0, 0, 0, 54, 16, 27, 83,
        83, 83, 83, 83, 83, 83
      ), 0x1
      )
    )
    val compressed = compressModel.pushTransactions(uncompressedGold)
      .advanceSimulation(true).pullTransactions()
    val compressedGold = Seq(
      CREECHighLevelTransaction(Seq(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1
      ), 0x501
      ),
      CREECHighLevelTransaction(Seq(
        0, 5, 54, -38, 11, 56,
        0, 5
      ), 0x1
      )
    )
    val decompressed = decompressModel.pushTransactions(compressedGold)
      .advanceSimulation(true).pullTransactions()
    assert(compressedGold == compressed)
    assert(decompressed == uncompressedGold)
  }
}