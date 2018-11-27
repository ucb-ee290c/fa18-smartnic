package compression

import chisel3.tester.ChiselScalatestTester
import interconnect.{BusParams, CREECBusParams, CREECHighLevelTransaction, SoftwareModel}
import org.scalatest.FlatSpec

class CREECCompressionModelTester extends FlatSpec with ChiselScalatestTester {
  def verifyModel[T <: SoftwareModel[CREECHighLevelTransaction,
    CREECHighLevelTransaction]](DUTFactory: Boolean => T,
                                uncodedGold: Seq[CREECHighLevelTransaction],
                                encodedGold: Seq[CREECHighLevelTransaction]): Unit = {
    implicit val busParams: BusParams = new CREECBusParams
    val encodeModel = DUTFactory(true)
    val decodeModel = DUTFactory(false)
    val encoded = encodeModel.pushTransactions(uncodedGold)
      .advanceSimulation(true).pullTransactions()
    assert(encodedGold == encoded)
    val decoded = decodeModel.pushTransactions(encodedGold)
      .advanceSimulation(true).pullTransactions()
    assert(decoded == uncodedGold)
  }

  "the CREECDifferentialCoderModel" should "encode and decode data" in {
    verifyModel[CREECDifferentialCoderModel](
      { x => new CREECDifferentialCoderModel(x) },
      Seq(
        CREECHighLevelTransaction(
          Seq(
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15, 16, 17, 18,
            19, 20, 21, 22, 23, 24
          ),
          0x501
        ),
        CREECHighLevelTransaction(
          Seq(
            0, 0, 0, 0, 0, 0, 54, 16, 27, 83,
            83, 83, 83, 83, 83, 83
          ),
          0x1
        )
      ),
      Seq(
        CREECHighLevelTransaction(
          Seq(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1
          ),
          0x501
        ),
        CREECHighLevelTransaction(
          Seq(
            0, 0, 0, 0, 0, 0, 54, -38, 11, 56,
            0, 0, 0, 0, 0, 0
          ),
          0x1
        )
      )
    )
  }

  "the CREECRunLengthCoderModel" should "encode and decode data" in {
    verifyModel[CREECRunLengthCoderModel](
      { x => new CREECRunLengthCoderModel(x) },
      Seq(
        CREECHighLevelTransaction(
          Seq(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1
          ),
          0x501
        ),
        CREECHighLevelTransaction(
          Seq(
            0, 0, 0, 0, 0, 0, 54, -38, 11, 56,
            0, 0, 0, 0, 0, 0
          ),
          0x1
        )
      ),
      Seq(
        CREECHighLevelTransaction(
          Seq(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1
          ),
          0x501
        ),
        CREECHighLevelTransaction(
          Seq(
            0, 5, 54, -38, 11, 56,
            0, 5
          ),
          0x1
        )
      )
    )
  }

  "the CompressorModel" should "compress and uncompress data" in {
    verifyModel[CompressorModel](
      { x => new CompressorModel(x) },
      Seq(
        CREECHighLevelTransaction(
          Seq(
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15, 16, 17, 18,
            19, 20, 21, 22, 23, 24
          ),
          0x501
        ),
        CREECHighLevelTransaction(
          Seq(
            0, 0, 0, 0, 0, 0, 54, 16, 27, 83,
            83, 83, 83, 83, 83, 83
          ),
          0x1
        ),
        // Test zero padding bytes
        CREECHighLevelTransaction(
          Seq(
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1
          ),
          0x1000
        )
      ),
      Seq(
        CREECHighLevelTransaction(
          Seq(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1
          ),
          0x501, compressed = true
        ),
        CREECHighLevelTransaction(
          Seq(
            0, 5, 54, -38, 11, 56,
            0, 5
          ),
          0x1, compressed = true
        ),
        CREECHighLevelTransaction(
          Seq(
            0, 15, 1, 0, 14, 0, 0, 0
          ),
          0x1000, compressed = true, compressionPadBytes = 3
        )
      )
    )
  }
}