package interconnect

import aes.AESBusParams
import org.scalatest.FlatSpec

class CREECWidthConverterTest extends FlatSpec {
  val busParamsBase = new CREECBusParams
  val busParamsExpand2 = new AESBusParams

  val testTx = Seq(
    CREECHeaderBeat(len = 1, id = 0x0, addr = 0x0)(busParamsBase),
    CREECHeaderBeat(len = 1, id = 0x1, addr = 0x0)(busParamsBase),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), id = 0x0)(busParamsBase),
    CREECDataBeat(Seq(9, 10, 11, 12, 13, 14, 15, 16), id = 0x0)(busParamsBase),
    CREECDataBeat(Seq(1, 1, 1, 1, 1, 1, 1, 1), id = 0x1)(busParamsBase),
    CREECDataBeat(Seq(2, 2, 2, 2, 2, 2, 2, 2), id = 0x1)(busParamsBase)
  )

  val outGoldExpand2 = Seq(
    CREECHeaderBeat(len = 0, id = 0x0, addr = 0x0)(busParamsExpand2),
    CREECHeaderBeat(len = 0, id = 0x1, addr = 0x0)(busParamsExpand2),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), id = 0x0)(busParamsExpand2),
    CREECDataBeat(Seq(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2), id = 0x1)(busParamsExpand2)
  )

  val testTxUnaligned = Seq(
    CREECHeaderBeat(len = 2, id = 0x0, addr = 0x0)(busParamsBase),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), id = 0x0)(busParamsBase),
    CREECDataBeat(Seq(9, 10, 11, 12, 13, 14, 15, 16), id = 0x0)(busParamsBase),
    CREECDataBeat(Seq(17, 18, 19, 20, 21, 22, 23, 24), id = 0x0)(busParamsBase),
  )

  val outGoldExpand2Unaligned = Seq(
    CREECHeaderBeat(len = 1, id = 0x0, addr = 0x0, encryptionPadBytes = 8)(busParamsExpand2),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), id = 0x0)(busParamsExpand2),
    CREECDataBeat(Seq(17, 18, 19, 20, 21, 22, 23, 24, 0, 0, 0, 0, 0, 0, 0, 0), id = 0x0)(busParamsExpand2),
  )

  behavior of "SW Width Converter"
  it should "pass through transactions when ratio = 1" in {
    val model = new CREECWidthConverterModel(busParamsBase, busParamsBase)
    val out = model.processTransactions(testTx)
    assert(out == testTx)
  }

  it should "expand by a factor of 2 when ratio = 2" in {
    val model = new CREECWidthConverterModel(busParamsBase, busParamsExpand2)
    val out = model.processTransactions(testTx)
    assert(out == outGoldExpand2)
  }

  it should "error out on unaligned inputs when ratio = 2" in {
    val model = new CREECWidthConverterModel(busParamsBase, busParamsExpand2)
    assertThrows[AssertionError] {
      model.processTransactions(testTxUnaligned)
    }
  }

  it should "contract by a factor of 2 when ratio = 2" in {
    val model = new CREECWidthConverterModel(busParamsExpand2, busParamsBase)
    val out = model.processTransactions(outGoldExpand2)
    println(out)
    assert(out == testTx)
  }
}
