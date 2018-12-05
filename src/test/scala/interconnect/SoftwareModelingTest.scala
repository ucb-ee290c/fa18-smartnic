package interconnect

import org.scalatest.FlatSpec

class SoftwareModelingTest extends FlatSpec {
  // Shared BusParams for all the tests below
  implicit val busParams: BusParams = BusParams.creecInterleave

  // High-level transactions for testing
  val highTxs = Seq(
    CREECHighLevelTransaction(Seq(
      12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27
    ), 0x0),
    CREECHighLevelTransaction(Seq(
      1, 2, 3, 4, 5, 6, 7, 8
    ), 0x1000))

  // The equivalent low-level transactions
  val lowTxs = Seq(
    CREECHeaderBeat(1, 0, 0x0),
    CREECDataBeat(Seq(12, 13, 14, 15, 16, 17, 18, 19), 0),
    CREECDataBeat(Seq(20, 21, 22, 23, 24, 25, 26, 27), 0),
    CREECHeaderBeat(0, 0, 0x1000),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), 0)
  )

  // The equivalent low-level transactions (with data interleaving, and multiple IDs)
  val lowTxsInterleaved = Seq(
    CREECHeaderBeat(1, 1, 0x0),
    CREECHeaderBeat(0, 2, 0x1000),
    CREECDataBeat(Seq(12, 13, 14, 15, 16, 17, 18, 19), 1),
    CREECDataBeat(Seq(1, 2, 3, 4, 5, 6, 7, 8), 2),
    CREECDataBeat(Seq(20, 21, 22, 23, 24, 25, 26, 27), 1)
  )

  "the High2Low model" should "translate correctly" in {
    val model = new CREECHighToLowModel(busParams)
    val out = model.processTransactions(highTxs)
    assert(out == lowTxs)
  }

  "the Low2High model" should "translate correctly with no interleaving" in {
    val model = new CREECLowToHighModel(busParams)
    val out = model.processTransactions(lowTxs)
    assert(out == highTxs)
  }

  "the Low2High model" should "translate correctly with interleaving" in {
    val model = new CREECLowToHighModel(busParams)
    val out = model.processTransactions(lowTxsInterleaved)
    // Cast Seq to Set because we don't care about ordering
    assert(Set(out:_*) == Set(highTxs:_*))
  }

  "the High2Low -> Low2High composed model" should "act as an identity transform" in {
    val model = new CREECHighToLowModel(busParams) -> new CREECLowToHighModel(busParams)
    val out = model.processTransactions(highTxs)
    assert(out == highTxs)
  }

  "the High2Low -> Low2High -> High2Low composed model" should "all compose correctly" in {
    val model =
      new CREECHighToLowModel(busParams) ->
      new CREECLowToHighModel(busParams) ->
      new CREECHighToLowModel(busParams)
    val out = model.processTransactions(highTxs)
    assert(out == lowTxs)
  }

  "the Low2High -> High2Low -> Low2High composed model" should "all compose correctly" in {
    val model =
      new CREECLowToHighModel(busParams) ->
      new CREECHighToLowModel(busParams) ->
      new CREECLowToHighModel(busParams)
    val out = model.processTransactions(lowTxsInterleaved)
    // Again casting to Set since ordering is expected to be irrelevant
    assert(Set(out:_*) == Set(highTxs:_*))
  }
}
