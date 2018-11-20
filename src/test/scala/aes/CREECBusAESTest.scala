package aes
import chisel3._
import chisel3.tester._
import interconnect.CREECAgent._
import interconnect.{SoftwareModel, CREECLowLevelTransaction, BusParams, CREECHeaderBeat, CREECDataBeat}
import org.scalatest.FlatSpec

class CREECBusAESTest extends FlatSpec with ChiselScalatestTester {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  //Key is fixed in the SW model
  //TODO: Consider how to provide the model the key at time of test

  "AESSWModel" should "encrypt" in {
    implicit val busParams: BusParams = new AESBusParams
    val model = new CREECEncryptModel(busParams)
    val id = 0
    //Index 0 of the seq is the LSB
    val data = Seq(1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 3, 3, 2).map(
                _.asInstanceOf[Byte])

    val encrypted_data = Seq(0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
                        0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(
                        _.asInstanceOf[Byte])

    val out = model.pushTransactions(Seq(
      CREECHeaderBeat(len=1, id=id, addr=0x0),
      CREECDataBeat(data= data, id=id),
    )).advanceSimulation(true).pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(len=1, id=id, addr=0x0),
      CREECDataBeat(data=encrypted_data, id=id),
    )
    assert(outGold == out)
  }

  "AESSWModel" should "decrypt" in {
    implicit val busParams: BusParams = new AESBusParams
    val model = new CREECDecryptModel(busParams)
    val id = 0
    //Index 0 of the seq is the LSB
    val decrypted_data = Seq(1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 3, 3, 2).map(
      _.asInstanceOf[Byte])

    val data = Seq(0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(
      _.asInstanceOf[Byte])

    val out = model.pushTransactions(Seq(
      CREECHeaderBeat(len=1, id=id, addr=0x0),
      CREECDataBeat(data= data, id=id),
    )).advanceSimulation(true).pullTransactions()
    val outGold = Seq(
      CREECHeaderBeat(len=1, id=id, addr=0x0),
      CREECDataBeat(data=decrypted_data, id=id),
    )
    assert(outGold == out)
  }
}
