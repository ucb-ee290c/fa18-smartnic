package interconnect

import aes.{AESTopCREECBus, CREECDecryptHighModel, CREECEncryptHighModel}
import chisel3.tester._
import compression.{Compressor, CompressorModel}
import ecc._
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}
import org.scalatest.FlatSpec

class RepublicTest extends FlatSpec with ChiselScalatestTester {
  implicit val creecParams: CREECBusParams = new CREECBusParams

  def readTxFromFileFirst512(file: String): Seq[CREECHighLevelTransaction] = {
    import scala.io.Source
    val fileBytes = Source.fromResource(file).toList.map(_.asInstanceOf[Byte]).take(512)
    val fileBytesPadded = fileBytes.padTo(math.ceil(fileBytes.length / 8.0).toInt * 8, 0.asInstanceOf[Byte])
    val dataPackets = fileBytesPadded.grouped(512)
    dataPackets.zipWithIndex.map {
      case (data, i) => CREECHighLevelTransaction(data = data, addr = i)
    }.toSeq
  }

  val theRepublic: Seq[CREECHighLevelTransaction] = readTxFromFileFirst512("The_Republic_Plato.txt")
  val theRepublicC: Seq[CREECHighLevelTransaction] = new CompressorModel(true).processTransactions(theRepublic)
  val theRepublicCP: Seq[CREECHighLevelTransaction] = new CREECPadderModel(16).processTransactions(theRepublicC)
  val theRepublicCPE: Seq[CREECHighLevelTransaction] = new CREECEncryptHighModel().processTransactions(theRepublicCP)
  val theRepublicCPEE: Seq[CREECHighLevelTransaction] = new ECCEncoderTopModel(RSParams.RS16_8_8).processTransactions(theRepublicCPE)
  val cilbuperEhtCPEE: Seq[CREECHighLevelTransaction] = new CommChannel(RSParams.RS16_8_8, noiseByteLevel = 4).processTransactions(theRepublicCPEE)
  val cilbuperEhtCPE: Seq[CREECHighLevelTransaction] = new ECCDecoderTopModel(RSParams.RS16_8_8).processTransactions(cilbuperEhtCPEE)
  val cilbuperEhtCP: Seq[CREECHighLevelTransaction] = new CREECDecryptHighModel().processTransactions(cilbuperEhtCPE)
  val cilbuperEhtC: Seq[CREECHighLevelTransaction] = new CREECStripperModel().processTransactions(cilbuperEhtCP)
  val cilbuperEht: Seq[CREECHighLevelTransaction] = new CompressorModel(false).processTransactions(cilbuperEhtC)
  assert(cilbuperEht == theRepublic)

  val theRepublicEnc: Seq[CREECHighLevelTransaction] = new CREECEncryptHighModel().processTransactions(theRepublic)
  val theRepublicECC: Seq[CREECHighLevelTransaction] = new ECCEncoderTopModel(RSParams.RS16_8_8).processTransactions(theRepublic)

  /*
   * Use this flag to control whether standalone cycle counts or
   * full-stack cycle counts are produced.
   */
  val standalone = true

  "this compress test" should "produce a cycle count when run" in {
    test(new Compressor(BusParams.blockDev, true)) { c =>
      val model = new CompressorModel(compress = true)
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      TesterUtils.runTest(c, model, driver, monitor, theRepublic)
    }
  }

  "this encrypt test" should "produce a cycle count when run" in {
    test(new AESTopCREECBus(BusParams.aes)) { c =>
      val model = new CREECEncryptHighModel
      val driver = new CREECDriver(c.io.encrypt_slave, c.clock)
      val monitor = new CREECMonitor(c.io.encrypt_master, c.clock)
      TesterUtils.runTest(c, model, driver, monitor, if(standalone) theRepublic else theRepublicCP)
    }
  }

  "this ECC test" should "produce a cycle count when run" in {
    test(new ECCEncoderTop(RSParams.RS16_8_8, creecParams, new ECCBusParams)) { c =>
      val model = new ECCEncoderTopModel(RSParams.RS16_8_8)
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)
      TesterUtils.runTest(c, model, driver, monitor, if(standalone) theRepublic else theRepublicCPE)
    }
  }

  "this unECC test" should "produce a cycle count when run" in {
    test(new ECCDecoderTop(RSParams.RS16_8_8, new ECCBusParams, creecParams)) { c =>
      val model = new ECCDecoderTopModel(RSParams.RS16_8_8)
      val driver = new CREECDriver(c.io.slave, c.clock)
      val monitor = new CREECMonitor(c.io.master, c.clock)
      TesterUtils.runTest(c, model, driver, monitor, if(standalone) theRepublicECC else cilbuperEhtCPEE, timeout = 10000)
    }
  }

  "this decrypt test" should "produce a cycle count when run" in {
    test(new AESTopCREECBus(BusParams.aes)) { c =>
      val model = new CREECDecryptHighModel
      val driver = new CREECDriver(c.io.decrypt_slave, c.clock)
      val monitor = new CREECMonitor(c.io.decrypt_master, c.clock)
      TesterUtils.runTest(c, model, driver, monitor, if(standalone) theRepublicEnc else cilbuperEhtCPE)
    }
  }

  "this decompress test" should "produce a cycle count when run" in {
    test(new Compressor(BusParams.creec, false)) { c =>
      val model = new CompressorModel(false)
      val driver = new CREECDriver(c.io.in, c.clock)
      val monitor = new CREECMonitor(c.io.out, c.clock)
      println(cilbuperEhtC)
      println(model.processTransactions(cilbuperEhtC))
      TesterUtils.runTest(c, model, driver, monitor, if(standalone) theRepublicC else cilbuperEhtC)
    }
  }
}