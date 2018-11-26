package aes
import chisel3._
import chisel3.tester._
import interconnect.CREECAgent._
import interconnect.{SoftwareModel, CREECHighLevelTransaction, CREECLowLevelTransaction, BusParams, CREECHeaderBeat, CREECDataBeat}
import org.scalatest.FlatSpec

// TODO: move the common hand-written data stimulus and golden reference to a common object
class CREECBusAESSWTest extends FlatSpec {
  //Key is fixed in the SW model
  //TODO: Consider how to provide the model the key at time of test
  "AESSWModel" should "encrypt low" in {
    implicit val busParams = new AESBusParams
    val aesEncryptModel = new CREECEncryptLowModel(busParams)
    val data = Seq(
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 3, 3, 2).map(_.asInstanceOf[Byte])

    val encryptedData = Seq(
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(_.asInstanceOf[Byte])

    val out = aesEncryptModel.processTransactions(Seq(
      CREECHeaderBeat(len=1, id=0, addr=0x0),
      CREECDataBeat(data=data, id=0),
    ))
    val outGold = Seq(
      CREECHeaderBeat(len=1, id=0, addr=0x0, encrypted=true),
      CREECDataBeat(data=encryptedData, id=0),
    )
    assert(outGold == out)
  }

  "AESSWModel" should "decrypt low" in {
    implicit val busParams = new AESBusParams
    val aesDecryptModel = new CREECDecryptLowModel(busParams)
    val decryptedData = Seq(
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 3, 3, 2).map(_.asInstanceOf[Byte])

    val data = Seq(
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(_.asInstanceOf[Byte])

    val out = aesDecryptModel.processTransactions(Seq(
      CREECHeaderBeat(len=1, id=0, addr=0x0),
      CREECDataBeat(data= data, id=0),
    ))
    val outGold = Seq(
      CREECHeaderBeat(len=1, id=0, addr=0x0),
      CREECDataBeat(data=decryptedData, id=0),
    )
    assert(outGold == out)
  }

  "AESSWModel" should "encrypt high" in {
    val aesEncryptModel = new CREECEncryptHighModel
    val data = Seq(
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 3, 3, 2,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 3, 3, 2).map(_.asInstanceOf[Byte])

    val encryptedData = Seq(
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(_.asInstanceOf[Byte])

    val out = aesEncryptModel.processTransactions(Seq(CREECHighLevelTransaction(data = data, addr = 0x0)))
    val outGold = Seq(CREECHighLevelTransaction(data = encryptedData, addr = 0x0, encrypted = true))
    assert(outGold == out)
  }

  "AESSWModel" should "decrypt high" in {
    val aesDecryptModel = new CREECDecryptHighModel
    val decryptedData = Seq(
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 3, 3, 2,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 3, 3, 2).map(_.asInstanceOf[Byte])

    val data = Seq(
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(_.asInstanceOf[Byte])

    val out = aesDecryptModel.processTransactions(Seq(CREECHighLevelTransaction(data = data, addr = 0x0, encrypted = true)))
    val outGold = Seq(CREECHighLevelTransaction(data = decryptedData, addr = 0x0, encrypted = false))
    assert(outGold == out)
  }

  "AESSWModel encrypt->decrypt chain" should "compose" in {
    val data = Seq(
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(_.asInstanceOf[Byte])

    // Composed model
    val composedModel =
      new CREECEncryptHighModel ->
      new CREECDecryptHighModel

    val out = composedModel.processTransactions(Seq(CREECHighLevelTransaction(data = data, addr = 0x0)))
    val outGold = Seq(CREECHighLevelTransaction(data = data, addr = 0x0))
    assert(outGold == out)
  }
}

/*
class CREECBusAESHWTest extends FlatSpec with ChiselScalatestTester {
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "verilator",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  //Key is fixed in the SW model
  //TODO: Consider how to provide the model the key at time of test

  "AESHWModel" should "match encryption with HLT" in {
    implicit val busParams: BusParams = new AESBusParams
    
    
    val data = Seq(0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(
      _.asInstanceOf[Byte])
    val txaction = Seq(CREECHighLevelTransaction(data, 0x0))
        
    //SW golden model
    val swModel = new CREECEncryptHighModel(busParams)
    val outGold = swModel.pushTransactions(txaction).advanceSimulation(true).pullTransactions()

    //Encryption
    test(new AESTopCREECBus(busParams)) { c=>
      val driver = new CREECDriver(c.io.encrypt_slave, c.clock)
      val monitor = new CREECMonitor(c.io.encrypt_master, c.clock)
        
      driver.pushTransactions(txaction)
      
      c.clock.step(60) 

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outGold == out)
    }
  }

  "AESHWModel" should "match decryption with HLT" in {
    implicit val busParams: BusParams = new AESBusParams
    
    
    val data = Seq(0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(
      _.asInstanceOf[Byte])
    val txaction = Seq(CREECHighLevelTransaction(data, 0x0))
        
    //SW golden model
    val swModel = new CREECDecryptHighModel(busParams)
    val outGold = swModel.pushTransactions(txaction).advanceSimulation(true).pullTransactions()

    test(new AESTopCREECBus(busParams)) { c=>
      val driver = new CREECDriver(c.io.decrypt_slave, c.clock)
      val monitor = new CREECMonitor(c.io.decrypt_master, c.clock)
        
      driver.pushTransactions(txaction)
      
      c.clock.step(60) 

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outGold == out)
    }
  }
  
  "AESHWModel" should "loop" in {
    implicit val busParams: BusParams = new AESBusParams 
    
    val data = Seq(0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
      0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
      0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(
      _.asInstanceOf[Byte])
    val txaction = Seq(CREECHighLevelTransaction(data, 0x0))
        
    test(new AESTopCREECBus(busParams)) { c=>
      val e_driver = new CREECDriver(c.io.encrypt_slave, c.clock)
      val e_monitor = new CREECMonitor(c.io.encrypt_master, c.clock)

      val d_driver = new CREECDriver(c.io.decrypt_slave, c.clock)
      val d_monitor = new CREECMonitor(c.io.decrypt_master, c.clock)
        
      e_driver.pushTransactions(txaction)
      
      c.clock.step(60) 

      val mid  = e_monitor.receivedTransactions.dequeueAll(_ => true)
      d_driver.pushTransactions(mid)

      c.clock.step(60)
      val out = d_monitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == txaction)
    }
  }
}
*/
