package aes
import chisel3.tester._
import interconnect.CREECAgent._
import interconnect._
import org.scalatest.FlatSpec

class CREECBusAESSWTest extends FlatSpec {
  val data = Seq(CREECHighLevelTransaction(Seq(
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 3, 3, 2,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 3, 3, 2
  ).map(_.asInstanceOf[Byte]), 0x0))

  val encryptedData = Seq(CREECHighLevelTransaction(Seq(
    0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
    0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
    0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
    0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c
  ).map(_.asInstanceOf[Byte]), 0x0, encrypted = true))

  //Key is fixed in the SW model
  //TODO: Consider how to provide the model the key at time of test
  "AESSWModel" should "encrypt low" in {
    val busParams = BusParams.aes
    val aesEncryptModel =
      new CREECHighToLowModel(busParams) ->
      new CREECEncryptLowModel(busParams) ->
      new CREECLowToHighModel(busParams)

    val out = aesEncryptModel.processTransactions(data)
    assert(out == encryptedData)
  }

  "AESSWModel" should "decrypt low" in {
    val busParams = BusParams.aes
    val aesDecryptModel =
      new CREECHighToLowModel(busParams) ->
      new CREECDecryptLowModel(busParams) ->
      new CREECLowToHighModel(busParams)

    val out = aesDecryptModel.processTransactions(encryptedData)
    assert(out == data)
  }

  "AESSWModel" should "encrypt high" in {
    val aesEncryptModel = new CREECEncryptHighModel
    val out = aesEncryptModel.processTransactions(data)
    assert(out == encryptedData)
  }

  "AESSWModel" should "decrypt high" in {
    val aesDecryptModel = new CREECDecryptHighModel
    val out = aesDecryptModel.processTransactions(encryptedData)
    assert(out == data)
  }

  "AESSWModel encrypt->decrypt chain" should "compose" in {
    // Composed model
    val composedModel =
      new CREECEncryptHighModel ->
      new CREECDecryptHighModel

    val out = composedModel.processTransactions(data)
    assert(out == data)
  }
}

class CREECBusAESHWTest extends FlatSpec with ChiselScalatestTester {
  val data = Seq(
    0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
    0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
    0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
    0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c).map(_.asInstanceOf[Byte])

  "AESHWModel" should "match encryption with HLT" in {
    val txaction = Seq(CREECHighLevelTransaction(data, 0x0),
                       CREECHighLevelTransaction(data, 0x1)
    )
        
    // SW golden model
    val swModel = new CREECEncryptHighModel
    val outGold = swModel.processTransactions(txaction)

    // Encryption RTL model
    test(new AESTopCREECBus(BusParams.aes)) { c =>
      val driver = new CREECDriver(c.io.encrypt_slave, c.clock)
      val monitor = new CREECMonitor(c.io.encrypt_master, c.clock)
        
      driver.pushTransactions(txaction)
      
      c.clock.step(100) 

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outGold == out)
    }
  }

  "AESHWModel" should "match decryption with HLT" in {
    val txaction = Seq(CREECHighLevelTransaction(data, 0x0, encrypted = true),
                       CREECHighLevelTransaction(data, 0x1, encrypted = true)
    )
        
    // SW golden model
    val swModel = new CREECDecryptHighModel
    val outGold = swModel.processTransactions(txaction)

    // Decryption RTL model
    test(new AESTopCREECBus(BusParams.aes)) { c =>
      val driver = new CREECDriver(c.io.decrypt_slave, c.clock)
      val monitor = new CREECMonitor(c.io.decrypt_master, c.clock)
        
      driver.pushTransactions(txaction)
      
      c.clock.step(100) 

      val out = monitor.receivedTransactions.dequeueAll(_ => true)
      assert(outGold == out)
    }
  }

  "AESHWModel" should "loop" in {
    val txaction = Seq(CREECHighLevelTransaction(data, 0x0),
                       CREECHighLevelTransaction(data, 0x1)
    )

    test(new AESTopCREECBus(BusParams.aes)) { c =>
      val encDriver = new CREECDriver(c.io.encrypt_slave, c.clock)
      val encMonitor = new CREECMonitor(c.io.encrypt_master, c.clock)

      val decDriver = new CREECDriver(c.io.decrypt_slave, c.clock)
      val decMonitor = new CREECMonitor(c.io.decrypt_master, c.clock)

      encDriver.pushTransactions(txaction)

      c.clock.step(100)

      val mid  = encMonitor.receivedTransactions.dequeueAll(_ => true)
      decDriver.pushTransactions(mid)

      c.clock.step(100)
      val out = decMonitor.receivedTransactions.dequeueAll(_ => true)
      assert(out == txaction)
    }
  }
}
