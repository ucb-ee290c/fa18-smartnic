package aes

//run testOnly *AESTop* -- -z AESTop

import org.scalatest.{FlatSpec, Matchers}

class AESTopSpec extends FlatSpec with Matchers {
  behavior of "AESTop"

  it should "encrypt and decrypt" in {
    val data = (BigInt(0x0203030101010101L) << 64) + BigInt(0x0101010101010101L)
    val ref_out = (BigInt(0x3c5cd4828515ad38L) << 64) + (BigInt(0xe12659ceL) << 32) + BigInt(0xd16b2314L)
    val trial = AESTopTrial(data_in=data, key_in=data, ref_out=ref_out, inv_data_in=ref_out, inv_ref_out=data)
//    AESTopCombinationalTester(trial) should be (true)
//    AESTopTimeInterleaveTester(trial) should be (true)
    AESTopFullTimeInterleaveTester(trial) should be (true)
  }
}
