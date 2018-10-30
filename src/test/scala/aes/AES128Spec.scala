package aes

//run testOnly *AES* -- -z AES

import org.scalatest.{FlatSpec, Matchers}

class AES128Spec extends FlatSpec with Matchers {
  behavior of "AES128"

  it should "encrypt" in {
    val data = (BigInt(0x0203030101010101L) << 64) + BigInt(0x0101010101010101L)
    val ref_out = (BigInt(0x3c5cd4828515ad38L) << 64) + (BigInt(0xe12659ceL) << 32) + BigInt(0xd16b2314L)
    val trial = AESTrial(data_in=data, key_in=data, ref_out=ref_out)
    AES128CombinationalTester(trial) should be (true)
    AES128Tester(trial) should be (true)
  }
}
