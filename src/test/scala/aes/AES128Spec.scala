package aes

import org.scalatest.{FlatSpec, Matchers}

class AES128Spec extends FlatSpec with Matchers {
  behavior of "AES128"

  it should "encrypt" in {
    val data = (BigInt(0x0101010101010101L) << 64) + BigInt(0x0101010101030302L)
    val trial = AESTrial(data_in=data, key_in=data)
    AES128Tester(trial) should be (true)
    AES128CombinationalTester(trial) should be (true)
  }
}
