package aes

//run testOnly *Sub* -- -z InvSub

import org.scalatest.{FlatSpec, Matchers}

class InvSubByteSpec extends FlatSpec with Matchers {
  behavior of "InvSubByte"

  it should "Invsubbyte" in {
    InvSubByteTester() should be (true)
  }
}

class InvSubMMSpec extends FlatSpec with Matchers {
  behavior of "InvSubMM"

  it should "InvsubMM" in {
    InvSubMMTester() should be (true)
  }
}

class InvSubCipher extends FlatSpec with Matchers {
  behavior of "InvSubcipher"

  it should "Invsubcipher" in {
    InvSubCipherTester() should be (true)
  }
}

/*
class SubStage extends FlatSpec with Matchers {
  behavior of "SubStage"

  it should "subStage" in {
    SubStageTester() should be (true)
  }
}
*/