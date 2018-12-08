package aes

//run testOnly *NIST* -- -z NIST

import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source
import scala.collection.mutable.ListBuffer

class AESNISTSpec extends FlatSpec with Matchers {
  behavior of "AESNIST"
  it should "encrypt and decrypt" in {

    //Reorder bytes and Convert to BigInt
    def convert(s: String): BigInt = {
      var post = ""
      for (i <- 0 until 16) {
        post = s.slice(2*i, 2*i + 2).concat(post)
      }
      BigInt(post, 16)
    }

    //Prepare the trials. We expect 568 tests
    val trials = new ListBuffer[AESNISTTrial]()
    val file_list = Seq(
      "KAT_AES/ECBGFSbox128.rsp",
      "KAT_AES/ECBKeySbox128.rsp",
      "KAT_AES/ECBVarKey128.rsp",
      "KAT_AES/ECBVarTxt128.rsp")

    for (filename <- file_list) {
      val lines = Source.fromResource(filename).getLines.toList
      var state = 0

      val encrypt_key = new ListBuffer[BigInt]()
      val encrypt_text = new ListBuffer[BigInt]()
      val encrypt_ref = new ListBuffer[BigInt]()

      val decrypt_key = new ListBuffer[BigInt]()
      val decrypt_cipher = new ListBuffer[BigInt]()
      val decrypt_ref = new ListBuffer[BigInt]()

      //Parsing state machine
      for (i <- 0 until lines.length) {
        val curr = lines(i)

        if (curr.contains("ENCRYPT")) {
          state = 1
        }
        else if (curr.contains("DECRYPT")) {
          state = 2
        }
        else if (curr.contains("KEY")) {
          val key = curr.split(" ")(2)
          if (state == 1) encrypt_key += convert(key)
          if (state == 2) decrypt_key += convert(key)
        }
        else if (curr.contains("CIPHERTEXT")) {
          val cipher = curr.split(" ")(2)
          if (state == 1) encrypt_ref += convert(cipher)
          if (state == 2) decrypt_cipher += convert(cipher)
        }
        else if (curr.contains("PLAINTEXT")) {
          val plain = curr.split(" ")(2)
          if (state == 1) encrypt_text += convert(plain)
          if (state == 2) decrypt_ref += convert(plain)
        }
      }

      assert(encrypt_key.length == encrypt_ref.length)
      assert(encrypt_key.length == encrypt_text.length)
      assert(decrypt_key.length == decrypt_ref.length)
      assert(decrypt_key.length == decrypt_cipher.length)

      //Create trials
      for (i <- 0 until encrypt_key.length) {
        trials += AESNISTTrial(
          key_in = encrypt_key(i),
          data_in = encrypt_text(i),
          ref_out = encrypt_ref(i),
          inv_data_in = encrypt_ref(i),
          inv_ref_out = encrypt_text(i))
      }
      for (i <- 0 until decrypt_key.length) {
        trials += AESNISTTrial(
          key_in = decrypt_key(i),
          data_in = decrypt_ref(i),
          ref_out = decrypt_cipher(i),
          inv_data_in = decrypt_cipher(i),
          inv_ref_out = decrypt_ref(i))
      }
    }
    val trials_seq = trials.toSeq
    AESNISTFullTimeInterleaveTester(trials_seq) should be(true)
  }
}
