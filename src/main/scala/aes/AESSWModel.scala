package aes

import interconnect._

//TODO: test-time key updates

// Based on https://gist.github.com/alexandru/ac1c01168710786b54b0
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Base64

object AESEncryption {
  def encrypt(key: Seq[Byte], value: Seq[Byte]): Seq[Byte] = {
    val cipher: Cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, keyToSpec(key))
    cipher.doFinal(value.toArray[Byte]).toSeq
  }

  def decrypt(key: Seq[Byte], encryptedValue: Seq[Byte]): Seq[Byte] = {
    val cipher: Cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, keyToSpec(key))
    cipher.doFinal(encryptedValue.toArray[Byte]).toSeq
  }

  private def keyToSpec(key: Seq[Byte]): SecretKeySpec = {
    val keyBytes: Array[Byte] = key.toArray[Byte]
    new SecretKeySpec(keyBytes, "AES")
  }
}

class AESBusParams extends BusParams(maxBeats = 128, maxInFlight=1, dataWidth=128)

//Note: a combined encrypt-decrypt SW unit is not necessary since
// the decrypt and encrypt are isolated at the bus level
class CREECEncryptLowModel(p: AESBusParams) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction]
  with HWKey {
  override def process(in: CREECLowLevelTransaction) : Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat => //passthrough
        Seq(t.copy(encrypted = true)(p))
      case t: CREECDataBeat => //encrypt
        Seq(t.copy(data = AESEncryption.encrypt(key, t.data))(p))
    }
  }
}

class CREECDecryptLowModel(p: AESBusParams) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction]
  with HWKey {
  override def process(in: CREECLowLevelTransaction) : Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat => //passthrough
        Seq(t.copy(encrypted = false)(p))
      case t: CREECDataBeat => //decrypt
        Seq(t.copy(data = AESEncryption.decrypt(key, t.data))(p))
    }
  }
}

// TODO: a custom encryption key can just be a constructor parameter for this class (with a default)
class CREECEncryptHighModel extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction]
  with HWKey {
  override def process(in: CREECHighLevelTransaction) : Seq[CREECHighLevelTransaction] = {
    assert(in.data.length % 16 == 0, "Encryption model expects data aligned on AES block size = 16 bytes")
    val encryptedData = AESEncryption.encrypt(key, in.data)
    Seq(in.copy(data = encryptedData, encrypted = true))
  }
}

class CREECDecryptHighModel extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction]
  with HWKey {
  override def process(in: CREECHighLevelTransaction) : Seq[CREECHighLevelTransaction] = {
    assert(in.data.length % 16 == 0, "Decryption model expects data aligned on AES block size = 16 bytes")
    val decryptedData = AESEncryption.decrypt(key, in.data)
    Seq(in.copy(data = decryptedData, encrypted = false))
  }
}