package aes

import scala.collection.mutable
import java.nio._
import interconnect.{SoftwareModel, CREECHighLevelTransaction, CREECLowLevelTransaction, BusParams, CREECHeaderBeat, CREECDataBeat}
import interconnect.CREECAgent.{CREECDriver, CREECMonitor}

//TODO: test-time key updates

// Based on https://gist.github.com/alexandru/ac1c01168710786b54b0
import java.security.MessageDigest
import java.util
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Base64

class AESEncryption {
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

  def keyToSpec(key: Seq[Byte]): SecretKeySpec = {
    var keyBytes: Array[Byte] = key.toArray[Byte]
    new SecretKeySpec(keyBytes, "AES")
  }
}

class AESBusParams extends BusParams(maxBeats = 128, maxInFlight=1, dataWidth=128)

//Note: a combined encrypt-decrypt SW unit is not necessary since
// the decrypt and encrypt are isolated at the bus level

//Forward the header beats, modify the data beats
class CREECEncryptLowModel(p: BusParams) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction]
  with HWKey {
  override def process(in: CREECLowLevelTransaction) : Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat => //passthrough
        Seq(t)
      case t: CREECDataBeat => { //encrypt
        val data = t.data
        val aes = new AESEncryption
        val en = aes.encrypt(key, data)
        val rr = new CREECDataBeat(en, t.id)(p)
        Seq(rr)
      }
      case _ => {
        assert(false, "How did you make it here?")
        Seq(in)
      }
    }
  }
}

class CREECDecryptLowModel(p: BusParams) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction]
  with HWKey {
  override def process(in: CREECLowLevelTransaction) : Seq[CREECLowLevelTransaction] = {
    in match {
      case t: CREECHeaderBeat => //passthrough
        Seq(t)
      case t: CREECDataBeat => { //decrypt
        val data = t.data
        val aes = new AESEncryption
        val de = aes.decrypt(key, data)
        val rr = new CREECDataBeat(de, t.id)(p)
        Seq(rr)
      }
      case _ => {
        assert(false, "How did you make it here?")
        Seq(in)
      }
    }
  }
}

class CREECEncryptHighModel(p: BusParams) extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction]
  with HWKey {
  override def process(in: CREECHighLevelTransaction) : Seq[CREECHighLevelTransaction] ={
    assert(in.data.length % p.bytesPerBeat == 0, "CREEC high transaction must have data with length = multiple of bus width")
    val dataBeats = in.data.grouped(p.bytesPerBeat).toSeq
    assert((dataBeats.length - 1) <= p.maxBeats, "CREEC high transaction has more beats than bus can support")

    val output = dataBeats.map { data =>
      val aes = new AESEncryption
      val en = aes.encrypt(key, data)
      en
    }.flatten

    Seq(CREECHighLevelTransaction(output, in.addr))
  }
}

class CREECDecryptHighModel(p: BusParams) extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction]
  with HWKey {
  override def process(in: CREECHighLevelTransaction) : Seq[CREECHighLevelTransaction] ={
    assert(in.data.length % p.bytesPerBeat == 0, "CREEC high transaction must have data with length = multiple of bus width")
    val dataBeats = in.data.grouped(p.bytesPerBeat).toSeq
    assert((dataBeats.length - 1) <= p.maxBeats, "CREEC high transaction has more beats than bus can support")

    val output = dataBeats.map { data =>
      val aes = new AESEncryption
      val de = aes.decrypt(key, data)
      de
    }.flatten

    Seq(CREECHighLevelTransaction(output, in.addr))
  }
}