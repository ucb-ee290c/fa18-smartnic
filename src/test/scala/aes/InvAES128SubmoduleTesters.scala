package aes

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.util._
import scala.math.pow


class InvSubByteWrapper extends Module {
  val io = IO(new TopWrapper)
  val dut = Module(new InvSubByte)

  dut.io.data_in := io.data_in_top.asTypeOf(Vec(16, UInt(8.W)))
  io.data_out_top := dut.io.data_out.asTypeOf(UInt(128.W))
}

/**
 * PeekPokeTester for SubByte
 */
class InvSubByteTester(dut: InvSubByteWrapper) extends PeekPokeTester(dut) {

    val d0 : BigInt = (BigInt(0x637c777bf26b6fc5L) << 64) + BigInt(0x3001672bfed7ab76L)
    poke(dut.io.data_in_top, d0)
    var bigIntOut : BigInt = peek(dut.io.data_out_top)
    logger info s" Input  as hex: 637c777bf26b6fc5 3001672bfed7ab76"
    var hex0 : Long = (bigIntOut << 64 >> 64).toLong
    var hex1 : Long = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: 0001020304050607 08090a0b0c0d0e0f"

    val d1 : BigInt = (BigInt(0xca82c97dL) << 96) + (BigInt(0xfa5947f0L) << 64) + (BigInt(0xadd4a2afL) << 32) + BigInt(0x9ca472c0L)
    poke(dut.io.data_in_top, d1)
    bigIntOut = peek(dut.io.data_out_top)
    logger info s" Input  as hex: ca82c97dfa5947f0 add4a2af9ca472c0"
    hex0 = (bigIntOut << 64 >> 64).toLong
    hex1 = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: 1011121314151617 18191a1b1c1d1e1f"

    val d15 : BigInt = (BigInt(0x51a1890dL) << 96) + (BigInt(0xbfe64268L) << 64) + (BigInt(0xbc992d0fL) << 32) + BigInt(0xb054bb16L)
    poke(dut.io.data_in_top, d15)
    bigIntOut = peek(dut.io.data_out_top)
    logger info s" Input  as hex: ${(d15 >> 64).toLong.toHexString} ${(d15 << 64 >> 64).toLong.toHexString}"
    hex0 = (bigIntOut << 64 >> 64).toLong
    hex1 = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: 70f1f2f3f4f5f6f7 78f9fafbfcfdfeff "
}

object InvSubByteTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new InvSubByteWrapper()) {
      c => new InvSubByteTester(c)
    }
  }
}

//-----------------------

class InvMMWrapper extends Module {
  val io = IO(new SmallWrapper)
  val dut = Module(new InvMixColumnsMM)

  dut.io.data_in := io.data_in_top.asTypeOf(Vec(4, UInt(8.W)))
  io.data_out_top := dut.io.data_out.asTypeOf(UInt(32.W))
}

/**
  * PeekPokeTester for SubByte
  */
class InvSubMMTester(dut: InvMMWrapper) extends PeekPokeTester(dut) {

  logger info s"Inv MM sub test"

  var data : Int = 0x632739ee
  poke(dut.io.data_in_top, data)
  var out : Int = peek(dut.io.data_out_top).toInt
  logger info s" Input  as hex: ${data.toHexString}"
  logger info s" Output as hex: ${out.toHexString}"
  logger info s" Expect as hex: 76c11135"

  data = 0x20cae990
  poke(dut.io.data_in_top, data)
  out = peek(dut.io.data_out_top).toInt
  logger info s" Input  as hex: ${data.toHexString}"
  logger info s" Output as hex: ${out.toHexString}"
  logger info s" Expect as hex: 3511c176"

  data = 0x63636363
  poke(dut.io.data_in_top, data)
  out = peek(dut.io.data_out_top).toInt
  logger info s" Input  as hex: ${data.toHexString}"
  logger info s" Output as hex: ${out.toHexString}"
  logger info s" Expect as hex: 63636363"

  data = 0x4b2b2b7b
  poke(dut.io.data_in_top, data)
  out = peek(dut.io.data_out_top).toInt
  logger info s" Input  as hex: ${data.toHexString}"
  logger info s" Output as hex: ${out.toHexString}"
  logger info s" Expect as hex: 1b1b1b2b"
}

object InvSubMMTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new InvMMWrapper()) {
      c => new InvSubMMTester(c)
    }
  }
}

// --------------------------
class InvCipherTopWrapper extends Bundle {
  val key_in_top = Input(UInt(128.W))
  val data_in_top = Input(UInt(128.W))
  val data_out_top = Output(UInt(128.W))

  val subbytetop = Output(UInt(128.W))
  val shiftrowstop = Output(UInt(128.W))
  val mixcolumnstop = Output(UInt(128.W))
  val addroundkey = Output(UInt(128.W))
}

class InvSubCipherWrapper extends Module {
  val io = IO(new InvCipherTopWrapper)
  val dut = Module(new InvAESCipherStage)

  dut.io.key_in := io.key_in_top.asTypeOf(Vec(16, UInt(8.W)))
  dut.io.data_in := io.data_in_top.asTypeOf(Vec(16, UInt(8.W)))
  io.data_out_top := dut.io.data_out.asTypeOf(UInt(128.W))

  io.subbytetop :=  dut.io.subbyteout
  io.shiftrowstop :=  dut.io.shiftrowsout
  io.mixcolumnstop :=  dut.io.mixcolsout
  io.addroundkey := dut.io.addroundkey
}

/**
  * PeekPokeTester for SubByte
  */
class InvSubCipherTester(dut: InvSubCipherWrapper) extends PeekPokeTester(dut) {

  logger info s" SubStage test"
  var key : BigInt = (BigInt(0xdf72378eL)) + (BigInt(0x6bede757L) << 32) + (BigInt(0xa50b42f1L) << 64) +  (BigInt(0x1097912aL) << 96)
  var data : BigInt = (BigInt(0xe89fa974L)) + (BigInt(0xeb3f432aL) << 32) + (BigInt(0x5440cbcbL) << 64) +  (BigInt(0xa77ca99cL) << 96)

  poke(dut.io.key_in_top, key)
  poke(dut.io.data_in_top, data)
  logger info s"cipher data      Input  as hex: ${(data >> 64).toLong.toHexString} ${(data << 64 >> 64).toLong.toHexString}"

  var bigIntOut : BigInt = peek(dut.io.addroundkey)
  var hex0 : Long = (bigIntOut << 64 >> 64).toLong
  var hex1 : Long = (bigIntOut >> 64).toLong
  logger info s"cipher addroundkey   Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s"cipher addroundkey   Expect as hex: b7eb38b6f14b893a 80d2a47d37ed9efa "

  bigIntOut = peek(dut.io.mixcolumnstop)
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s"cipher mixcol    Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s"cipher mixcol    Expect as hex: a72497c6c6baacd9 4e4367e11d27d357"


  bigIntOut = peek(dut.io.shiftrowstop)
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s"cipher shiftrows Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s"cipher shiftrows Expect as hex: 1d43acc6a72767d9 c624d3e14eba9757"

  bigIntOut = peek(dut.io.data_out_top)
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s"cipher data Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s"cipher data Expect as hex: de64aac7893d0ae5 c7a6a9e0b6c085da"

}

object InvSubCipherTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new InvSubCipherWrapper()) {
      c => new InvSubCipherTester(c)
    }
  }
}
