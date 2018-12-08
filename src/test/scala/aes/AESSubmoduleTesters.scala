package aes

import chisel3._
import chisel3.iotesters.PeekPokeTester
import scala.math.pow

class TopWrapper extends Bundle {
  val data_in_top = Input(UInt(128.W))
  val data_out_top = Output(UInt(128.W))
}

class SubByteWrapper extends Module {
  val io = IO(new TopWrapper)
  val dut = Module(new SubByte)

  dut.io.data_in := io.data_in_top.asTypeOf(Vec(16, UInt(8.W)))
  io.data_out_top := dut.io.data_out.asTypeOf(UInt(128.W))
}

/**
 * PeekPokeTester for SubByte
 */
class SubByteTester(dut: SubByteWrapper) extends PeekPokeTester(dut) {

    val d0 : BigInt = (BigInt(0x0001020304050607L) << 64) + BigInt(0x08090a0b0c0d0e0fL)
    poke(dut.io.data_in_top, d0)
    var bigIntOut : BigInt = peek(dut.io.data_out_top)
    logger info s" Input  as hex: 0001020304050607 08090a0b0c0d0e0f"
    var hex0 : Long = (bigIntOut << 64 >> 64).toLong
    var hex1 : Long = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: 637c777bf26b6fc5 3001672bfed7ab76"

    val d1 : BigInt = (BigInt(0x1011121314151617L) << 64) + BigInt(0x18191a1b1c1d1e1fL)
    poke(dut.io.data_in_top, d1)
    bigIntOut = peek(dut.io.data_out_top)
    logger info s" Input  as hex: 1011121314151617 18191a1b1c1d1e1f"
    hex0 = (bigIntOut << 64 >> 64).toLong
    hex1 = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: ca82c97dfa5947f0 add4a2af9ca472c0 "

    //weird behavior with 0xff b/c of signed values
    val d15 : BigInt = (BigInt(0x70f1f2f3f4f5f6f7L) << 64) + BigInt(0x78f9fafbfcfdfeffL)
    poke(dut.io.data_in_top, d15)
    bigIntOut = peek(dut.io.data_out_top)
    logger info s" Input  as hex: ${(d15 >> 64).toLong.toHexString} ${(d15 << 64 >> 64).toLong.toHexString}"
    hex0 = (bigIntOut << 64 >> 64).toLong
    hex1 = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: 51a1890dbfe64268 bc992d0fb054bb16 "
}

object SubByteTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new SubByteWrapper()) {
      c => new SubByteTester(c)
    }
  }
}

//-----------------------

class SmallWrapper extends Bundle {
  val data_in_top = Input(UInt(32.W))
  val data_out_top = Output(UInt(32.W))
}

class MMWrapper extends Module {
  val io = IO(new SmallWrapper)
  val dut = Module(new MixColumnsMM)

  dut.io.data_in := io.data_in_top.asTypeOf(Vec(4, UInt(8.W)))
  io.data_out_top := dut.io.data_out.asTypeOf(UInt(32.W))
}

/**
  * PeekPokeTester for SubByte
  */
class SubMMTester(dut: MMWrapper) extends PeekPokeTester(dut) {

  logger info s"MM sub test"

  var data : Int = 0x76c11135
  poke(dut.io.data_in_top, data)
  var out : Int = peek(dut.io.data_out_top).toInt
  logger info s" Input  as hex: ${data.toHexString}"
  logger info s" Output as hex: ${out.toHexString}"
  logger info s" Expect as hex: 632739ee"

  data = 0x3511c176
  poke(dut.io.data_in_top, data)
  out = peek(dut.io.data_out_top).toInt
  logger info s" Input  as hex: ${data.toHexString}"
  logger info s" Output as hex: ${out.toHexString}"
  logger info s" Expect as hex: 20cae990"

  data = 0x63636363
  poke(dut.io.data_in_top, data)
  out = peek(dut.io.data_out_top).toInt
  logger info s" Input  as hex: ${data.toHexString}"
  logger info s" Output as hex: ${out.toHexString}"
  logger info s" Expect as hex: 63636363"

  data = 0x1b1b1b2b
  poke(dut.io.data_in_top, data)
  out = peek(dut.io.data_out_top).toInt
  logger info s" Input  as hex: ${data.toHexString}"
  logger info s" Output as hex: ${out.toHexString}"
  logger info s" Expect as hex: 4b2b2b7b"
}

object SubMMTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new MMWrapper()) {
      c => new SubMMTester(c)
    }
  }
}

// ------------------

class RCONIOBundle extends Bundle {
  val rcon_table = Output(UInt((8*13).W))
}

trait connectRCONstage {
  def connect(a : RCON, b : RCON): Unit = {
    b.io.last_rcon := a.io.next_rcon
  }
}

class RCONWrapper extends Module with connectRCONstage{
  val io = IO(new RCONIOBundle)
  val dut1 = Module(new RCON)
  val dut2 = Module(new RCON)
  val dut3 = Module(new RCON)
  val dut4 = Module(new RCON)
  val dut5 = Module(new RCON)
  val dut6 = Module(new RCON)
  val dut7 = Module(new RCON)
  val dut8 = Module(new RCON)
  val dut9 = Module(new RCON)
  val dut10 = Module(new RCON)
  val dut11 = Module(new RCON)
  val dut12 = Module(new RCON)
  val dut13 = Module(new RCON)

  dut1.io.last_rcon := 0x01.U(8.W)
  connect(dut1, dut2)
  connect(dut2, dut3)
  connect(dut3, dut4)
  connect(dut4, dut5)
  connect(dut5, dut6)
  connect(dut6, dut7)
  connect(dut7, dut8)
  connect(dut8, dut9)
  connect(dut9, dut10)
  connect(dut10, dut11)
  connect(dut11, dut12)
  connect(dut12, dut13)

  val rcon_table = Wire(Vec(13, UInt(8.W)))
  rcon_table(0) := dut1.io.next_rcon
  rcon_table(1) := dut2.io.next_rcon
  rcon_table(2) := dut3.io.next_rcon
  rcon_table(3) := dut4.io.next_rcon
  rcon_table(4) := dut5.io.next_rcon
  rcon_table(5) := dut6.io.next_rcon
  rcon_table(6) := dut7.io.next_rcon
  rcon_table(7) := dut8.io.next_rcon
  rcon_table(8) := dut9.io.next_rcon
  rcon_table(9) := dut10.io.next_rcon
  rcon_table(10) := dut11.io.next_rcon
  rcon_table(11) := dut12.io.next_rcon
  rcon_table(12) := dut13.io.next_rcon

  io.rcon_table := rcon_table.asTypeOf(UInt((13*8).W))
}

/**
  * PeekPokeTester for SubByte
  */
class SubRCONTester(dut: RCONWrapper) extends PeekPokeTester(dut) {

  logger info s"RCON sub test"

//  var data : Int = 0x76c11135
//  poke(dut.io.data_in_top, data)
  step(1)
  var out : BigInt = peek(dut.io.rcon_table)
  logger info s"rcon First 4 as hex: ${(out % pow(2, 32).toLong).toLong.toHexString}"
  logger info s"rcon Expect  as hex: 10080402"
  logger info s"rcon Next 4  as hex: ${((out >> 32) % pow(2, 32).toLong).toLong.toHexString}"
  logger info s"rcon Expect  as hex: 1b804020"
  logger info s"rcon Next 4  as hex: ${((out >> (32*2))% pow(2, 32).toLong).toLong.toHexString}"
  logger info s"rcon Expect  as hex: abd86c36"
}

object SubRCONTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new RCONWrapper()) {
      c => new SubRCONTester(c)
    }
  }
}

// --------------------------
class KeyExpansionTopWrapper extends Bundle {
  val key_in_top = Input(UInt(128.W))
  val key_out_top = Output(UInt(128.W))
  val rcon = Input(UInt(8.W))
}

class SubKeyExpansionWrapper extends Module {
  val io = IO(new KeyExpansionTopWrapper)
  val dut = Module(new KeyExpansion)

  dut.io.key_in := io.key_in_top.asTypeOf(Vec(16, UInt(8.W)))
  dut.io.rcon := io.rcon
  io.key_out_top := dut.io.key_out.asTypeOf(UInt(128.W))
}

/**
  * PeekPokeTester for SubKeyExpansion
  */
class SubKeyExpansionTester(dut: SubKeyExpansionWrapper) extends PeekPokeTester(dut) {

  var key : BigInt = (BigInt(0x3c4fcf098815f7abL) << 64) + BigInt(0x76d2ae2816157e2bL)
  var rcon : Int = 0x01
  poke(dut.io.key_in_top, key)
  poke(dut.io.rcon, rcon)
  var bigIntOut : BigInt = peek(dut.io.key_out_top)
  logger info s" Key Input  as hex: ${(key >> 64).toLong.toHexString} ${(key << 64 >> 64).toLong.toHexString}"
  var hex0 : Long = (bigIntOut << 64 >> 64).toLong
  var hex1 : Long = (bigIntOut >> 64).toLong
  logger info s" Key Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s" Key Expect as hex: d5766c2ae939a323 612c548817fefaa0"

  key = (BigInt(0x1011121314151617L) << 64) + BigInt(0x18191a1b1c1d1e1fL)
  rcon = 0x02
  poke(dut.io.key_in_top, key)
  poke(dut.io.rcon, rcon)
  bigIntOut = peek(dut.io.key_out_top)
  logger info s" Key Input  as hex: ${(key >> 64).toLong.toHexString} ${(key << 64 >> 64).toLong.toHexString}"
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s" Key Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s" Key Expect as hex: 7dca82cb6ddb90d8 79ce86cf61d79cd4"

  key = ((BigInt(0xd9d09fb4L) << 96) + (BigInt(0xa6a5e6ceL) << 64) + (BigInt(0xdbd39cb5L) << 32) +  BigInt(0xa7a4e7cfL))
  rcon= 0x04
  poke(dut.io.key_in_top, key)
  poke(dut.io.rcon, rcon)
  bigIntOut = peek(dut.io.key_out_top)
  logger info s" Key Input  as hex: ${(key >> 64).toLong.toHexString} ${(key << 64 >> 64).toLong.toHexString}"
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s" Key Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s" Key Expect as hex: 8e3772df57e7ed6b f1420ba52a919710"

  key = ((BigInt(0x8E3772dfL) << 96) + (BigInt(0x57e7ed6B) << 64) + (BigInt(0xf1420ba5L) << 32) +  BigInt(0x2a919710))
  rcon= 0x08
  poke(dut.io.key_in_top, key)
  poke(dut.io.rcon, rcon)
  bigIntOut = peek(dut.io.key_out_top)
  logger info s" Key Input  as hex: ${(key >> 64).toLong.toHexString} ${(key << 64 >> 64).toLong.toHexString}"
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s" Key Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s" Key Expect as hex: 9c1a9949122deb96 45ca06fdb4880d58"
}

object SubKeyExpansionTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new SubKeyExpansionWrapper()) {
      c => new SubKeyExpansionTester(c)
    }
  }
}


// --------------------------
class CipherTopWrapper extends Bundle {
  val key_in_top = Input(UInt(128.W))
  val data_in_top = Input(UInt(128.W))
  val data_out_top = Output(UInt(128.W))

  val subbytetop = Output(UInt(128.W))
  val shiftrowstop = Output(UInt(128.W))
  val mixcolumnstop = Output(UInt(128.W))
  val addroundkey = Output(UInt(128.W))
}

class SubCipherWrapper extends Module {
  val io = IO(new CipherTopWrapper)
  val dut = Module(new AESCipherStage)

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
class SubCipherTester(dut: SubCipherWrapper) extends PeekPokeTester(dut) {

  logger info s" SubStage test"
  var key : BigInt = (BigInt(0x8e3772dfL) << 96) + (BigInt(0x57e7ed6bL) << 64) + (BigInt(0xf1420ba5L) << 32) +  BigInt(0x2a919710L)
  var data : BigInt = (BigInt(0x74a99fe8L) << 96) + (BigInt(0x2a433febL) << 64) + (BigInt(0xcbcb4054L) << 32) +  BigInt(0x9ca97ca7L)

  poke(dut.io.key_in_top, key)
  poke(dut.io.data_in_top, data)
  logger info s"cipher data      Input  as hex: ${(data >> 64).toLong.toHexString} ${(data << 64 >> 64).toLong.toHexString}"

  var bigIntOut : BigInt = peek(dut.io.subbytetop)
  var hex0 : Long = (bigIntOut << 64 >> 64).toLong
  var hex1 : Long = (bigIntOut >> 64).toLong
  logger info s"cipher Subbyte   Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s"cipher Subbyte   Expect as hex: 92d3db9be51a75e9 1f1f0920ded3105c"

  bigIntOut = peek(dut.io.shiftrowstop)
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s"cipher shiftrows Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s"cipher shiftrows Expect as hex: e51f109b1fd3dbe9 ded37520921a095c"

  bigIntOut = peek(dut.io.mixcolumnstop)
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s"cipher mixcol    Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s"cipher mixcol    Expect as hex: 68817fe716ae3575 61917ad2c8ccf22b"

  bigIntOut = peek(dut.io.data_out_top)
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s"cipher data Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s"cipher data Expect as hex: e6b60d384149d818 90d37177e25d653b"

}

object SubCipherTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new SubCipherWrapper()) {
      c => new SubCipherTester(c)
    }
  }
}

// --------------------------
class CompleteStageTopWrapper extends Bundle {
  val key_in_top = Input(UInt(128.W))
  val key_out_top = Output(UInt(128.W))
  val data_in_top = Input(UInt(128.W))
  val data_out_top = Output(UInt(128.W))
  val last_rcon = Input(UInt(8.W))
  val next_rcon = Output(UInt(8.W))
}

class SubStageWrapper extends Module {
  val io = IO(new CompleteStageTopWrapper)
  val dut = Module(new AES128CompleteStage)

  dut.io.key_in := io.key_in_top.asTypeOf(Vec(16, UInt(8.W)))
  dut.io.data_in := io.data_in_top.asTypeOf(Vec(16, UInt(8.W)))
  dut.io.last_rcon := io.last_rcon
  io.key_out_top := dut.io.key_out.asTypeOf(UInt(128.W))
  io.data_out_top := dut.io.data_out.asTypeOf(UInt(128.W))
  io.next_rcon := dut.io.next_rcon
}

/**
  * PeekPokeTester for SubByte
  */
class SubStageTester(dut: SubStageWrapper) extends PeekPokeTester(dut) {

  logger info s" SubStage test"
  var last_rcon : Int = 0x02
  var key : BigInt = (BigInt(0xd9d09fb4L) << 96) + (BigInt(0xa6a5e6ceL) << 64) + (BigInt(0xdbd39cb5L) << 32) +  BigInt(0xa7a4e7cfL)
  var data : BigInt = (BigInt(0x74a99fe8L) << 96) + (BigInt(0x2a433febL) << 64) + (BigInt(0xcbcb4054L) << 32) +  BigInt(0x9ca97ca7L)

  poke(dut.io.key_in_top, key)
  poke(dut.io.data_in_top, data)
  poke(dut.io.last_rcon, last_rcon)

  logger info s" Rcon  input: ${last_rcon}"
  logger info s" Rcon output: ${peek(dut.io.next_rcon)}"
  logger info s" Rcon expect: ${last_rcon << 1}" // not entirely correct

  var bigIntOut : BigInt = peek(dut.io.key_out_top)
  logger info s" Key Input  as hex: ${(key >> 64).toLong.toHexString} ${(key << 64 >> 64).toLong.toHexString}"
  var hex0 : Long = (bigIntOut << 64 >> 64).toLong
  var hex1 : Long = (bigIntOut >> 64).toLong
  logger info s" Key Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s" Key Expect as hex: 8e3772df57e7ed6b f1420ba52a919710"

  bigIntOut = peek(dut.io.data_out_top)
  logger info s" data Input  as hex: ${(data >> 64).toLong.toHexString} ${(data << 64 >> 64).toLong.toHexString}"
  hex0 = (bigIntOut << 64 >> 64).toLong
  hex1 = (bigIntOut >> 64).toLong
  logger info s" data Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
  logger info s" data Expect as hex: e6b60d384149d818 90d37177e25d653b"

}

object SubStageTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new SubStageWrapper()) {
      c => new SubStageTester(c)
    }
  }
}


//-----------------------------
class KeyScheduleTestBundle extends Bundle {
  val key_in_top = Input(UInt(128.W))
  val key_in_valid = Input(Bool())
  val key_time_ready = Output(Bool())
  val key_time_valid = Output(Bool())
  val equality = Output(Vec(10, Bool()))
  val key_comb_schedule = Output(Vec(10, UInt(128.W)))
  val key_time_schedule = Output(Vec(10, UInt(128.W)))
}

class KeyScheduleTestWrapper extends Module {
  val io = IO(new KeyScheduleTestBundle)

  val key_comb = Module (new KeySchedule)
  val key_time = Module (new KeyScheduleTimeInterleave)

  key_comb.io.key_in      := io.key_in_top.asTypeOf(Vec(16, UInt(8.W)))
  key_time.io.key_in.bits := io.key_in_top.asTypeOf(Vec(16, UInt(8.W)))
  key_time.io.key_in.valid := io.key_in_valid

  io.key_time_ready := key_time.io.key_in.ready
  io.key_time_valid := key_time.io.key_valid

  for (i <- 0 until 10) {
    io.equality(i) := key_comb.io.key_schedule(i).asTypeOf(UInt(128.W)) === key_time.io.key_schedule(i).asTypeOf(UInt(128.W))
    io.key_comb_schedule(i) := key_comb.io.key_schedule(i).asTypeOf(UInt(128.W))
    io.key_time_schedule(i) := key_time.io.key_schedule(i).asTypeOf(UInt(128.W))
  }
}

class SubKeyScheduleTimeInterleaveTester(dut: KeyScheduleTestWrapper) extends PeekPokeTester(dut) {
  logger info s" Key Schedule Time Interleave test"
  var key : BigInt = (BigInt(0xd9d09fb4L) << 96) + (BigInt(0xa6a5e6ceL) << 64) + (BigInt(0xdbd39cb5L) << 32) +  BigInt(0xa7a4e7cfL)

  val maxCyclesWait = 10
  var cyclesWaiting = 0
  logger info s"Waiting for key schedule to be ready"
  while ((peek(dut.io.key_time_ready) == 0) && cyclesWaiting < maxCyclesWait) {
    cyclesWaiting += 1
    logger info s"waited: $cyclesWaiting cycles"
    step(1)
  }

  if (cyclesWaiting > maxCyclesWait) {
    expect(false, "Waited too long")
  }

  expect(dut.io.key_time_ready, 1, "key_time should be ready")
  expect(dut.io.key_time_valid, 0, "key_time should not be valid")

  poke(dut.io.key_in_top, key)
  poke(dut.io.key_in_valid, 1)

  step(1)
  expect(dut.io.key_time_ready, 0, "key_time should have accepted new key")
  expect(dut.io.key_time_valid, 0, "key_time should have accepted new key")

  cyclesWaiting = 0
  while ((peek(dut.io.key_time_valid) == 0) && cyclesWaiting < maxCyclesWait) {
    cyclesWaiting += 1
    logger info s"waited: $cyclesWaiting cycles"
    step(1)
  }

  if (cyclesWaiting > maxCyclesWait) {
    expect(false, "Waited too long")
  }

  for (i <- 0 until 10) {
    expect(dut.io.equality(i), 1, s"Output $i does not match")
  }
}

object SubKeyScheduleTimeInterleaveTester {
  def apply(): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new KeyScheduleTestWrapper()) {
      c => new SubKeyScheduleTimeInterleaveTester(c)
    }
  }
}
