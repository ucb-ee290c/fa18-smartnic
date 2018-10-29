package aes

import chisel3._
import chisel3.util._
//import freechips.rocketchip.subsystem.BaseSubsystem
//import freechips.rocketchip.config.{Parameters, Field}
//import freechips.rocketchip.diplomacy._
//import freechips.rocketchip.regmapper.{HasRegMap, RegField}
//import freechips.rocketchip.tilelink._

trait hasSubByte {
    def subByte(byte_in: UInt): UInt = {
        (byte_in
          ^ (byte_in << 1).asTypeOf(UInt(8.W))
          ^ (byte_in << 2).asTypeOf(UInt(8.W))
          ^ (byte_in << 3).asTypeOf(UInt(8.W))
          ^ (byte_in << 4).asTypeOf(UInt(8.W))
          ^ 99.U(8.W)
          ).asTypeOf(UInt(8.W))
    }
}

class DataBundle extends Bundle {
    val data_in = Input(Vec(16, UInt(8.W)))
    val data_out = Output(Vec(16, UInt(8.W)))
}

class DataBundleWithKeyIn extends DataBundle {
    val key_in = Input(Vec(16, UInt(8.W)))
}

//Byte substitution using the S-box method
//out = b + b << 1 + b << 2 + b << 3 + b << 4 + (99l mod 16)
class SubByte extends Module with hasSubByte {
    val io = IO(new DataBundle())

    for (i <- 0 until 16) {
        io.data_out(i) := subByte(io.data_in(i))
    }
}

//shuffle the 16-bit array
class ShiftRows extends Module {
    val io = IO(new DataBundle())

    io.data_out(0) := io.data_in(0)
    io.data_out(1) := io.data_in(5)
    io.data_out(2) := io.data_in(10)
    io.data_out(3) := io.data_in(15)

    io.data_out(4) := io.data_in(4)
    io.data_out(5) := io.data_in(9)
    io.data_out(6) := io.data_in(14)
    io.data_out(7) := io.data_in(3)

    io.data_out(8) := io.data_in(8)
    io.data_out(9) := io.data_in(13)
    io.data_out(10) := io.data_in(2)
    io.data_out(11) := io.data_in(7)

    io.data_out(12) := io.data_in(12)
    io.data_out(13) := io.data_in(1)
    io.data_out(14) := io.data_in(6)
    io.data_out(15) := io.data_in(11)
}


//Module takes in data and key and applies addition
class AddRoundKey extends Module {
    val io = IO(new DataBundleWithKeyIn())

    for (i <- 0 until 16) {
        io.data_out(i) := io.data_in(i) ^ io.key_in(i) //RoundKey
    }
}

class MMDataBundle extends Bundle {
    val data_in = Input(Vec(4, UInt(8.W)))
    val data_out = Output(Vec(4, UInt(8.W)))
}

class MixColumnsMM extends Module {
    val io = IO(new MMDataBundle())

    io.data_out(0) := 2.U * io.data_in(0) + 3.U * io.data_in(1) + 1.U * io.data_in(2) + 1.U * io.data_in(3)
    io.data_out(1) := 1.U * io.data_in(0) + 2.U * io.data_in(1) + 3.U * io.data_in(2) + 1.U * io.data_in(3)
    io.data_out(2) := 1.U * io.data_in(0) + 1.U * io.data_in(1) + 2.U * io.data_in(2) + 3.U * io.data_in(3)
    io.data_out(3) := 3.U * io.data_in(0) + 1.U * io.data_in(1) + 1.U * io.data_in(2) + 2.U * io.data_in(3)
}

//Column Mixing provides the primary obfuscation in AES
class MixColumns extends Module {
    val io = IO(new DataBundle())

    //Cast to a 4x4 matrix
    val d_in = io.data_in.asTypeOf(Vec(4, Vec(4, UInt(8.W))))
    val d_out = Wire(Vec(4, Vec(4, UInt(8.W))))
    io.data_out := d_out.asTypeOf(Vec(16, UInt(8.W)))

    val MM0 = Module(new MixColumnsMM())
    val MM1 = Module(new MixColumnsMM())
    val MM2 = Module(new MixColumnsMM())
    val MM3 = Module(new MixColumnsMM())

    MM0.io.data_in := d_in(0)
    d_out(0) := MM0.io.data_out

    MM1.io.data_in := d_in(1)
    d_out(1) := MM1.io.data_out

    MM2.io.data_in := d_in(2)
    d_out(2) := MM1.io.data_out

    MM3.io.data_in := d_in(3)
    d_out(3) := MM3.io.data_out
}

class KeyExpansionBundle extends Bundle {
    val key_in  = Input(Vec(16, UInt(8.W))) //Key for this stage
    val key_out = Output(Vec(16, UInt(8.W))) // calculate the next stage
    val rcon    = Input(UInt(8.W)) //generated in upper stage
}

//KeyExpansion stage. Applies the AES key schedule
//TODO: Consider pipelining? Long critical path
//TODO: Verify the addition
class KeyExpansion extends Module with hasSubByte {
    val io = IO(new KeyExpansionBundle())

    //aliases
    val k_in = io.key_in
    val k_out = Wire(Vec(16, UInt(8.W)))
    io.key_out := k_out

    //Wi
    k_out(0) := k_in(0) ^ subByte(k_in(1)) ^ io.rcon
    k_out(1) := k_in(1) ^ subByte(k_in(2))
    k_out(2) := k_in(2) ^ subByte(k_in(3))
    k_out(3) := k_in(3) ^ subByte(k_in(0))

    //Wi+1...3
    for (i <- 4 until 16) {
        k_out(i) := k_in(i) ^ k_out(i-4)
        k_out(i) := k_in(i) ^ k_out(i-4)
        k_out(i) := k_in(i) ^ k_out(i-4)
    }
}

class AESCipherStage extends Module {
    val io = IO(new DataBundleWithKeyIn())

    val sub_byte = Module(new SubByte())
    val shift_rows = Module(new ShiftRows())
    val mix_columns = Module(new MixColumns())
    val add_round_key = Module(new AddRoundKey())

    add_round_key.io.key_in := io.key_in

    //Chain modules together
    sub_byte.io.data_in :=  io.data_in
    shift_rows.io.data_in := sub_byte.io.data_out
    mix_columns.io.data_in :=  shift_rows.io.data_out
    add_round_key.io.data_in := mix_columns.io.data_out
    io.data_out := add_round_key.io.data_out
}

class AESCipherEndStage extends Module {
    val io = IO(new DataBundleWithKeyIn())

    val sub_byte = Module(new SubByte())
    val shift_rows = Module(new ShiftRows())
    val add_round_key = Module(new AddRoundKey())

    add_round_key.io.key_in := io.key_in

    //Chain modules together
    sub_byte.io.data_in :=  io.data_in
    shift_rows.io.data_in := sub_byte.io.data_out
    add_round_key.io.data_in := shift_rows.io.data_out
    io.data_out := add_round_key.io.data_out
}


class RCONBundle extends Bundle{
    val last_rcon = Input(UInt(8.W))
    val next_rcon = Output(UInt(8.W))
}

class RCON extends Module {
    val io = IO(new RCONBundle)

    when (io.last_rcon >= 128.U(8.W)) {
        io.next_rcon := (io.last_rcon << 2) ^ 27.U(8.W) // TODO: make intellij resolve the XOR
    } .otherwise {
        io.next_rcon := io.last_rcon << 2
    }

}

class AES128CompleteStageIO extends Bundle{
    val data_in = Input(Vec(16, UInt(8.W)))
    val data_out = Output(Vec(16, UInt(8.W)))

    val key_in = Input(Vec(16, UInt(8.W)))
    val key_out = Output(Vec(16, UInt(8.W)))

    val last_rcon = Input(UInt(8.W))
    val next_rcon = Output(UInt(8.W))
}

class AES128CompleteStage extends Module {
    val io = IO(new AES128CompleteStageIO)

    val rcon_gen = Module(new RCON())
    rcon_gen.io.last_rcon := io.last_rcon
    io.next_rcon := rcon_gen.io.next_rcon

    val roundkey_gen = Module(new KeyExpansion)
    roundkey_gen.io.key_in := io.key_in
    roundkey_gen.io.rcon :=  rcon_gen.io.next_rcon
    io.key_out := roundkey_gen.io.key_out

    val cipher = Module(new AESCipherStage)
    cipher.io.data_in := io.data_in
    cipher.io.key_in := roundkey_gen.io.key_out
    io.data_out := cipher.io.data_out
}

//abuts the previous stage to the next one
trait connectsStages {
    def connectStages(prev : AES128CompleteStage, next: AES128CompleteStage): Unit = {
        next.io.data_in     := prev.io.data_out
        next.io.key_in      := prev.io.key_out
        next.io.last_rcon   := prev.io.next_rcon
    }
}
class DataBundleTop extends Bundle {
    val data_in  = Input(UInt(128.W))
    val data_out = Output(UInt(128.W))
    val key_in   = Input(UInt(128.W))
}

//Purely combinational form
//Generally avoid this block because its long critical path
class AES128Combinational extends Module with connectsStages{
    val io = IO (new DataBundleTop)

    val data_in_top = io.data_in.asTypeOf(Vec(16, UInt(8.W)))
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))

    //Initial round
    val stage0_addRoundKey = Module(new AddRoundKey())
    stage0_addRoundKey.io.key_in := data_in_top
    stage0_addRoundKey.io.data_in := key_in_top
    val stage0_data_out = stage0_addRoundKey.io.data_out

    // Round 1
    val stage1_roundkey = Module(new KeyExpansion)
    stage1_roundkey.io.key_in := key_in_top
    stage1_roundkey.io.rcon := 1.U(8.W)
    val stage1_key = stage1_roundkey.io.key_out

    val stage1_cipher = Module(new AESCipherStage)
    stage1_cipher.io.data_in := stage0_data_out
    stage1_cipher.io.key_in := stage1_key
    val stage1_data_out = stage1_cipher.io.data_out

    // Round 2
    val stage2 = Module( new AES128CompleteStage)
    stage2.io.data_in   := stage1_data_out
    stage2.io.key_in    := stage1_key
    stage2.io.last_rcon := 1.U(8.W)

    // Round3
    val stage3 = Module( new AES128CompleteStage)
    connectStages(stage2, stage3)

    // Round4
    val stage4 = Module( new AES128CompleteStage)
    connectStages(stage3, stage4)

    // Round5
    val stage5 = Module( new AES128CompleteStage)
    connectStages(stage4, stage5)

    // Round6
    val stage6 = Module( new AES128CompleteStage)
    connectStages(stage5, stage6)

    // Round7
    val stage7 = Module( new AES128CompleteStage)
    connectStages(stage6, stage7)

    // Round8
    val stage8 = Module( new AES128CompleteStage)
    connectStages(stage7, stage8)

    // Round9
    val stage9 = Module( new AES128CompleteStage)
    connectStages(stage8, stage9)

    // output round
    val stage10_rcon_gen = Module(new RCON())
    stage10_rcon_gen.io.last_rcon := stage9.io.next_rcon
    val stage10_rcon = stage10_rcon_gen.io.next_rcon

    val stage10_roundkey = Module(new KeyExpansion)
    stage10_roundkey.io.key_in := stage9.io.key_out
    stage10_roundkey.io.rcon := stage10_rcon
    val stage10_key = stage10_roundkey.io.key_out

    val stage10_cipher = Module( new AESCipherEndStage )
    stage10_cipher.io.data_in := stage9.io.data_out
    stage10_cipher.io.key_in := stage10_key

    io.data_out := stage10_cipher.io.data_out.asTypeOf(UInt(128.W))
}

class DataBundleTopDecoupled extends Bundle {
    val data_in     = Flipped(Decoupled(UInt(128.W)))
    val data_out    = Decoupled(UInt(128.W))
    val key_in      = Input(UInt(128.W))
}

/*
 * Pipelined AES module
 * 10 stages plus initial stage
 * Initial and first stage are combined, making for 10 periods to complete
 */
class AES128 extends Module {
    val io = IO (new DataBundleTopDecoupled)

    val data_in_top = io.data_in.bits.asTypeOf(Vec(16, UInt(8.W)))
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))

    //State Machine ---------------------------------------
    val start = io.data_in.fire && io.data_out.fire
    val counter = Reg(UInt(4.W))
    val running = counter < 10.U // Halt at 10.U
    counter := Mux(start, 0.U,
        Mux(running, counter + 1.U, counter))

    val mux_select_stage1 = counter === 0.U

    // Ready Valid
    io.data_in.ready  := !running
    io.data_out.valid := !running

    //Computations -----------------------------------------
    //Initial round
    val stage0_addRoundKey = Module(new AddRoundKey())
    stage0_addRoundKey.io.key_in := key_in_top
    stage0_addRoundKey.io.data_in := data_in_top
    val stage0_data_out = stage0_addRoundKey.io.data_out

    // Round 1
    val stage1_rcon = 1.U(8.W) // Initial

    val stage1_roundkey = Module(new KeyExpansion)
    stage1_roundkey.io.key_in := key_in_top
    stage1_roundkey.io.rcon := stage1_rcon
    val stage1_key = stage1_roundkey.io.key_out

    val stage1_cipher = Module(new AESCipherStage)
    stage1_cipher.io.data_in := stage0_data_out
    stage1_cipher.io.key_in := stage1_key
    val stage1_data_out = stage1_cipher.io.data_out

    //stages 2-9
    val data_reg    = Reg(Vec(16, UInt(8.W)))
    val key_reg     = Reg(Vec(16, UInt(8.W)))
    val rcon_reg    = Reg(UInt(8.W))

    val AESStage = Module( new AES128CompleteStage)
    AESStage.io.data_in     := data_reg
    AESStage.io.key_in      := key_reg
    AESStage.io.last_rcon   := rcon_reg

    val data_next   = AESStage.io.data_out
    val key_next    = AESStage.io.key_out
    val rcon_next   = AESStage.io.next_rcon

    data_reg    := Mux(mux_select_stage1, stage1_data_out, data_next)
    key_reg     := Mux(mux_select_stage1, stage1_key, key_next)
    rcon_reg    := Mux(mux_select_stage1, stage1_rcon, rcon_next)

    // output round
    val stage10_rcon_gen = Module(new RCON())
    stage10_rcon_gen.io.last_rcon := rcon_reg
    val stage10_rcon = stage10_rcon_gen.io.next_rcon

    val stage10_roundkey = Module(new KeyExpansion)
    stage10_roundkey.io.key_in := key_reg
    stage10_roundkey.io.rcon := stage10_rcon
    val stage10_key = stage10_roundkey.io.key_out

    val stage10_cipher = Module( new AESCipherEndStage )
    stage10_cipher.io.data_in := data_reg
    stage10_cipher.io.key_in := stage10_key

    val data_out_top = stage10_cipher.io.data_out.asTypeOf(UInt(128.W))
    io.data_out.bits    := RegEnable(data_out_top, running)
}