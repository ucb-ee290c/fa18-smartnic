package aes

import chisel3._
import chisel3.util._
import interconnect.{CREECBusParams, CREECReadBus, CREECWriteBus}
//import freechips.rocketchip.subsystem.BaseSubsystem
//import freechips.rocketchip.config.{Parameters, Field}
//import freechips.rocketchip.diplomacy._
//import freechips.rocketchip.regmapper.{HasRegMap, RegField}
//import freechips.rocketchip.tilelink._

trait hasSubByte {
    val sBoxTable = VecInit(
    Seq(99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 254, 215, 171, 118,
        202, 130, 201, 125, 250, 89, 71, 240, 173, 212, 162, 175, 156, 164, 114, 192,
        183, 253, 147, 38, 54, 63, 247, 204, 52, 165, 229, 241, 113, 216, 49, 21,
        4, 199, 35, 195, 24, 150, 5, 154, 7, 18, 128, 226, 235, 39, 178, 117,
        9, 131, 44, 26, 27, 110, 90, 160, 82, 59, 214, 179, 41, 227, 47, 132,
        83, 209, 0, 237, 32, 252, 177, 91, 106, 203, 190, 57, 74, 76, 88, 207,
        208, 239, 170, 251, 67, 77, 51, 133, 69, 249, 2, 127, 80, 60, 159, 168,
        81, 163, 64, 143, 146, 157, 56, 245, 188, 182, 218, 33, 16, 255, 243, 210,
        205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126, 61, 100, 93, 25, 115,
        96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 184, 20, 222, 94, 11, 219,
        224, 50, 58, 10, 73, 6, 36, 92, 194, 211, 172, 98, 145, 149, 228, 121,
        231, 200, 55, 109, 141, 213, 78, 169, 108, 86, 244, 234, 101, 122, 174, 8,
        186, 120, 37, 46, 28, 166, 180, 198, 232, 221, 116, 31, 75, 189, 139, 138,
        112, 62, 181, 102, 72, 3, 246, 14, 97, 53, 87, 185, 134, 193, 29, 158,
        225, 248, 152, 17, 105, 217, 142, 148, 155, 30, 135, 233, 206, 85, 40, 223,
        140, 161, 137, 13, 191, 230, 66, 104, 65, 153, 45, 15, 176, 84, 187, 22)
      .map(_.U(8.W))
    )

    def subByte(byte_in: UInt): UInt = {
       sBoxTable(byte_in)
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
//TODO: Resolve
//Bitwise XOR and composition are valid
//Critical path is 3 gmul2 blocks plus 3-way XOR
trait MixColumnsFunctions {
    def gmul2(a : UInt): UInt = {
        Mux((a & 0x80.U(8.W)).orR, (a << 1) ^ 0x1b.U(8.W), a << 1)
    }

    def gmul3(a : UInt): UInt = {
        Mux((a & 0x80.U(8.W)).orR, (a << 1) ^ a ^ 0x1b.U(8.W), (a << 1) ^ a)
    }

    def gmul4(a: UInt): UInt = {
        gmul2(gmul2(a))
    }
    def gmul8(a: UInt): UInt = {
        gmul2(gmul2(gmul2(a)))
    }

    def gmul9(a : UInt): UInt = {
        a ^ gmul8(a)
    }

    def gmul11(a : UInt): UInt = {
        a ^ gmul2(a) ^ gmul8(a)
    }

    def gmul13(a : UInt): UInt = {
        a ^ gmul4(a) ^ gmul8(a)
    }

    def gmul14(a : UInt): UInt = {
        gmul2(a) ^ gmul4(a) ^ gmul8(a)
    }
}

class MixColumnsMM extends Module with MixColumnsFunctions{
    val io = IO(new MMDataBundle())

    io.data_out(0) := gmul2(io.data_in(0)) ^ gmul3(io.data_in(1)) ^ io.data_in(2) ^ io.data_in(3)
    io.data_out(1) := io.data_in(0) ^ gmul2(io.data_in(1)) ^ gmul3(io.data_in(2)) ^ io.data_in(3)
    io.data_out(2) := io.data_in(0) ^ io.data_in(1) ^ gmul2(io.data_in(2)) ^ gmul3(io.data_in(3))
    io.data_out(3) := gmul3(io.data_in(0)) ^ io.data_in(1) ^ io.data_in(2) ^ gmul2(io.data_in(3))
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
    d_out(2) := MM2.io.data_out

    MM3.io.data_in := d_in(3)
    d_out(3) := MM3.io.data_out
}


class RCONBundle extends Bundle{
    val last_rcon = Input(UInt(8.W))
    val next_rcon = Output(UInt(8.W))
}

class RCON extends Module {
    val io = IO(new RCONBundle)

    when ((io.last_rcon & 0x80.U(8.W)).orR) {
        io.next_rcon := (io.last_rcon << 1) ^ 0x1B.U(8.W) // TODO: make intellij resolve the XOR
    } .otherwise {
        io.next_rcon := io.last_rcon << 1
    }

}

class KeyExpansionBundle extends Bundle {
    val key_in  = Input(Vec(16, UInt(8.W))) //Key for this stage
    val key_out = Output(Vec(16, UInt(8.W))) // calculate the next stage
    val rcon    = Input(UInt(8.W)) //generated in upper stage
}

//KeyExpansion stage. Applies the AES key schedule
//TODO: Consider pipelining? Long critical path
class KeyExpansion extends Module with hasSubByte {
    val io = IO(new KeyExpansionBundle())

    //Wi
    io.key_out(0) := io.key_in(0) ^ subByte(io.key_in(13)) ^ io.rcon
    io.key_out(1) := io.key_in(1) ^ subByte(io.key_in(14))
    io.key_out(2) := io.key_in(2) ^ subByte(io.key_in(15))
    io.key_out(3) := io.key_in(3) ^ subByte(io.key_in(12))

    //Wi+1...3
    for (i <- 4 until 16) {
        io.key_out(i) := io.key_in(i) ^ io.key_out(i-4)
    }

}

//Calculates the entire schedule
class KeyScheduleBundle extends Bundle {
    val key_in  = Input(Vec(16, UInt(8.W)))
    val key_schedule = Output(Vec(10, (Vec(16, UInt(8.W)))))
}

trait keyConnect {
    def connectRCON(prev: RCON, next: RCON) = {
        next.io.last_rcon := prev.io.next_rcon
    }

    def connectKeyStage(prev: KeyExpansion, next: KeyExpansion, rcon: RCON) = {
        next.io.key_in := prev.io.key_out
        next.io.rcon := rcon.io.next_rcon
    }
}

//TODO: investigate time-interleaved implementation
//TODO: Investigate parallel generation imeplementation
class KeySchedule extends Module with keyConnect{
    val io = IO(new KeyScheduleBundle())

    val r2 = Module( new RCON())
    val r3 = Module( new RCON())
    val r4 = Module( new RCON())
    val r5 = Module( new RCON())
    val r6 = Module( new RCON())
    val r7 = Module( new RCON())
    val r8 = Module( new RCON())
    val r9 = Module( new RCON())
    val r10 = Module( new RCON())

    r2.io.last_rcon := 1.U(8.W)
    connectRCON(r2, r3)
    connectRCON(r3, r4)
    connectRCON(r4, r5)
    connectRCON(r5, r6)
    connectRCON(r6, r7)
    connectRCON(r7, r8)
    connectRCON(r8, r9)
    connectRCON(r9, r10)

    val k1 = Module( new KeyExpansion())
    val k2 = Module( new KeyExpansion())
    val k3 = Module( new KeyExpansion())
    val k4 = Module( new KeyExpansion())
    val k5 = Module( new KeyExpansion())
    val k6 = Module( new KeyExpansion())
    val k7 = Module( new KeyExpansion())
    val k8 = Module( new KeyExpansion())
    val k9 = Module( new KeyExpansion())
    val k10 = Module( new KeyExpansion())

    k1.io.rcon := 1.U(8.W)
    k1.io.key_in := io.key_in

    connectKeyStage(k1, k2, r2)
    connectKeyStage(k2, k3, r3)
    connectKeyStage(k3, k4, r4)
    connectKeyStage(k4, k5, r5)
    connectKeyStage(k5, k6, r6)
    connectKeyStage(k6, k7, r7)
    connectKeyStage(k7, k8, r8)
    connectKeyStage(k8, k9, r9)
    connectKeyStage(k9, k10, r10)

    io.key_schedule(0) := k1.io.key_out
    io.key_schedule(1) := k2.io.key_out
    io.key_schedule(2) := k3.io.key_out
    io.key_schedule(3) := k4.io.key_out
    io.key_schedule(4) := k5.io.key_out
    io.key_schedule(5) := k6.io.key_out
    io.key_schedule(6) := k7.io.key_out
    io.key_schedule(7) := k8.io.key_out
    io.key_schedule(8) := k9.io.key_out
    io.key_schedule(9) := k10.io.key_out
}


class DataBundleWithKeyInDebug extends DataBundleWithKeyIn {
    val subbyteout = Output(UInt(128.W))
    val shiftrowsout  = Output(UInt(128.W))
    val mixcolsout = Output(UInt(128.W))
    val addroundkey = Output(UInt(128.W))
}

class AESCipherStage extends Module {
    val io = IO(new DataBundleWithKeyInDebug())

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

    //Debug
    io.subbyteout := sub_byte.io.data_out.asTypeOf(UInt(128.W))
    io.shiftrowsout := shift_rows.io.data_out.asTypeOf(UInt(128.W))
    io.mixcolsout := mix_columns.io.data_out.asTypeOf(UInt(128.W))
    io.addroundkey := add_round_key.io.data_out.asTypeOf(UInt(128.W))
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
    def connectStages(prev : AES128CompleteStage, next: AES128CompleteStage) = {
        next.io.data_in     := prev.io.data_out
        next.io.key_in      := prev.io.key_out
        next.io.last_rcon   := prev.io.next_rcon
    }

    def connectInvStages(prev: InvAESCipherStage, next: InvAESCipherStage, key: Vec[UInt] ) = {
        next.io.data_in := prev.io.data_out
        next.io.key_in  := key
    }
}
class DataBundleTop extends Bundle {
    val data_in  = Input(UInt(128.W))
    val data_out = Output(UInt(128.W))
    val key_in   = Input(UInt(128.W))
}

class DataBundleTopDebug extends Bundle {
  val data_in  = Input(UInt(128.W))
  val data_out = Output(UInt(128.W))
  val key_in   = Input(UInt(128.W))
  val stage1out = Output(UInt(128.W))
  val stage2out = Output(UInt(128.W))
  val stage2key = Output(UInt(128.W))
  val stage3out = Output(UInt(128.W))
  val stage3key = Output(UInt(128.W))
  val stage3rcon = Output(UInt(8.W))
  val stage4out = Output(UInt(128.W))
  val stage5out = Output(UInt(128.W))
  val stage6out = Output(UInt(128.W))
  val stage7out = Output(UInt(128.W))
  val stage8out = Output(UInt(128.W))
  val stage9out = Output(UInt(128.W))
}

//Purely combinational form
//Generally avoid this block because its long critical path
class AES128Combinational extends Module with connectsStages{
    val io = IO (new DataBundleTopDebug)

    val data_in_top = io.data_in.asTypeOf(Vec(16, UInt(8.W)))
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))

    //Initial round
    val stage0_addRoundKey = Module(new AddRoundKey())
    stage0_addRoundKey.io.key_in := key_in_top
    stage0_addRoundKey.io.data_in := data_in_top
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
    io.stage9out := stage9.io.data_out.asTypeOf(UInt(128.W))
    io.stage8out := stage8.io.data_out.asTypeOf(UInt(128.W))
    io.stage7out := stage7.io.data_out.asTypeOf(UInt(128.W))
    io.stage6out := stage6.io.data_out.asTypeOf(UInt(128.W))
    io.stage5out := stage5.io.data_out.asTypeOf(UInt(128.W))
    io.stage4out := stage4.io.data_out.asTypeOf(UInt(128.W))
    io.stage3out := stage3.io.data_out.asTypeOf(UInt(128.W))
    io.stage2out := stage2.io.data_out.asTypeOf(UInt(128.W))
    io.stage1out := stage1_cipher.io.data_out.asTypeOf(UInt(128.W))
    io.stage3key := stage3.io.key_out.asTypeOf(UInt(128.W))
    io.stage2key := stage2.io.key_out.asTypeOf(UInt(128.W))
    io.stage3rcon := stage3.io.next_rcon.asTypeOf(UInt(128.W))
}

class DataBundleTopDecoupled extends Bundle {
    val data_in     = Flipped(Decoupled(UInt(128.W)))
    val data_out    = Decoupled(UInt(128.W))
    val key_in      = Input(UInt(128.W))
}

class DataBundleTopDecoupledDebug extends DataBundleTopDecoupled {
    val running     = Output(Bool())
    val peek_stage  = Output(UInt(128.W))
    val counter     = Output(UInt(4.W))
}

/*
 * Pipelined AES module
 * 10 stages plus initial stage
 * Initial and first stage are combined, making for 10 periods to complete
 */
class AES128 extends Module {
    val io = IO (new DataBundleTopDecoupledDebug)

    val data_in_top = io.data_in.bits.asTypeOf(Vec(16, UInt(8.W)))
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))

    //State Machine ---------------------------------------
    val numStages = 10 //for AES128

    val start = io.data_in.fire && io.data_out.fire
    val counter = RegInit(0.U(4.W))
    val running = counter > 0.U

    counter := Mux(running, counter-1.U,
            Mux(start, numStages.U, counter))

    val mux_select_stage1 = counter === numStages.U

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

    //Debug
    io.running      := running
    io.counter      := counter
    io.peek_stage   := data_reg.asTypeOf(UInt(128.W))
}

class AES128EncrypterWrapper extends Module {
    val io = IO(new Bundle {
        val slave = new CREECWriteBus(new CREECBusParams)
        val master = Flipped(new CREECWriteBus(new CREECBusParams))
    })
    // Hookup AES128 to the slave and master port here
    io.slave.header.ready := false.B
    io.master.header.valid := false.B
}

class AES128DecrypterWrapper extends Module {
    val io = IO(new Bundle {
        val slave = new CREECReadBus(new CREECBusParams)
        val master = Flipped(new CREECReadBus(new CREECBusParams))
    })
    // Hookup the decrypter
    io.slave.header.ready := false.B
    io.master.header.valid := false.B
}