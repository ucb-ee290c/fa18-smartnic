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

class AES128TimeInterleaveCompute extends Module {
    val io = IO (new DataBundleKeyScheduleDecoupledDebug)

    val data_in_top = io.data_in.bits.asTypeOf(Vec(16, UInt(8.W)))
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))
    val key_schedule = io.key_schedule

    //State Machine ---------------------------------------
    val numStages = 10 //for AES128

    val start = io.data_in.fire && io.data_out.fire && io.key_valid
    val counter = RegInit(0.U(4.W))
    val running = counter > 0.U

    counter := Mux(running, counter-1.U,
        Mux(start, numStages.U, counter))

    val mux_select_stage1 = counter === numStages.U

    // Ready Valid
    io.data_in.ready  := !running && io.key_valid
    io.data_out.valid := !running && io.key_valid

    //Computations -----------------------------------------
    //Initial round
    val stage0_addRoundKey = Module(new AddRoundKey())
    stage0_addRoundKey.io.key_in := key_in_top
    stage0_addRoundKey.io.data_in := data_in_top
    val stage0_data_out = stage0_addRoundKey.io.data_out

    // Round 1
    val stage1_cipher = Module(new AESCipherStage)
    stage1_cipher.io.data_in := stage0_data_out
    stage1_cipher.io.key_in := key_schedule(0)
    val stage1_data_out = stage1_cipher.io.data_out

    //stages 2-9
    val data_reg    = Reg(Vec(16, UInt(8.W)))

    val AESStage = Module( new AESCipherStage)
    AESStage.io.data_in     := data_reg
    AESStage.io.key_in     := io.key_schedule(10.U - counter)

    val data_next   = AESStage.io.data_out
    data_reg    := Mux(mux_select_stage1, stage1_data_out, data_next)

    // output round
    val stage10 = Module( new AESCipherEndStage )
    stage10.io.data_in := data_reg
    stage10.io.key_in := key_schedule(9)

    val data_out_top = stage10.io.data_out.asTypeOf(UInt(128.W))
    io.data_out.bits    := RegEnable(data_out_top, running)

    //Debug
    io.running      := running
    io.counter      := counter
    io.peek_stage   := data_reg.asTypeOf(UInt(128.W))
}

/*
 * Time-interleaved AES module without iterative key generation
 * 10 stages plus initial stage
 * Initial and first stage are combined, making for 10 periods to complete
 * Separately initializes keygen from compute. Expects keygen to be ready
 * //TODO explore time-interleaved key_gen
 */
class AES128TimeInterleave extends Module {
    val io = IO (new DataBundleTopDecoupledDebug)

    val keygen = Module(new KeySchedule)
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))
    keygen.io.key_in := key_in_top

    val cipher = Module(new AES128TimeInterleaveCompute)
    cipher.io.data_in.bits := io.data_in.bits
    cipher.io.data_in.valid := io.data_in.valid
    io.data_in.ready := cipher.io.data_in.ready

    cipher.io.key_in := io.key_in
    cipher.io.key_schedule := keygen.io.key_schedule
    cipher.io.key_valid := true.B

    io.data_out.bits := cipher.io.data_out.bits
    io.data_out.valid := cipher.io.data_out.valid
    cipher.io.data_out.ready := io.data_out.ready

    //Debug
    io.running      := cipher.io.running
    io.counter      := cipher.io.counter
    io.peek_stage   := cipher.io.peek_stage
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