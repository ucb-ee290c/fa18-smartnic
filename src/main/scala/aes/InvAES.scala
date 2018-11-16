package aes

import chisel3._
import chisel3.util._
//import interconnect.{CREECBusParams, CREECReadBus, CREECWriteBus}
//import freechips.rocketchip.subsystem.BaseSubsystem
//import freechips.rocketchip.config.{Parameters, Field}
//import freechips.rocketchip.diplomacy._
//import freechips.rocketchip.regmapper.{HasRegMap, RegField}
//import freechips.rocketchip.tilelink._

trait hasInvSubByte {
    val invSBoxTable = VecInit(
    Seq(82, 9, 106, 213, 48, 54, 165, 56, 191, 64, 163, 158, 129, 243, 215, 251,
        124, 227, 57, 130, 155, 47, 255, 135, 52, 142, 67, 68, 196, 222, 233, 203,
        84, 123, 148, 50, 166, 194, 35, 61, 238, 76, 149, 11, 66, 250, 195, 78,
        8, 46, 161, 102, 40, 217, 36, 178, 118, 91, 162, 73, 109, 139, 209, 37,
        114, 248, 246, 100, 134, 104, 152, 22, 212, 164, 92, 204, 93, 101, 182, 146,
        108, 112, 72, 80, 253, 237, 185, 218, 94, 21, 70, 87, 167, 141, 157, 132,
        144, 216, 171, 0, 140, 188, 211, 10, 247, 228, 88, 5, 184, 179, 69, 6,
        208, 44, 30, 143, 202, 63, 15, 2, 193, 175, 189, 3, 1, 19, 138, 107,
        58, 145, 17, 65, 79, 103, 220, 234, 151, 242, 207, 206, 240, 180, 230, 115,
        150, 172, 116, 34, 231, 173, 53, 133, 226, 249, 55, 232, 28, 117, 223, 110,
        71, 241, 26, 113, 29, 41, 197, 137, 111, 183, 98, 14, 170, 24, 190, 27,
        252, 86, 62, 75, 198, 210, 121, 32, 154, 219, 192, 254, 120, 205, 90, 244,
        31, 221, 168, 51, 136, 7, 199, 49, 177, 18, 16, 89, 39, 128, 236, 95,
        96, 81, 127, 169, 25, 181, 74, 13, 45, 229, 122, 159, 147, 201, 156, 239,
        160, 224, 59, 77, 174, 42, 245, 176, 200, 235, 187, 60, 131, 83, 153, 97,
        23, 43, 4, 126, 186, 119, 214, 38, 225, 105, 20, 99, 85, 33, 12, 125)
      .map(_.U(8.W))
    )

    def invSubByte(byte_in: UInt): UInt = {
       invSBoxTable(byte_in)
   }
}

//Byte substitution using the S-box method
class InvSubByte extends Module with hasInvSubByte {
    val io = IO(new DataBundle())

    for (i <- 0 until 16) {
        io.data_out(i) := invSubByte(io.data_in(i))
    }
}

//shuffle the 16-bit array
class InvShiftRows extends Module {
    val io = IO(new DataBundle())

    io.data_out(0) := io.data_in(0)
    io.data_out(5) := io.data_in(1)
    io.data_out(10) := io.data_in(2)
    io.data_out(15) := io.data_in(3)

    io.data_out(4) := io.data_in(4)
    io.data_out(9) := io.data_in(5)
    io.data_out(14) := io.data_in(6)
    io.data_out(3) := io.data_in(7)

    io.data_out(8) := io.data_in(8)
    io.data_out(13) := io.data_in(9)
    io.data_out(2) := io.data_in(10)
    io.data_out(7) := io.data_in(11)

    io.data_out(12) := io.data_in(12)
    io.data_out(1) := io.data_in(13)
    io.data_out(6) := io.data_in(14)
    io.data_out(11) := io.data_in(15)
}

class InvMixColumnsMM extends Module with MixColumnsFunctions{
    val io = IO(new MMDataBundle())

    io.data_out(0) := gmul14(io.data_in(0)) ^ gmul11(io.data_in(1)) ^ gmul13(io.data_in(2)) ^ gmul9(io.data_in(3))
    io.data_out(1) := gmul9(io.data_in(0)) ^ gmul14(io.data_in(1)) ^ gmul11(io.data_in(2)) ^ gmul13(io.data_in(3))
    io.data_out(2) := gmul13(io.data_in(0)) ^ gmul9(io.data_in(1)) ^ gmul14(io.data_in(2)) ^ gmul11(io.data_in(3))
    io.data_out(3) := gmul11(io.data_in(0)) ^ gmul13(io.data_in(1)) ^ gmul9(io.data_in(2)) ^ gmul14(io.data_in(3))
}

//Column Mixing provides the primary obfuscation in AES
//TODO: Make generic and take in MMColumns as a parameter?
class InvMixColumns extends Module {
    val io = IO(new DataBundle())

    //Cast to a 4x4 matrix
    val d_in = io.data_in.asTypeOf(Vec(4, Vec(4, UInt(8.W))))
    val d_out = Wire(Vec(4, Vec(4, UInt(8.W))))
    io.data_out := d_out.asTypeOf(Vec(16, UInt(8.W)))

    val MM0 = Module(new InvMixColumnsMM())
    val MM1 = Module(new InvMixColumnsMM())
    val MM2 = Module(new InvMixColumnsMM())
    val MM3 = Module(new InvMixColumnsMM())

    MM0.io.data_in := d_in(0)
    d_out(0) := MM0.io.data_out

    MM1.io.data_in := d_in(1)
    d_out(1) := MM1.io.data_out

    MM2.io.data_in := d_in(2)
    d_out(2) := MM2.io.data_out

    MM3.io.data_in := d_in(3)
    d_out(3) := MM3.io.data_out
}

class InvAESCipherStage extends Module {
    val io = IO(new DataBundleWithKeyInDebug())

    val add_round_key = Module(new AddRoundKey())
    val inv_mix_columns = Module(new InvMixColumns())
    val inv_sub_byte = Module(new InvSubByte())
    val inv_shift_rows = Module(new InvShiftRows())

    add_round_key.io.key_in := io.key_in

    //Chain modules together
    add_round_key.io.data_in :=  io.data_in
    inv_mix_columns.io.data_in := add_round_key.io.data_out
    inv_shift_rows.io.data_in := inv_mix_columns.io.data_out
    inv_sub_byte.io.data_in := inv_shift_rows.io.data_out
    io.data_out := inv_sub_byte.io.data_out

    //Debug
    io.subbyteout := inv_sub_byte.io.data_out.asTypeOf(UInt(128.W))
    io.shiftrowsout := inv_shift_rows.io.data_out.asTypeOf(UInt(128.W))
    io.mixcolsout := inv_mix_columns.io.data_out.asTypeOf(UInt(128.W))
    io.addroundkey := add_round_key.io.data_out.asTypeOf(UInt(128.W))
}

class InvAESCipherInitStage extends Module {
    val io = IO(new DataBundleWithKeyIn())

    val add_round_key = Module(new AddRoundKey())
    val inv_sub_byte = Module(new InvSubByte())
    val inv_shift_rows = Module(new InvShiftRows())

    add_round_key.io.key_in := io.key_in

    //Chain modules together
    add_round_key.io.data_in := io.data_in
    inv_shift_rows.io.data_in := add_round_key.io.data_out
    inv_sub_byte.io.data_in := inv_shift_rows.io.data_out
    io.data_out := inv_sub_byte.io.data_out
}

//Purely combinational form
//Generally avoid this block because its long critical path
class InvAES128Combinational extends Module with connectsStages {
    val io = IO(new DataBundleTopDebug)

    val data_in_top = io.data_in.asTypeOf(Vec(16, UInt(8.W)))
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))

    val keygen = Module(new KeySchedule)
    keygen.io.key_in := key_in_top
    val key_schedule = keygen.io.key_schedule

    //Initial round
    val stage0 = Module(new InvAESCipherInitStage())
    stage0.io.key_in := key_schedule(9)
    stage0.io.data_in := data_in_top

    // Round1
    val stage1 = Module( new InvAESCipherStage)
    stage1.io.key_in := key_schedule(8)
    stage1.io.data_in := stage0.io.data_out

    // Round1
    val stage2 = Module( new InvAESCipherStage)
    connectInvStages(stage1, stage2, key_schedule(7))

    // Round3
    val stage3 = Module( new InvAESCipherStage)
    connectInvStages(stage2, stage3, key_schedule(6))

    // Round4
    val stage4 = Module( new InvAESCipherStage)
    connectInvStages(stage3, stage4, key_schedule(5))

    // Round5
    val stage5 = Module( new InvAESCipherStage)
    connectInvStages(stage4, stage5, key_schedule(4))

    // Round6
    val stage6 = Module( new InvAESCipherStage)
    connectInvStages(stage5, stage6, key_schedule(3))

    // Round7
    val stage7 = Module( new InvAESCipherStage)
    connectInvStages(stage6, stage7, key_schedule(2))

    // Round8
    val stage8 = Module( new InvAESCipherStage)
    connectInvStages(stage7, stage8, key_schedule(1))

    // Round9
    val stage9 = Module( new InvAESCipherStage)
    connectInvStages(stage8, stage9, key_schedule(0))


    // output round
    val stage10 = Module(new AddRoundKey())
    stage10.io.key_in := key_in_top
    stage10.io.data_in := stage9.io.data_out
    val stage10_data_out = stage10.io.data_out

    io.data_out := stage10_data_out.asTypeOf(UInt(128.W))

    io.stage9out := stage9.io.data_out.asTypeOf(UInt(128.W))
    io.stage8out := stage8.io.data_out.asTypeOf(UInt(128.W))
    io.stage7out := stage7.io.data_out.asTypeOf(UInt(128.W))
    io.stage6out := stage6.io.data_out.asTypeOf(UInt(128.W))
    io.stage5out := stage5.io.data_out.asTypeOf(UInt(128.W))
    io.stage4out := stage4.io.data_out.asTypeOf(UInt(128.W))
    io.stage3out := stage3.io.data_out.asTypeOf(UInt(128.W))
    io.stage2out := stage2.io.data_out.asTypeOf(UInt(128.W))
    io.stage1out := stage1.io.data_out.asTypeOf(UInt(128.W))
    io.stage3key := key_schedule(3).asTypeOf(UInt(128.W))
    io.stage2key := key_schedule(2).asTypeOf(UInt(128.W))
    io.stage3rcon := 4.U //TODO: deprecate
}

class InvAES128TimeInterleaveCompute extends Module {
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

    val mux_select_stage0 = counter === numStages.U

    // Ready Valid
    io.data_in.ready  := !running && io.key_valid
    io.data_out.valid := !running && io.key_valid

    //Computations -----------------------------------------
    //Initial round
    val stage0 = Module(new InvAESCipherInitStage())
    stage0.io.key_in := key_schedule(9)
    stage0.io.data_in := data_in_top
    val stage0_data_out = stage0.io.data_out

    //stages 1-8
    val data_reg    = Reg(Vec(16, UInt(8.W)))

    val InvAESStage = Module(new InvAESCipherStage)
    InvAESStage.io.data_in     := data_reg
    InvAESStage.io.key_in      := key_schedule(counter - 1.U)

    val data_next   = InvAESStage.io.data_out
    data_reg    := Mux(mux_select_stage0, stage0_data_out, data_next)

    // output round
    val stage9 = Module( new InvAESCipherStage)
    stage9.io.data_in := data_reg
    stage9.io.key_in := key_schedule(0)

    val stage10 = Module(new AddRoundKey())
    stage10.io.key_in := key_in_top
    stage10.io.data_in := stage9.io.data_out
    val stage10_data_out = stage10.io.data_out

    val data_out_top = stage10.io.data_out.asTypeOf(UInt(128.W))
    io.data_out.bits    := RegEnable(data_out_top, running)

    //Debug
    io.running      := running
    io.counter      := counter
    io.peek_stage   := data_reg.asTypeOf(UInt(128.W))
}

/*
 * Time-interleaved AES module
 * 10 stages plus initial stage
 * Initial and first stage are combined, making for 10 periods to complete
 * Separately initializes keygen from compute. Expects keygen to be ready
 * //TODO explore time-interleaved key_gen
 */
class InvAES128 extends Module {
    val io = IO (new DataBundleTopDecoupledDebug)

    val keygen = Module(new KeySchedule)
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))
    keygen.io.key_in := key_in_top

    val cipher = Module(new InvAES128TimeInterleaveCompute)
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
