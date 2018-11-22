package aes

import chisel3._
import chisel3.util._
import interconnect.{CREECBusParams, CREECBus}
//import freechips.rocketchip.subsystem.BaseSubsystem
//import freechips.rocketchip.config.{Parameters, Field}
//import freechips.rocketchip.diplomacy._
//import freechips.rocketchip.regmapper.{HasRegMap, RegField}
//import freechips.rocketchip.tilelink._


class DataBundle extends Bundle {
    val data_in = Input(Vec(16, UInt(8.W)))
    val data_out = Output(Vec(16, UInt(8.W)))
}

class DataBundleWithKeyIn extends DataBundle {
    val key_in = Input(Vec(16, UInt(8.W)))
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

class KeyScheduleBundleDecoupled extends Bundle {
    val key_in       = Flipped(Decoupled(Vec(16, UInt(8.W))))
    val key_schedule = Output(Vec(10, (Vec(16, UInt(8.W)))))
    val key_valid    = Output(Bool())
}

class KeyScheduleTimeInterleave extends Module {
    val io = IO(new KeyScheduleBundleDecoupled)

    //State Machine ---------------------------------------
    val numStages = 10 //for AES128

    val start = io.key_in.fire
    val counter = RegInit(0.U(4.W))
    val running = counter < numStages.U

    counter := Mux(running, counter+1.U,
        Mux(start, 0.U, counter))

    val mux_select = start

    // Ready Valid
    io.key_in.ready   := !running
    io.key_valid      := !running

    //Compute ----------------------------------------------
    val rcon_reg    = Reg(UInt(8.W))
    val key_reg     = Reg(Vec(16, UInt(8.W)))

    val rcon_gen = Module(new RCON)
    rcon_gen.io.last_rcon := rcon_reg

    val roundkey_gen = Module(new KeyExpansion)
    roundkey_gen.io.rcon :=  rcon_reg
    roundkey_gen.io.key_in := key_reg

    val rcon_next   = rcon_gen.io.next_rcon
    val key_next    = roundkey_gen.io.key_out

    key_reg     := Mux(mux_select, io.key_in.bits, key_next)
    rcon_reg    := Mux(mux_select, 1.U, rcon_next)

    //Output ----------------------------------------------
    val key_schedule_reg = Reg(Vec(10, Vec(16,UInt(8.W))))

    for (i <- 0 until 10) {
        key_schedule_reg(i) := Mux(counter === i.U, key_next, key_schedule_reg(i))
        io.key_schedule(i)  := key_schedule_reg(i)
    }
}


class DataBundleWithKeyInDebug extends DataBundleWithKeyIn {
    val subbyteout = Output(UInt(128.W))
    val shiftrowsout  = Output(UInt(128.W))
    val mixcolsout = Output(UInt(128.W))
    val addroundkey = Output(UInt(128.W))
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

class DataBundleKeyScheduleDecoupled extends Bundle {
    val data_in      = Flipped(Decoupled(UInt(128.W)))
    val data_out     = Decoupled(UInt(128.W))
    val key_in       = Input(UInt(128.W))
    val key_schedule = Input(Vec(10, Vec(16, UInt(8.W))))
    val key_valid    = Input(Bool())
}

class DataBundleKeyScheduleDecoupledDebug extends DataBundleKeyScheduleDecoupled {
    val running     = Output(Bool())
    val peek_stage  = Output(UInt(128.W))
    val counter     = Output(UInt(4.W))
}

trait HWKey {
    val key = Seq(1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 3, 3, 2).map(
        _.asInstanceOf[Byte])

    def keyAsBigInt(): BigInt = {
        var rr : BigInt = 0
        for (i <- 0 until key.length) {
            rr = (rr << 8) + BigInt(key(i))
        }
        rr
    }
}
