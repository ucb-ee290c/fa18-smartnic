package aes

import chisel3._
import chisel3.util._
import interconnect.{BusParams, CREECBusParams, CREECBus}

class AESTopBundle extends Bundle {
    val key_in      = Input(UInt(128.W))

    val encrypt_data_in     = Input(UInt(128.W))
    val encrypt_data_out    = Output(UInt(128.W))

    val decrypt_data_in     = Input(UInt(128.W))
    val decrypt_data_out    = Output(UInt(128.W))
}

class AESTopBundleDecoupled extends Bundle {
    val key_in      = Input(UInt(128.W))

    val encrypt_data_in     = Flipped(Decoupled(UInt(128.W)))
    val encrypt_data_out    = Decoupled(UInt(128.W))

    val decrypt_data_in     = Flipped(Decoupled(UInt(128.W)))
    val decrypt_data_out    = Decoupled(UInt(128.W))
}

class AESTopBundleFullyDecoupled extends Bundle {
  val key_in       = Flipped(Decoupled(UInt(128.W)))

  val encrypt_data_in     = Flipped(Decoupled(UInt(128.W)))
  val encrypt_data_out    = Decoupled(UInt(128.W))

  val decrypt_data_in     = Flipped(Decoupled(UInt(128.W)))
  val decrypt_data_out    = Decoupled(UInt(128.W))
}

class AESTopCombinational extends Module {
    val io = IO(new AESTopBundle)

    val encrypt = Module(new AES128Combinational)
    encrypt.io.data_in      := io.encrypt_data_in
    io.encrypt_data_out     := encrypt.io.data_out
    encrypt.io.key_in       := io.key_in

    val decrypt = Module(new InvAES128Combinational)
    decrypt.io.data_in      := io.decrypt_data_in
    io.decrypt_data_out     := decrypt.io.data_out
    decrypt.io.key_in       := io.key_in
}

//TODO: Parametrize
class AESTopTimeInterleave extends Module {
    val io = IO(new AESTopBundleDecoupled)

    val keygen = Module(new KeySchedule)
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))
    keygen.io.key_in := key_in_top

    val encrypt = Module(new AES128TimeInterleaveCompute)
    encrypt.io.data_in.bits      := io.encrypt_data_in.bits
    encrypt.io.data_in.valid     := io.encrypt_data_in.valid
    io.encrypt_data_in.ready     := encrypt.io.data_in.ready

    io.encrypt_data_out.bits     := encrypt.io.data_out.bits
    io.encrypt_data_out.valid    := encrypt.io.data_out.valid
    encrypt.io.data_out.ready    := io.encrypt_data_out.ready

    encrypt.io.key_in       := io.key_in
    encrypt.io.key_schedule := keygen.io.key_schedule
    encrypt.io.key_valid    := true.B

    val decrypt = Module(new InvAES128TimeInterleaveCompute)
    decrypt.io.data_in.bits      := io.decrypt_data_in.bits
    decrypt.io.data_in.valid     := io.decrypt_data_in.valid
    io.decrypt_data_in.ready     := decrypt.io.data_in.ready

    io.decrypt_data_out.bits     := decrypt.io.data_out.bits
    io.decrypt_data_out.valid    := decrypt.io.data_out.valid
    decrypt.io.data_out.ready    := io.decrypt_data_out.ready

    decrypt.io.key_in       := io.key_in
    decrypt.io.key_schedule := keygen.io.key_schedule
    decrypt.io.key_valid    := true.B
}

class AESTopFullTimeInterleave extends Module {
    val io = IO(new AESTopBundleFullyDecoupled)

    val keygen = Module(new KeyScheduleTimeInterleave)
    val key_in_top  = io.key_in.asTypeOf(Vec(16, UInt(8.W)))
    keygen.io.key_in.bits := key_in_top
    keygen.io.key_in.valid := io.key_in.valid
    io.key_in.ready := keygen.io.key_in.ready

    val encrypt = Module(new AES128TimeInterleaveCompute)
    encrypt.io.data_in.bits      := io.encrypt_data_in.bits
    encrypt.io.data_in.valid     := io.encrypt_data_in.valid
    io.encrypt_data_in.ready     := encrypt.io.data_in.ready

    io.encrypt_data_out.bits     := encrypt.io.data_out.bits
    io.encrypt_data_out.valid    := encrypt.io.data_out.valid
    encrypt.io.data_out.ready    := io.encrypt_data_out.ready

    encrypt.io.key_in       := io.key_in.bits
    encrypt.io.key_schedule := keygen.io.key_schedule
    encrypt.io.key_valid    := keygen.io.key_valid

    val decrypt = Module(new InvAES128TimeInterleaveCompute)
    decrypt.io.data_in.bits      := io.decrypt_data_in.bits
    decrypt.io.data_in.valid     := io.decrypt_data_in.valid
    io.decrypt_data_in.ready     := decrypt.io.data_in.ready

    io.decrypt_data_out.bits     := decrypt.io.data_out.bits
    io.decrypt_data_out.valid    := decrypt.io.data_out.valid
    decrypt.io.data_out.ready    := io.decrypt_data_out.ready

    decrypt.io.key_in       := io.key_in.bits
    decrypt.io.key_schedule := keygen.io.key_schedule
    decrypt.io.key_valid    := keygen.io.key_valid
}

//------------------------------------

// CREECBus integration with the AESTop module
//Based on RSEncoderTop
// What to do:
// *** Encrypter
//   + Receive write header from upstream block (slave)
//   + Forward write header to downstream block (master)
//   + Then Receive write data from upstream block (slave)
//   + Send *encrypted* write data to downstream block (master)
//TODO: Add input and output sync FIFOs
//TODO: Add width conversion

//Decrypt and Encrypt use the same state machine
class AESCREECBusFSM(val busParams: BusParams = new CREECBusParams) extends Module {
    val io = IO(new Bundle {
        val slave = Flipped(new CREECBus(busParams))
        val master = new CREECBus(busParams)

        val aes_data_in     = Decoupled(UInt(128.W))
        val aes_data_out    = Flipped(Decoupled(UInt(128.W)))
    })

    //State Definitions
    //Chisel Enum syntax requires the 's' at the beginning
    val sIDLE :: sHEADER_SEND :: sDATA_WAIT :: sCOMP_WAIT :: sCOMPUTE :: sDONE :: Nil = Enum(6)
    val state = RegInit(sIDLE)

    //IO Wrapper Reg for Module
    //TODO: Handle width conversion?
    require(busParams.dataWidth == 128)
    val dataInReg  = Reg(UInt(128.W))
    val dataOutReg = Reg(UInt(128.W))
    val dataIDReg  = Reg(UInt(busParams.idBits.W))

    val headerReg = Reg(chiselTypeOf(io.slave.header.bits))

    //Decoupled IO
    io.slave.header.ready := state === sIDLE
    io.slave.data.ready   := state === sDATA_WAIT

    // Forward the header
    io.master.header.bits <> headerReg
    // Flip the encrypted metadata
    io.master.header.bits.encrypted := ~headerReg.encrypted
    io.master.header.valid := state === sHEADER_SEND


    // TODO: Handle id better
    io.master.data.valid     := state === sDONE
    io.master.data.bits.id   := dataIDReg
    io.master.data.bits.data := dataOutReg

    //Track data beats processed
    val totalBeats = Reg(UInt(busParams.beatBits.W))
    val beatsDone  = Reg(UInt(busParams.beatBits.W))
    val allBeatsDone = beatsDone >= totalBeats

    // Trigger the encoding process when compute is ready
    io.aes_data_in.bits  := dataInReg
    io.aes_data_in.valid := state === sCOMP_WAIT
    io.aes_data_out.ready := state === sDONE

    //State Machine
    switch (state) {
        //Waiting for Header
        //On header fire, transition to wait for data
        is (sIDLE) {
            // Properly reset registers to prepare for the next input
            beatsDone := 0.U
            dataInReg := 0.U
            dataOutReg := 0.U
            dataIDReg := 0.U

            when (io.slave.header.fire()) {
                state := sHEADER_SEND
                totalBeats := (io.slave.header.bits.len + 1.U) // length is 0-indexed
                headerReg := io.slave.header.bits
            }
        }

        is (sHEADER_SEND) {
            when (io.master.header.fire()) {
                state := sDATA_WAIT
            }
        }

        //Waiting for data
        //On data fire, wait for compute to be ready
        is (sDATA_WAIT) {
            when (io.slave.data.fire()) { //start encryption
                state := sCOMP_WAIT
                dataInReg := io.slave.data.bits.data
                dataIDReg := io.slave.data.bits.id
            }
        }
        //Wait for compute to be ready
        is (sCOMP_WAIT) {
            when (io.aes_data_in.fire()) {
                state := sCOMPUTE
            }
        }
        //TODO: cut out this stage
        //Run compute
        //On done fire, transition to done check
        is (sCOMPUTE) {
            //Feed the data to AES and wait for end
            when (io.aes_data_out.valid) {
                state := sDONE
                beatsDone := beatsDone + 1.U
                dataOutReg := io.aes_data_out.bits
            }
        }
        //done check
        //If no more work, reset. Otherwise, wait for data
        is (sDONE) {
            when (io.master.data.fire()) {
                state := Mux(allBeatsDone, sIDLE, sDATA_WAIT)
            }
        }
    }
}

class AESTopCREECBus(val busParams: BusParams = new CREECBusParams) extends Module
    with HWKey {
    val io = IO(new Bundle {
        val encrypt_slave = Flipped(new CREECBus(busParams))
        val encrypt_master = new CREECBus(busParams)

        val decrypt_slave = Flipped(new CREECBus(busParams))
        val decrypt_master = new CREECBus(busParams)
    })

    def connectDecoupled(master: DecoupledIO[Data], slave: DecoupledIO[Data]) = {
        slave.bits := master.bits
        slave.valid := master.valid
        master.ready := slave.ready
    }

    val AESTop = Module(new AESTopFullTimeInterleave)

    // Key ----------------------------------------
    // TODO: Add support for external key

    val myKey = keyAsBigInt().U(128.W)
     
    AESTop.io.key_in.bits := myKey
    // HACK: Reset logic
    val keyValidReg = RegInit(false.B)
    val keyDoneReg = RegInit(false.B)
    AESTop.io.key_in.valid := keyValidReg

    keyValidReg := !keyDoneReg && AESTop.io.key_in.ready
    keyDoneReg  := keyDoneReg || AESTop.io.key_in.fire()
    
    // Encrypt ------------------------------------

    val encrypt_FSM = Module(new AESCREECBusFSM(busParams))
    connectDecoupled(io.encrypt_slave.header, encrypt_FSM.io.slave.header)
    connectDecoupled(io.encrypt_slave.data, encrypt_FSM.io.slave.data)
    connectDecoupled(encrypt_FSM.io.master.header, io.encrypt_master.header)
    connectDecoupled(encrypt_FSM.io.master.data, io.encrypt_master.data)

    connectDecoupled(encrypt_FSM.io.aes_data_in, AESTop.io.encrypt_data_in)
    connectDecoupled(AESTop.io.encrypt_data_out, encrypt_FSM.io.aes_data_out)
    
    // Decrypt ------------------------------------

    val decrypt_FSM = Module(new AESCREECBusFSM(busParams))
    connectDecoupled(io.decrypt_slave.header, decrypt_FSM.io.slave.header)
    connectDecoupled(io.decrypt_slave.data, decrypt_FSM.io.slave.data)
    connectDecoupled(decrypt_FSM.io.master.header, io.decrypt_master.header)
    connectDecoupled(decrypt_FSM.io.master.data, io.decrypt_master.data)

    connectDecoupled(decrypt_FSM.io.aes_data_in, AESTop.io.decrypt_data_in)
    connectDecoupled(AESTop.io.decrypt_data_out, decrypt_FSM.io.aes_data_out)
}

