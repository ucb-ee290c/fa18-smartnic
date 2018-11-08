package aes

import chisel3._
import chisel3.util._
import interconnect.{CREECBusParams, CREECReadBus, CREECWriteBus}
//import freechips.rocketchip.subsystem.BaseSubsystem
//import freechips.rocketchip.config.{Parameters, Field}
//import freechips.rocketchip.diplomacy._
//import freechips.rocketchip.regmapper.{HasRegMap, RegField}
//import freechips.rocketchip.tilelink._


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
