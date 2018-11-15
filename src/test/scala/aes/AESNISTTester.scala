package aes

import chisel3._
import chisel3.iotesters.PeekPokeTester

/**
 * Case class holding information needed to run an individual test
 */
case class AESNISTTrial(
    key_in   : BigInt,
    data_in  : BigInt,
    ref_out  : BigInt,
    inv_data_in  : BigInt,
    inv_ref_out  : BigInt,
)

/*
 * In this test, the DUT is first receives a new key and processes it.
 * Then it receives new data to encrypt. After one cycle, it receives new data to decrypt.
 */
class AESNISTFullTimeInterleaveTester(dut: AESTopFullTimeInterleave, trials: Seq[AESNISTTrial]) extends PeekPokeTester(dut) {
  logger info s"Time-interleaved encrypt and decrypt with time-interleaved key gen"
  val maxCyclesWait = 10
  var cyclesWaiting = 0
  var trialcount = 0

  logger info s"Start!"

  for (trial <- trials) {
    logger info s"Starting Trial $trialcount"

    //Wait for key ready
    while ((peek(dut.io.key_in.ready) == 0) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      step(1)
    }
    if (cyclesWaiting > maxCyclesWait) {
      expect(false, "Waited too long")
    }
    cyclesWaiting = 0

    //Setup key first
    expect(dut.io.key_in.ready, 1, "dut should be ready for new key")
    expect(dut.io.encrypt_data_in.ready, 1, "dut should be ready for new data")
    expect(dut.io.encrypt_data_out.valid, 1, "dut output should be valid")

    logger info s"Inserting new key"

    poke(dut.io.key_in.bits, trial.key_in)
    poke(dut.io.key_in.valid, 1)
    step(1)
    expect(dut.io.key_in.ready, 0, "dut should have accepted new key")
    expect(dut.io.encrypt_data_in.ready, 0, "dut should be processing new key")
    expect(dut.io.encrypt_data_out.valid, 0, "dut should be processing new key")
    poke(dut.io.key_in.valid, 0)
    //Should take 9 cycles, after one cycle taken for decrypt poke
    while ((peek(dut.io.key_in.ready) == 0) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      step(1)
    }

    if (cyclesWaiting > maxCyclesWait) {
      expect(false, "Waited too long")
    }

    logger info s"Key ready! Poking stimuli"

    //Run encrypt and decrypt in two different phases
    //Check that the DUT is ready, then poke the stimuli
    expect(dut.io.encrypt_data_out.valid, 1, "dut output should be valid")
    expect(dut.io.encrypt_data_in.ready, 1, "dut should be ready for new data")

    poke(dut.io.encrypt_data_in.bits, trial.data_in)
    poke(dut.io.encrypt_data_out.ready, 1)
    poke(dut.io.encrypt_data_in.valid, 1)
    step(1)
    poke(dut.io.encrypt_data_out.ready, 0)
    poke(dut.io.encrypt_data_in.valid, 0)

    expect(dut.io.decrypt_data_out.valid, 1, "dut output should be valid")
    expect(dut.io.decrypt_data_in.ready, 1, "dut should be ready for new data")

    poke(dut.io.decrypt_data_in.bits, trial.inv_data_in)
    poke(dut.io.decrypt_data_out.ready, 1)
    poke(dut.io.decrypt_data_in.valid, 1)
    step(1)
    poke(dut.io.decrypt_data_out.ready, 0)
    poke(dut.io.decrypt_data_in.valid, 0)

    //Should take 9 cycles, after one cycle taken for decrypt poke
    cyclesWaiting = 0
    while ((peek(dut.io.encrypt_data_out.valid) == 0) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      step(1)
    }

    if (cyclesWaiting > maxCyclesWait) {
      expect(false, "Waited too long")
    }

    logger info s"Encrypt complete"

    //Check independent operation
    expect(dut.io.decrypt_data_out.valid, 0)
    expect(dut.io.decrypt_data_in.ready, 0)

    var bigIntOut: BigInt = peek(dut.io.encrypt_data_out.bits)
    var hex0: Long = (bigIntOut << 64 >> 64).toLong
    var hex1: Long = (bigIntOut >> 64).toLong
    logger info s"Encrypt Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s"Encrypt Expect as hex: ${(trial.ref_out >> 64).toLong.toHexString} ${(trial.ref_out << 64 >> 64).toLong.toHexString}"
    expect(dut.io.encrypt_data_out.bits, trial.ref_out, "Encrypt Output did not match!")

    //Step once more to complete decrypt
    step(1)

    expect(dut.io.decrypt_data_out.valid, 1)
    expect(dut.io.decrypt_data_in.ready, 1)

    logger info s"Decrypt complete"

    bigIntOut = peek(dut.io.decrypt_data_out.bits)
    hex0 = (bigIntOut << 64 >> 64).toLong
    hex1 = (bigIntOut >> 64).toLong
    logger info s"Decrypt Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s"Decrypt Expect as hex: ${(trial.inv_ref_out >> 64).toLong.toHexString} ${(trial.inv_ref_out << 64 >> 64).toLong.toHexString}"
    expect(dut.io.decrypt_data_out.bits, trial.inv_ref_out, "Decrypt Output did not match!")

    trialcount += 1
  }
}


object AESNISTFullTimeInterleaveTester {
  def apply(trials: Seq[AESNISTTrial]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new AESTopFullTimeInterleave()) {
      c => new AESNISTFullTimeInterleaveTester(c, trials)
    }
  }
}