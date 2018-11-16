package aes

import chisel3._
import chisel3.iotesters.PeekPokeTester


/**
 * DspTester for InvAES128Combinational
 * Does not work with FIRRTL emulator because of size
 */
class InvAES128CombinationalTester(dut: InvAES128Combinational, trial: AESTrial) extends PeekPokeTester(dut) {
    def readOutput(signal: Bits, name : String): Unit = {
      val bigIntOut : BigInt = peek(signal)
      val hex0 : Long = (bigIntOut << 64 >> 64).toLong
      val hex1 : Long = (bigIntOut >> 64).toLong
      logger info s" $name : Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    }

    poke(dut.io.data_in, trial.data_in)
    poke(dut.io.key_in, trial.key_in)
    step(1)

    val bigIntOut : BigInt = peek(dut.io.data_out)
    logger info s"Combinational"

    val hex0 : Long = (bigIntOut << 64 >> 64).toLong
    val hex1 : Long = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: ${(trial.ref_out >> 64).toLong.toHexString} ${(trial.ref_out << 64 >> 64).toLong.toHexString}"
    expect(dut.io.data_out, trial.ref_out, "Output did not match!")
}

/**
 * Convenience function for running tests
 */
object InvAES128CombinationalTester {
  def apply(trial: AESTrial): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new InvAES128Combinational()) {
      c => new InvAES128CombinationalTester(c, trial)
    }
  }
}

/**
  * DspTester for InvAES128
  * Currently runs 1 trial
  */
class InvAES128Tester(dut: InvAES128, trial: AESTrial) extends PeekPokeTester(dut) {
    logger info s"Time Interleaved InvAES128"
    val maxCyclesWait = 14
    var cyclesWaiting = 0

    logger info s"Start!"
    //Setup key
    poke(dut.io.key_in, trial.key_in)
    step(1)

    poke(dut.io.data_in.bits, trial.data_in)
    poke(dut.io.data_out.ready, 1)
    poke(dut.io.data_in.valid, 1)
    step(1)

    poke(dut.io.data_out.ready, 0)
    poke(dut.io.data_in.valid, 0)

    while ((peek(dut.io.data_out.valid) == 0) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        logger info s"waited: $cyclesWaiting cycles"
        logger info s"counter: ${peek(dut.io.counter)}"
        step(1)
      }

    if (cyclesWaiting >= maxCyclesWait) {
      expect(false, "Waited too long")
    }

    val bigIntOut : BigInt = peek(dut.io.data_out.bits)
    val hex0 : Long = (bigIntOut << 64 >> 64).toLong
    val hex1 : Long = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: ${(trial.ref_out >> 64).toLong.toHexString} ${(trial.ref_out << 64 >> 64).toLong.toHexString}"
    expect(dut.io.data_out.bits, trial.ref_out, "Output did not match!")
}

/**
  * Convenience function for running tests
  */
object InvAES128Tester {
  def apply(trial: AESTrial): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new InvAES128()) {
      c => new InvAES128Tester(c, trial)
    }
  }
}
