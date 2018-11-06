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

    readOutput(dut.io.stage1out, "stage1out")
    readOutput(dut.io.stage2key, "stage2key")
    readOutput(dut.io.stage2out, "stage2out")
    readOutput(dut.io.stage3key, "stage3key")
    readOutput(dut.io.stage3out, "stage3out")
    readOutput(dut.io.stage3rcon, "stage3rcon")
    readOutput(dut.io.stage4out, "stage4out")
    readOutput(dut.io.stage5out, "stage5out")
    readOutput(dut.io.stage6out, "stage6out")
    readOutput(dut.io.stage7out, "stage7out")
    readOutput(dut.io.stage8out, "stage8out")
    readOutput(dut.io.stage9out, "stage9out")


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
/*
/**
  * DspTester for AES128
  * Currently runs 1 trial
  */
class AES128Tester(dut: AES128, trial: AESTrial) extends PeekPokeTester(dut) {
    logger info s"Pipelined AES128"
    val maxCyclesWait = 12
    var cyclesWaiting = 0

//    logger info s"counter: ${peek(dut.io.counter)}"
//    logger info s"in ready? : ${peek(dut.io.data_in.ready) == 1}"
    logger info s"Start!"

    poke(dut.io.data_in.bits, trial.data_in)
    poke(dut.io.key_in, trial.key_in)
    poke(dut.io.data_out.ready, 1)
    poke(dut.io.data_in.valid, 1)
    step(1)

    poke(dut.io.data_out.ready, 0)
    poke(dut.io.data_in.valid, 0)
    while ((peek(dut.io.data_out.valid) == 0) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        logger info s"waited: $cyclesWaiting cycles"
        logger info s"counter: ${peek(dut.io.counter)}"
//        logger info s"running? : ${peek(dut.io.running) == 1}"
//        var peekstage = peek(dut.io.peek_stage)
//        logger info s"peek stage? : ${(peekstage >> 64).toLong.toHexString} ${(peekstage << 64 >> 64).toLong.toHexString}"
//        logger info s"in ready? : ${peek(dut.io.data_in.ready) == 1}"
//        logger info s""
        step(1)
      }

    if (cyclesWaiting >= maxCyclesWait) {
      expect(false, "Waited too long")
    }

    val bigIntOut : BigInt = peek(dut.io.data_out.bits)
    logger info s" Output as dec: $bigIntOut"
    val hex0 : Long = (bigIntOut << 64 >> 64).toLong
    val hex1 : Long = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
    logger info s" Expect as hex: ${(trial.ref_out >> 64).toLong.toHexString} ${(trial.ref_out << 64 >> 64).toLong.toHexString}"
    expect(dut.io.data_out.bits, trial.ref_out, "Output did not match!")
}

/**
  * Convenience function for running tests
  */
object AES128Tester {
  def apply(trial: AESTrial): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new AES128()) {
      c => new AES128Tester(c, trial)
    }
  }
}*/
