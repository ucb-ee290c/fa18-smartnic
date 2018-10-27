package aes

import chisel3._
import chisel3.util._
import dsptools.DspTester

/**
 * Case class holding information needed to run an individual test
 */
case class AESTrial(
    data_in  : BigInt, // UInt(128.W)
    key_in   : BigInt, // UInt(128.W)
)

/**
 * DspTester for AES128Combinational
 * Does not work with FIRRTL emulator because of size
 */
class AES128CombinationalTester(dut: AES128Combinational, trial: AESTrial) extends DspTester(dut) {
    poke(dut.io.data_in, trial.data_in)
    poke(dut.io.key_in, trial.key_in)
    peek(dut.io.data_out.data)
}

/**
 * Convenience function for running tests
 */
object AES128CombinationalTester {
  def apply(trial: AESTrial): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new AES128Combinational()) {
      c => new AES128CombinationalTester(c, trial)
    }
  }
}

/**
  * DspTester for AES128
  * Currently runs 1 trial
  */
class AES128Tester(dut: AES128, trial: AESTrial) extends DspTester(dut) {
    //Copied from DspTester b/c the existing implementation does not account for BigInt
    private def dspPeek(node: Data): (Double, BigInt) = {
      val bi: BigInt = updatableDspVerbose.withValue(updatableSubVerbose.value) {
        node match {
          // Unsigned bigint
          case b: Bits => peek(b.asInstanceOf[Bits])
        }
      }
      val (dblOut, bigIntOut) = node match {
        // UInt + SInt = Bits
        case _: Bits => (bi.doubleValue, bi)
      }
      (dblOut, bigIntOut)
    }

    def peekLocal(node: UInt): BigInt = BigInt(dspPeek(node)._1.round)


    //Tester setup
    val maxCyclesWait = 4
    var cyclesWaiting = 0

    poke(dut.io.data_in.bits, trial.data_in)
    poke(dut.io.key_in, trial.key_in)
    poke(dut.io.data_out.ready, 1)
    poke(dut.io.data_in.valid, 1)
    while (!peek(dut.io.data_out.valid) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
    }

    val bigIntOut : BigInt = peekLocal(dut.io.data_out.bits.data)
    logger info s" Output as dec: $bigIntOut"
    val hex0 : Long = (bigIntOut << 64 >> 64).toLong
    val hex1 : Long = (bigIntOut >> 64).toLong
    logger info s" Output as hex: ${hex1.toHexString} ${hex0.toHexString}"
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
}
