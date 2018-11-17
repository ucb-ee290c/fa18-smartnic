package interconnect

import org.scalatest._
import chisel3._
import chisel3.experimental.MultiIOModule
import chisel3.tester._
import chisel3.util.ShiftRegister

// A shift register "testbench generator"
class ShifterModule[T <: Data](ioType: T, cycles: Int = 1) extends MultiIOModule {
  val in = IO(Input(ioType))
  val out = IO(Output(ioType))
  out := ShiftRegister(in, cycles)
}

class ShiftRegisterTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2"

  it should "test shift registers with abstractions" in {
    val shifterCycles = 4
    val shifterType = UInt(8.W)
    test(new ShifterModule(shifterType, shifterCycles)) { c =>
      // Exhaustive test
      val vals = (0 until (math.pow(2, shifterType.getWidth)-1).toInt).map(_.U)
      fork {
        for (v <- vals) {
          c.in.poke(v)
          c.clock.step(1)
        }
      }.fork {
        c.clock.step(shifterCycles)
        for (v <- vals) {
          c.out.expect(v)
          c.clock.step(1)
        }
      }.join
    }
  }
}
