package interconnect

import org.scalatest._
import chisel3._
import chisel3.experimental.MultiIOModule
import chisel3.tester._
import chisel3.util.ShiftRegister

// A more sensible shift register test
class ShifterModule[T <: Data](ioType: T, cycles: Int = 1) extends MultiIOModule {
  val in = IO(Input(ioType))
  val out = IO(Output(ioType))
  out := ShiftRegister(in, cycles)
}

// TODO: upstream this version of the test to chisel-testers2
class ShiftRegisterTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2"

  it should "test shift registers with abstractions" in {
    test(new ShifterModule(UInt(8.W), 4)) { c =>
      val vals = (42 until 256).map(_.U)
      fork {
        for (v <- vals) {
          c.in.poke(v)
          c.clock.step(1)
        }
      }.fork {
        c.clock.step(4)
        for (v <- vals) {
          c.out.expect(v)
          c.clock.step(1)
        }
      }.join
    }
  }
}
