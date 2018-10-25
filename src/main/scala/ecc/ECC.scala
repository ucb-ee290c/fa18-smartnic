// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._

class ECC extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(64.W)))
    val out = new DecoupledIO(UInt(64.W))
  })

  val inReadyReg = RegInit(true.B)
  val outValidReg = RegInit(true.B)

  io.in.ready := inReadyReg
  io.out.valid := outValidReg
  io.out.bits <> io.in.bits

}
