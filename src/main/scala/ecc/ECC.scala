// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._

case class ECCParams(dataWidth: Int = 64) {
  val inW = dataWidth.W
  val outW = (dataWidth + 1).W
}

// TODO: Implement different ECC algorithms and have a parameter
// to select them. EE290C's lecture note is a good starting point
// + single parity check
// + vertical parity check
// + Reed-Solomon check
// + LDPC check

class ECCEncoder (val p: ECCParams = new ECCParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(p.inW)))
    val out = new DecoupledIO(UInt(p.outW))
  })

  val inReadyReg = RegInit(true.B)
  val outValidReg = RegInit(false.B)

  io.in.ready := inReadyReg
  io.out.valid := outValidReg

  val start = RegInit(false.B)
  val dataIn = RegInit(0.U(p.inW))
  val dataOut = RegInit(0.U(p.outW))

  when (io.in.fire()) {
    start := true.B
    dataIn := io.in.bits
    inReadyReg := false.B
  }

  when (start) {
    // Count the number of hot (1) bits of the input
    val numHotBits = PopCount(dataIn)

    when ((numHotBits & 1.U) === 1.U) {
      dataOut := Cat(dataIn, 1.U)
      outValidReg := true.B
    } .otherwise {
      dataOut := dataIn
      outValidReg := true.B
    }

    start := false.B
  }

  when (outValidReg && io.out.ready) {
    outValidReg := false.B
    inReadyReg := true.B
  }

  io.out.bits := dataOut
}
