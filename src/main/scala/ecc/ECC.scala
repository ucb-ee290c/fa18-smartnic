// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._
import interconnect._

// References:
// [1] http://ptgmedia.pearsoncmg.com/images/art_sklar7_reed-solomon/elementLinks/art_sklar7_reed-solomon.pdf
// [2] https://downloads.bbc.co.uk/rd/pubs/whp/whp-pdf-files/WHP031.pdf
// Here is a brief description on one example of Reed-Solomon encoder
// Reed-Solomon(7, 3) with 3-bit symbol
// #symbols: n=7, #message symbols: k=3, #parity symbols: n-k=4
// With symbolWidth = 3 (or GF(2^3))
// a^0 --> 1
// a^1 --> 2
// a^2 --> 4
// a^3 --> 3
// a^4 --> 6
// a^5 --> 7
// a^6 --> 5
// a^7 --> 1
// We need two things:
// Primitive polynomial f(X) = X^3 + X^1 + 1 --> 1011(2) --> 11(10)
// Generator polynomial g(X) = a^3 * X^0 + a^1 * X^1 + a^0 * X^2 + a^3 * X^3 + X^4
//                      g(X) =   3 * X^0 +   2 * X^1 +   1 * X^2 +   3 * X^3 + X^4
// output(X) = message(X) * X^(n - k) + (message(X) * X^(n - k) % g(X))
// Note that the arithmetic operations in GF(2^m) are different from the conven-
// tional ones
// Addition is simply bit-wise XOR
// Multiplication is slightly more complicated. The result needs to be mod by
// the value representing the primitive polynomial (in this case, 11)
// In general, a GF operations of two m-bit operands results to a m-bit value
//
case class RSParams(
  val n: Int = 7,
  val k: Int = 3,
  val symbolWidth: Int = 3,
  val gCoeffs: Seq[Int] = Seq(3, 2, 1, 3),
  val fConst: Int = 11,
  val Log2Val: Seq[Int] = Seq(1, 2, 4, 3, 6, 7, 5, 1),
  val Val2Log: Seq[Int] = Seq(0, 7, 1, 3, 2, 6, 4, 5)
)

// TODO: Evaluate the effectiveness of this combinational circuit
// of doing Galois multiplication versus the simpler approach of using
// a Lookup Table
object GMul {
  def apply(a: UInt, b: UInt, dataWidth: Int, fConst: UInt): UInt = {
    val op1 = a.asTypeOf(UInt(dataWidth.W))
    val op2 = b.asTypeOf(UInt(dataWidth.W))
    val tmp = Wire(Vec(dataWidth, UInt((2 * dataWidth - 1).W)))
    for (i <- dataWidth - 1 to 0 by - 1) {
      val tmp0 = if (i == dataWidth - 1) {
                 Mux(op2(i), op1 << i, 0.U)
               } else {
                 tmp(i + 1) ^ Mux(op2(i), op1 << i, 0.U)
               }

      val tmp1 = if (i == 0) {
                tmp0
              } else {
                Mux(tmp0(i + dataWidth - 1), tmp0 ^ fConst << (i - 1), tmp0)
              }

      tmp(i) := tmp1
    }
    tmp(0)
  }
}

// This module will accept k symbols (io.in.fire() === true.B until received k symbols)
// It will emit n symbols (io.out.fire() === true.B until sent n symbols)
// Each symbol has a width of *symbolWidth*
// FIXME: the incoming data is likely to be a multiple of symbol width
// TODO:
//   + Incorporate CREECBus
class RSEncoder(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  val inReadyReg = RegInit(true.B)
  val outValidReg = RegInit(false.B)

  val inputSymbolCnt = RegInit(0.U(32.W))
  val outputSymbolCnt = RegInit(0.U(32.W))

  io.in.ready := inReadyReg
  io.out.valid := outValidReg

  when (inputSymbolCnt === p.k.asUInt() - 1.U) {
    inputSymbolCnt := 0.U
    inReadyReg := false.B
  }
  .elsewhen (io.in.fire()) {
    inputSymbolCnt := inputSymbolCnt + 1.U
    outValidReg := true.B
  }

  when (outputSymbolCnt === p.n.asUInt() - 1.U) {
    outputSymbolCnt := 0.U
    outValidReg := false.B
    inReadyReg := true.B
  }
  .elsewhen (io.out.fire()) {
    outputSymbolCnt := outputSymbolCnt + 1.U
  }

  val Regs = RegInit(VecInit(Seq.fill(p.n - p.k)(0.U(p.symbolWidth.W))))
  val inputBitsReg = RegNext(io.in.bits, 0.U)

  // Make sure the arithmetic operations are correct (in Galois field)
  val feedback = Mux(outputSymbolCnt < p.k.asUInt(),
                     inputBitsReg ^ Regs(p.n - p.k - 1), 0.U)
  for (i <- 0 until p.n - p.k) {
    if (i == 0) {
      Regs(0) := GMul(feedback, p.gCoeffs(0).asUInt(),
                      p.symbolWidth, p.fConst.asUInt())
    } else {
      Regs(i) := Regs(i - 1) ^ GMul(feedback, p.gCoeffs(i).asUInt(),
                                    p.symbolWidth, p.fConst.asUInt())
    }
  }

  io.out.bits := Mux(outputSymbolCnt < p.k.asUInt(),
                     inputBitsReg, Regs(p.n - p.k - 1))
}

class SyndromeCell(val p: RSParams = new RSParams(),
                   val cellIndex: Int = 0)
  extends Module {
  val io = IO(new Bundle {
    val SIn = Input(UInt(p.symbolWidth.W))
    val Rx = Input(UInt(p.symbolWidth.W))
    val passThrough = Input(Bool())

    val SOut = Output(UInt(p.symbolWidth.W))
  })

  val Reg0 = RegInit(0.U(p.symbolWidth.W))
  val Reg1 = RegInit(0.U(p.symbolWidth.W))
  val Reg2 = RegInit(0.U(p.symbolWidth.W))

  Reg0 := io.Rx
  Reg1 := GMul((Reg0 ^ Reg1), p.Log2Val(cellIndex + 1).asUInt(),
               p.symbolWidth, p.fConst.asUInt())
  Reg2 := Mux(io.passThrough, (Reg0 ^ Reg1), io.SIn)
  io.SOut := Reg2
}

// This is the first step of RSDecoder
// Here we want to verify whether the incoming sequence of symbols forms
// a polynomial in(X) that has roots of a^1, a^2, ..., a^(n - k)
// i.e., in(a^1) = 0, in(a^2) = 0, ..., in(a^(n - k)) = 0
// in(X) = in(0) * X^(n - 1) + in(1) * X^(n - 2) + ... + in(n - 1) * X^0
// We want to use systolic array of (n - k) cells, where each cell computes
// in(a^i), i = 1..(n - k)
// The cells chain together like this:
// cell_(n - k - 1) --> cell_(n - k - 2) --> ... --> cell_1 --> cell_0 --> output
// like a shift register
// At each cycle, we broadcast one input symbol to all the cells. Each cell
// performs individual computation and stores its temporary result into internal
// registers. Once all the (n) input symbols have been supplied to the cells,
// the cells should have their final value (which hopefully is zero).
// Then each cell passes its value to the previous cell in a shift-register
// fashion. Thus, at the output side, we would expect to receive (n - k) zeroes
// This is a very simple version of systolic array which cells do not communicate
// often (only passing data at the end)
class SyndromeCompute(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  val cells = Seq.tabulate(p.n - p.k)(i => Module(new SyndromeCell(p, i)))
  val sInit :: sCompute :: sDone :: Nil = Enum(3)
  val state = RegInit(sInit)

  val inCnt = RegInit(0.U(32.W))
  val outCnt = RegInit(0.U(32.W))

  io.in.ready := (state === sCompute)
  io.out.valid := (state === sDone)
  io.out.bits := cells(0).io.SOut

  // Systolic array of SyndromeCells
  for (i <- p.n - p.k - 1 to 0 by - 1) {
    cells(i).io.Rx := io.in.bits

    if (i == p.n - p.k - 1) {
      cells(i).io.SIn := 0.U
    } else {
      cells(i).io.SIn := cells(i).io.SOut
    }

    cells(i).io.passThrough := (state === sCompute)
  }

  when (state === sInit) {
    state := sCompute
    inCnt := 0.U
    outCnt := 0.U
  }

  when (state === sCompute) {
    when (io.in.fire()) {
      when (inCnt === p.n.asUInt() - 1.U) {
        state := sDone
      }
      .otherwise {
        inCnt := inCnt + 1.U
      }
    }
  }

  when (state === sDone) {
    when (io.out.fire()) {
      when (outCnt === (p.n - p.k).asUInt() - 1.U) {
        state := sInit
      }
      .otherwise {
        outCnt := outCnt + 1.U
      }
    }
  }

}

// Top-level ECC module that hooks up with the rest of the system
// What to do:
//   + Receive write request from upstream block (slave)
//   + Send write request to downstream block (master)
//   + Receive write data from upstream block (slave)
//   + Send write data to downstream block (master)
//   + Receive read request from upstream block (slave)
//   + Send read request to downstream block (master)
//   + Receive read response from downstream block (master)
//   + Send read response to upstream block (slave)
// TODO: Make it generic (parameterized) to different ECC algorithms
class ECC(val rsParams: RSParams = new RSParams(),
          val busParams: CREECBusParams = new CREECBusParams()
  ) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(busParams))
    val master = new CREECBus(busParams)
  })

  io.master.wrReq.bits.compressed := false.B
  io.master.wrReq.bits.ecc := true.B
  io.master.wrReq.bits.encrypted := false.B

  io.slave.rdData.bits.compressed := io.master.rdData.bits.compressed
  io.slave.rdData.bits.ecc := io.master.rdData.bits.ecc
  io.slave.rdData.bits.encrypted := io.master.rdData.bits.encrypted

  io.master.wrReq.bits.addr := io.slave.wrReq.bits.addr
  io.master.wrReq.bits.len := io.slave.wrReq.bits.len
  io.master.wrReq.bits.id := io.slave.wrReq.bits.id

  io.master.wrData.bits.id := io.slave.wrData.bits.id

  // TODO: handle read request
  // Before even doing that, I need to implement the RSDecoder
  io.slave.rdReq.ready := false.B

  io.master.rdReq.bits.addr := io.slave.rdReq.bits.addr
  io.master.rdReq.bits.len := io.slave.rdReq.bits.len
  io.master.rdReq.bits.id := io.slave.rdReq.bits.id

  io.master.rdReq.valid := false.B

  io.slave.rdData.bits.id := io.master.rdData.bits.id
  io.slave.rdData.bits.data := 0.U
  io.slave.rdData.valid := false.B

  io.master.rdData.ready := false.B

  val numItems = rsParams.n * rsParams.symbolWidth / busParams.dataWidth
  val sInit :: sCompute :: sDone :: Nil = Enum(3)
  val state = RegInit(sInit)

  val enc = Module(new RSEncoder(rsParams))

  val dataInReg = RegInit(0.U(busParams.dataWidth.W))
  val dataOutReg = RegInit(0.U((rsParams.n * rsParams.symbolWidth).W))

  io.slave.wrReq.ready := state === sInit
  io.slave.wrData.ready :=  state === sInit
  io.master.wrReq.valid := state === sDone
  io.master.wrData.valid := state === sDone
  io.master.wrData.bits.data := dataOutReg(busParams.dataWidth - 1, 0)

  val encInCnt = RegInit(0.U(32.W))
  val encOutCnt = RegInit(0.U(32.W))
  val itemCnt = RegInit(0.U(32.W))

  enc.io.in.valid := (state === sCompute) && (encInCnt < rsParams.k.asUInt())
  enc.io.in.bits := dataInReg
  enc.io.out.ready := (state === sCompute) && (encOutCnt < rsParams.n.asUInt())

  when (state === sInit) {
    encInCnt := 0.U
    encOutCnt := 0.U
    itemCnt := 0.U

    when (io.slave.wrReq.fire() && io.slave.wrData.fire()) {
      state := sCompute
      dataInReg := io.slave.wrData.bits.data
    }
  }

  when (state === sCompute) {
    when (enc.io.in.fire()) {
      encInCnt := encInCnt + 1.U
      dataInReg := (dataInReg >> rsParams.symbolWidth)
    }

    when (enc.io.out.fire()) {
      when (encOutCnt === rsParams.n.asUInt() - 1.U) {
        state := sDone
      }
      .otherwise {
        encOutCnt := encOutCnt + 1.U
      }
      dataOutReg := (dataOutReg << rsParams.symbolWidth) + enc.io.out.bits
    }
  }

  when (state === sDone) {
    when (io.master.wrReq.fire() && io.master.wrData.fire()) {
      dataOutReg := (dataOutReg >> busParams.dataWidth)
      when (itemCnt === numItems.asUInt() - 1.U) {
        state := sInit
      }
      .otherwise {
        itemCnt := itemCnt + 1.U
      }
    }
  }
}
