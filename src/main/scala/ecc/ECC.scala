// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._
import interconnect.{CREECBusParams, CREECWriteBus}
//import interconnect._

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

object GInv {
  def shlByOne(a: UInt, dataWidth: Int, fConst: UInt): UInt = {
    val mask = math.pow(2, dataWidth).toInt - 1
    val tmp = Wire(UInt((dataWidth + 1).W))
    tmp := a << 1
    val result = Mux(tmp(dataWidth), (tmp & mask.asUInt()) ^
                                     (fConst & mask.asUInt()), tmp)
    result
  }

  def apply(a: UInt, dataWidth: Int, fConst: UInt): UInt = {
    val op = a.asTypeOf(UInt(dataWidth.W))
    val numVals = math.pow(2, dataWidth).toInt

    val rootsFromOp = Seq.fill(numVals)(Wire(UInt(dataWidth.W))).scan(op)( 
      (prev: UInt, next: UInt) => {
      next := shlByOne(prev, dataWidth, fConst)
      next
    })

    val rootsFromOne = Seq.fill(numVals)(Wire(UInt(dataWidth.W))).scan(1.U)( 
      (prev: UInt, next: UInt) => {
      next := shlByOne(prev, dataWidth, fConst)
      next
    })

    val rootChecks = rootsFromOp.zip(rootsFromOne).map{
      case(rootFromOp: UInt, rootFromOne: UInt) =>
        Mux(rootFromOp === 1.U, rootFromOne, 0.U) }

    val result = rootChecks.reduce(_ ^ _)
    result
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

class PolyCell(val dataWidth: Int, val fConst: Int, cellIndex: Int)
  extends Module {
  val io = IO(new Bundle {
    val running = Input(Bool())
    val SIn = Input(UInt(dataWidth.W))
    val coeff = Input(UInt(dataWidth.W))
    val Rx = Input(UInt(dataWidth.W))

    val SOut = Output(UInt(dataWidth.W))
  })

  val Reg0 = RegInit(0.U(dataWidth.W))
  val Reg1 = RegInit(0.U(dataWidth.W))
  val Reg2 = RegInit(0.U(dataWidth.W))

  when (io.running) {
    Reg0 := io.Rx
    Reg1 := GMul((Reg0 ^ Reg1), io.coeff,
                 dataWidth, fConst.asUInt())
  }
  Reg2 := Mux(io.running, (Reg0 ^ Reg1), io.SIn)

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
// in (n - k) successive cycles
// This is a very simple version of systolic array which cells do not communicate
// often (only passing data at the end)
// This implementation is based on Horner's method
class PolyCompute(val dataWidth: Int,
                  val numCells: Int,
                  val numInputs: Int,
                  val coeffs: Seq[Int],
                  val fConst: Int)
  extends Module {
  val io = IO(new Bundle {
    val coeff = Input(UInt(dataWidth.W))
    val in = Flipped(new DecoupledIO(UInt(dataWidth.W)))
    val out = new DecoupledIO(UInt(dataWidth.W))
  })

  val cells = Seq.tabulate(numCells)(i => Module(new PolyCell(dataWidth, fConst, i)))
  val sIn :: sCompute :: sOut :: Nil = Enum(3)
  val state = RegInit(sIn)

  val inCnt = RegInit(0.U(32.W))
  val outCnt = RegInit(0.U(32.W))

  io.in.ready := (state === sIn)
  io.out.valid := (state === sOut)
  io.out.bits := cells(0).io.SOut

  // Systolic array of PolyCells
  for (i <- numCells - 1 to 0 by - 1) {
    cells(i).io.running := (state === sIn && io.in.fire()) || (state === sCompute)
    cells(i).io.Rx := io.in.bits
    if (numCells == 1) {
      cells(i).io.coeff := io.coeff
    }
    else {
      cells(i).io.coeff := coeffs(i).asUInt()
    }

    if (i == numCells - 1) {
      cells(i).io.SIn := 0.U
    } else {
      cells(i).io.SIn := cells(i + 1).io.SOut
    }
  }

  when (state === sIn) {
    when (io.in.fire()) {
      when (inCnt === numInputs.asUInt() - 1.U) {
        state := sCompute
        inCnt := 0.U
      }
      .otherwise {
        inCnt := inCnt + 1.U
      }
    }
  }

  // Have an extra cycle to make sure the computation finishes
  when (state === sCompute) {
    state := sOut
  }

  when (state === sOut) {
    when (io.out.fire()) {
      when (outCnt === numCells.asUInt() - 1.U) {
        state := sIn
        outCnt := 0.U
      }
      .otherwise {
        outCnt := outCnt + 1.U
      }
    }
  }
}

class GFInversion(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(p.symbolWidth.W))
    val out = Output(UInt(p.symbolWidth.W))
  })

  val inputReg = RegNext(io.in)
  val numVals = math.pow(2, p.symbolWidth).toInt

  inputReg := io.in

  val tmp = GInv(inputReg, p.symbolWidth, p.fConst.asUInt())
  val outputReg = RegNext(tmp)
  printf("TEST %d %d\n", inputReg, outputReg)
  io.out := outputReg
}

class ErrorPolyGen(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  val syndCmp = Module(new PolyCompute(p.symbolWidth, p.n - p.k, p.n,
                                       p.Log2Val, p.fConst))

  val numRoots = math.pow(2, p.symbolWidth).toInt
  val chienCmp = Module(new PolyCompute(p.symbolWidth, numRoots,
                                        p.n - p.k + 1,
                                        p.Log2Val, p.fConst))

  val sInit :: sErrorPolyGen :: sChienCmp :: sErrorMagCmp0 :: sErrorMagCmp1 :: sDone :: Nil = Enum(6)
  val state = RegInit(sInit)

  val syndCmpOutCnt = RegInit(0.U(32.W))
  val cmpCnt = RegInit(0.U(32.W))

  // Registers for Error evaluator polynomial
  val OARegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))
  val OBRegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))

  // Registers for Error locator polynomial
  val TARegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))
  val TBRegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))

  io.in.ready := (state === sInit)
  io.out.valid := (state === sDone)

  syndCmp.io.coeff := 0.U
  syndCmp.io.in <> io.in
  syndCmp.io.out.ready := (state === sInit)

  when (state === sInit) {
    OARegs(p.n - p.k) := 1.U
    TBRegs(0) := 1.U

    when (syndCmp.io.out.fire()) {
      syndCmpOutCnt := syndCmpOutCnt + 1.U
      OBRegs(p.n - p.k - 1) := syndCmp.io.out.bits
      for (i <- p.n - p.k - 1 to 1 by - 1) {
        OBRegs(i - 1) := OBRegs(i)
      }
    }

    when (syndCmpOutCnt === (p.n - p.k - 1).asUInt()) {
      state := sErrorPolyGen
    }
  }

  when (state === sErrorPolyGen) {
    when (cmpCnt === (p.n - p.k).asUInt()) {
      state := sChienCmp
    }
    .otherwise {
      cmpCnt := cmpCnt + 1.U
      for (i <- 0 to p.n - p.k) {
        val theta = OBRegs(p.n - p.k - 1)
        val gamma = OARegs(p.n - p.k)

        if (i == 0) {
          OBRegs(i) := GMul(theta, OARegs(i), p.symbolWidth, p.fConst.asUInt()) ^
                       GMul(gamma, 0.U, p.symbolWidth, p.fConst.asUInt())
          TBRegs(i) := GMul(theta, TARegs(i), p.symbolWidth, p.fConst.asUInt()) ^
                       GMul(gamma, 0.U, p.symbolWidth, p.fConst.asUInt())
        } else {
          OBRegs(i) := GMul(theta, OARegs(i), p.symbolWidth, p.fConst.asUInt()) ^
                       GMul(gamma, OBRegs(i - 1), p.symbolWidth, p.fConst.asUInt())
          TBRegs(i) := GMul(theta, TARegs(i), p.symbolWidth, p.fConst.asUInt()) ^
                       GMul(gamma, TBRegs(i - 1), p.symbolWidth, p.fConst.asUInt())
          OARegs(i) := OBRegs(i - 1)
          TARegs(i) := TBRegs(i - 1)
        }
      }
    }
  }

  chienCmp.io.coeff := 0.U
  chienCmp.io.in.valid := (state === sChienCmp)
  chienCmp.io.in.bits := Mux(state === sChienCmp, TBRegs(p.n - p.k), 0.U)
  chienCmp.io.out.ready := (state === sChienCmp)
  val chienOut = RegNext(chienCmp.io.out.bits)
  val chienCmpOutCnt = RegInit(0.U(32.W))

  // This queue stores the roots of the error location polynomial
  // formed by TBRegs
  val chienQueue = Module(new Queue(UInt(p.symbolWidth.W), 1))// p.n - p.k))

  chienQueue.io.enq.bits := chienCmpOutCnt
  chienQueue.io.enq.valid := (chienCmp.io.out.bits === 0.U) &&
                             (state === sChienCmp) &&
                             (chienCmp.io.out.fire())
  chienQueue.io.deq.ready := (state === sErrorMagCmp0)

  when (state === sChienCmp) {
    when (chienCmp.io.in.fire()) {
      for (i <- 0 until p.n - p.k) {
        TBRegs(i + 1) := TBRegs(i)
      }
    }

    when (chienCmp.io.out.fire()) {
      chienCmpOutCnt := chienCmpOutCnt + 1.U
    }

    when (chienCmpOutCnt === numRoots.asUInt() - 1.U) {
      state := sErrorMagCmp0
    }
  }

  val rootIndex = RegInit(0.U(32.W))
  val oResult = RegInit(0.U(p.symbolWidth.W))
  val coeffs = VecInit(p.Log2Val.map(_.U))

  val oCmp = Module(new PolyCompute(p.symbolWidth, 1, p.n - p.k + 1,
                                    p.Log2Val, p.fConst))
  val tDerivCmp = Module(new PolyCompute(p.symbolWidth, 1, p.n - p.k + 1,
                                         p.Log2Val, p.fConst))

  oCmp.io.coeff := rootIndex
  oCmp.io.in.valid := (state === sErrorMagCmp1)
  oCmp.io.in.bits := OBRegs(p.n - p.k)
  oCmp.io.out.ready := (state === sErrorMagCmp1)

  tDerivCmp.io.coeff := rootIndex
  tDerivCmp.io.in.valid := (state === sErrorMagCmp1)
  tDerivCmp.io.in.bits := TBRegs(p.n - p.k)
  tDerivCmp.io.out.ready := (state === sErrorMagCmp1)

  when (state === sErrorMagCmp0) {
    when (chienQueue.io.deq.fire()) {
      rootIndex := coeffs(chienQueue.io.deq.bits)
      state := sErrorMagCmp1
    }
  }

  when (state === sErrorMagCmp1) {
    when (oCmp.io.in.fire() && tDerivCmp.io.in.fire()) {
      for (i <- 0 until p.n - p.k) {
        OBRegs(i + 1) := OBRegs(i)
      }
    }

    when (oCmp.io.out.fire()) {
      oResult := oCmp.io.out.bits
      state := sErrorMagCmp0
    }
  }

//  printf("TEST state=%d chienIn=%d, chienOut=%d chienCmpOutCnt=%d, rootIndex=%d, oResult=%d\n",
//    state, TBRegs(p.n - p.k), chienOut, chienCmpOutCnt, rootIndex, oResult)

//  printf("TEST state=%d OBRegs(0)=%d, OBRegs(1)=%d, OBRegs(2)=%d, OBRegs(3)=%d, OBRegs(4)=%d, TBRegs(0)=%d, TBRegs(1)=%d, TBRegs(2)=%d, TBRegs(3)=%d, TBRegs(4)=%d\n",
//    state, OBRegs(0), OBRegs(1), OBRegs(2), OBRegs(3), OBRegs(4),
//           TBRegs(0), TBRegs(1), TBRegs(2), TBRegs(3), TBRegs(4))

  io.out.bits := 0.U
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
class ECCEncoderWrapper(val rsParams: RSParams = new RSParams(),
          val busParams: CREECBusParams = new CREECBusParams()
  ) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECWriteBus(busParams))
    val master = new CREECWriteBus(busParams)
  })

  io.master.header.bits.compressed := false.B
  io.master.header.bits.ecc := true.B
  io.master.header.bits.encrypted := false.B

//  io.slave.rdData.bits.compressed := io.master.rdData.bits.compressed
//  io.slave.rdData.bits.ecc := io.master.rdData.bits.ecc
//  io.slave.rdData.bits.encrypted := io.master.rdData.bits.encrypted

  io.master.header.bits.addr := io.slave.header.bits.addr
  io.master.header.bits.len := io.slave.header.bits.len
  io.master.header.bits.id := io.slave.header.bits.id

  io.master.data.bits.id := io.slave.data.bits.id

  val numItems = rsParams.n * rsParams.symbolWidth / busParams.dataWidth
  val sInit :: sCompute :: sDone :: Nil = Enum(3)
  val state = RegInit(sInit)

  val enc = Module(new RSEncoder(rsParams))

  val dataInReg = RegInit(0.U(busParams.dataWidth.W))
  val dataOutReg = RegInit(0.U((rsParams.n * rsParams.symbolWidth).W))

  io.slave.header.ready := state === sInit
  io.slave.data.ready :=  state === sInit
  io.master.header.valid := state === sDone
  io.master.data.valid := state === sDone
  io.master.data.bits.data := dataOutReg(busParams.dataWidth - 1, 0)

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

    when (io.slave.header.fire() && io.slave.data.fire()) {
      state := sCompute
      dataInReg := io.slave.data.bits.data
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
    when (io.master.header.fire() && io.master.data.fire()) {
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
