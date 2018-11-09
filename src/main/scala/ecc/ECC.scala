// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._
import interconnect.{CREECBusParams, CREECWriteBus}

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
  // Sample configuration of RS(7, 3) with 3-bit symbol
  val n: Int = 7,
  val k: Int = 3,
  val symbolWidth: Int = 3,
  val gCoeffs: Seq[Int] = Seq(3, 2, 1, 3),
  val fConst: Int = 11,
  val Log2Val: Seq[Int] = Seq(1, 2, 4, 3, 6, 7, 5, 1),
  val Val2Log: Seq[Int] = Seq(0, 7, 1, 3, 2, 6, 4, 5)
) {

  // GF Arithmetic operations
  object GFOp {
    def mul(a: UInt, b: UInt): UInt = {
      val op1 = a.asTypeOf(UInt(symbolWidth.W))
      val op2 = b.asTypeOf(UInt(symbolWidth.W))
      val tmp = Wire(Vec(symbolWidth, UInt((2 * symbolWidth - 1).W)))
      for (i <- symbolWidth - 1 to 0 by - 1) {
        val tmp0 = if (i == symbolWidth - 1) {
                     Mux(op2(i), op1 << i, 0.U)
                   } else {
                     tmp(i + 1) ^ Mux(op2(i), op1 << i, 0.U)
                   }

        val tmp1 = if (i == 0) {
                     tmp0
                   } else {
                     Mux(tmp0(i + symbolWidth - 1),
                         tmp0 ^ (fConst.asUInt() << (i - 1)), tmp0)
                   }
        tmp(i) := tmp1
      }
      tmp(0)
    }

    def shlByOne(a: UInt): UInt = {
      val mask = math.pow(2, symbolWidth).toInt - 1
      val tmp = Wire(UInt((symbolWidth + 1).W))
      tmp := a << 1
      val result = Mux(tmp(symbolWidth),
        (tmp & mask.asUInt()) ^ (fConst.asUInt() & mask.asUInt()), tmp)
      result
    }

    def inv(a: UInt): UInt = {
      val op = a.asTypeOf(UInt(symbolWidth.W))
      val numVals = math.pow(2, symbolWidth).toInt

      val rootsFromOp = Seq.fill(numVals)(Wire(UInt(symbolWidth.W))).scan(op)(
        (prev: UInt, next: UInt) => {
        next := shlByOne(prev)
        next
      })

      val rootsFromOne = Seq.fill(numVals)(Wire(UInt(symbolWidth.W))).scan(1.U)(
        (prev: UInt, next: UInt) => {
        next := shlByOne(prev)
        next
      })

      val rootChecks = rootsFromOp.zip(rootsFromOne).map{
        case(rootFromOp: UInt, rootFromOne: UInt) =>
          Mux(rootFromOp === 1.U, rootFromOne, 0.U) }

      val result = rootChecks.reduce(_ ^ _)
      result
    }
  }
}

// This module will accept k symbols (io.in.fire() === true.B until received k symbols)
// It will emit n symbols (io.out.fire() === true.B until sent n symbols)
// Each symbol has a width of *symbolWidth*
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
      Regs(0) := p.GFOp.mul(feedback, p.gCoeffs(0).asUInt())
    } else {
      Regs(i) := Regs(i - 1) ^ p.GFOp.mul(feedback, p.gCoeffs(i).asUInt())
    }
  }

  io.out.bits := Mux(outputSymbolCnt < p.k.asUInt(),
                     inputBitsReg, Regs(p.n - p.k - 1))
}

class PolyCell(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val running = Input(Bool())
    val SIn = Input(UInt(p.symbolWidth.W))
    val coeff = Input(UInt(p.symbolWidth.W))
    val Rx = Input(UInt(p.symbolWidth.W))

    val SOut = Output(UInt(p.symbolWidth.W))
  })

  val Reg0 = RegInit(0.U(p.symbolWidth.W))
  val Reg1 = RegInit(0.U(p.symbolWidth.W))
  val Reg2 = RegInit(0.U(p.symbolWidth.W))

  when (io.running) {
    Reg0 := io.Rx
    Reg1 := p.GFOp.mul((Reg0 ^ Reg1), io.coeff)
  } .otherwise {
    Reg0 := 0.U
    Reg1 := 0.U
  }

  Reg2 := Mux(io.running, (Reg0 ^ Reg1), io.SIn)

  io.SOut := Reg2
}

// This class computes one or many polynomials in a systolic-array fashion.
// The number of polynomials is configured via the parameter *numCells*.
// Each cell has an assigned root value.
// The coefficients of each polynomial are supplied by the input signal
// in successive clock cycles.
// This implementation is based on Horner's method
class PolyCompute(val p: RSParams = new RSParams(),
                  val numCells: Int,
                  val numInputs: Int)
  extends Module {
  val io = IO(new Bundle {
    val coeff = Input(UInt(p.symbolWidth.W))
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  val cells = Seq.tabulate(numCells)(i => Module(new PolyCell(p)))
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
      cells(i).io.coeff := p.Log2Val(i).asUInt()
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

class RSDecoder(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  val syndCmp = Module(new PolyCompute(p, p.n - p.k, p.n))
  val numRoots = math.pow(2, p.symbolWidth).toInt
  val chienSearch = Module(new PolyCompute(p, numRoots, p.n - p.k + 1))

  val sInit :: sKeyEquationSolver :: sChienSearch :: sErrorCorrection0 :: sErrorCorrection1 :: sErrorCorrection2 :: Nil = Enum(6)
  val state = RegInit(sInit)

  val syndCmpOutCnt = RegInit(0.U(32.W))
  val cmpCnt = RegInit(0.U(32.W))

  // Registers for Error evaluator polynomial
  val OARegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))
  val OBRegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))

  // Registers for Error locator polynomial
  val TARegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))
  val TBRegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))
  val TBDerivRegs = RegInit(VecInit(Seq.fill(p.n - p.k)(0.U(p.symbolWidth.W))))

  // Buffer input data for later correction
  val inputQueue = Module(new Queue(UInt(p.symbolWidth.W), p.n))
  inputQueue.io.enq <> io.in

  io.in.ready := (state === sInit)

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
      state := sKeyEquationSolver
    }
  }

  when (state === sKeyEquationSolver) {
    when (cmpCnt === (p.n - p.k).asUInt()) {
      state := sChienSearch
      for (i <- 0 until p.n - p.k) {
        if (i % 2 == 1) {
          TBDerivRegs(i) := 0.U
        }
        else {
          TBDerivRegs(i) := TBRegs(i + 1)
        }
      }
    }
    .otherwise {
      cmpCnt := cmpCnt + 1.U
      for (i <- 0 to p.n - p.k) {
        val theta = OBRegs(p.n - p.k - 1)
        val gamma = OARegs(p.n - p.k)

        if (i == 0) {
          OBRegs(i) := p.GFOp.mul(theta, OARegs(i)) ^ p.GFOp.mul(gamma, 0.U)
          TBRegs(i) := p.GFOp.mul(theta, TARegs(i)) ^ p.GFOp.mul(gamma, 0.U)
        } else {
          OBRegs(i) := p.GFOp.mul(theta, OARegs(i)) ^ p.GFOp.mul(gamma, OBRegs(i - 1))
          TBRegs(i) := p.GFOp.mul(theta, TARegs(i)) ^ p.GFOp.mul(gamma, TBRegs(i - 1))
          OARegs(i) := OBRegs(i - 1)
          TARegs(i) := TBRegs(i - 1)
        }
      }
    }
  }

  chienSearch.io.coeff := 0.U
  chienSearch.io.in.valid := (state === sChienSearch)
  chienSearch.io.in.bits := Mux(state === sChienSearch, TBRegs(p.n - p.k), 0.U)
  chienSearch.io.out.ready := (state === sChienSearch)
  val chienOut = RegNext(chienSearch.io.out.bits)
  val chienSearchOutCnt = RegInit(0.U(32.W))

  // This queue stores the roots of the error location polynomial
  // formed by TBRegs
  val chienQueue = Module(new Queue(UInt(p.symbolWidth.W), p.n - p.k))
  chienQueue.io.enq.bits := chienSearchOutCnt
  chienQueue.io.enq.valid := (chienSearch.io.out.bits === 0.U) &&
                             (state === sChienSearch) &&
                             (chienSearch.io.out.fire())

  when (state === sChienSearch) {
    when (chienSearch.io.in.fire()) {
      for (i <- 0 until p.n - p.k) {
        TBRegs(i + 1) := TBRegs(i)
        if (i == 0) {
          TBRegs(0) := TBRegs(p.n - p.k)
        }
      }
    }

    when (chienSearch.io.out.fire()) {
      chienSearchOutCnt := chienSearchOutCnt + 1.U
    }

    when (chienSearchOutCnt === numRoots.asUInt() - 1.U) {
      state := sErrorCorrection0
    }
  }

  val rootIndex = RegInit(0.U(32.W))
  val oResult = RegInit(0.U(p.symbolWidth.W))
  val tDerivResult = RegInit(0.U(p.symbolWidth.W))
  val oResultFired = RegInit(false.B)
  val tDerivResultFired = RegInit(false.B)
  val oInCnt = RegInit(0.U(32.W))
  val tDerivInCnt = RegInit(0.U(32.W))

  val coeffs = VecInit(p.Log2Val.map(_.U))
  val oCmp = Module(new PolyCompute(p, 1, p.n - p.k + 1))
  val tDerivCmp = Module(new PolyCompute(p, 1, p.n - p.k))

  val errorMagReg = RegNext(p.GFOp.mul(oResult, p.GFOp.inv(p.GFOp.mul(tDerivResult, coeffs(rootIndex)))))

  oCmp.io.coeff := coeffs(rootIndex)
  oCmp.io.in.valid := (state === sErrorCorrection2 &&
                       oInCnt <= (p.n - p.k).asUInt())
  oCmp.io.in.bits := OBRegs(p.n - p.k)
  oCmp.io.out.ready := (state === sErrorCorrection2)

  tDerivCmp.io.coeff := coeffs(rootIndex)
  tDerivCmp.io.in.valid := (state === sErrorCorrection2 &&
                            tDerivInCnt <= (p.n - p.k - 1).asUInt())
  tDerivCmp.io.in.bits := TBDerivRegs(p.n - p.k - 1)
  tDerivCmp.io.out.ready := (state === sErrorCorrection2)

  val outCnt = RegInit(0.U(32.W))
  val outValidReg = RegInit(false.B)
  val outBitsReg = RegInit(0.U)
  val correctCnd = (state === sErrorCorrection0) &&
                   (oResultFired && tDerivResultFired)
  inputQueue.io.deq.ready := state === sErrorCorrection1
  io.out.valid := correctCnd || outValidReg
  io.out.bits := outBitsReg ^ errorMagReg

  chienQueue.io.deq.ready := (state === sErrorCorrection0)

  when (state === sErrorCorrection0) {
    oResultFired := false.B
    tDerivResultFired := false.B
    oInCnt := 0.U
    tDerivInCnt := 0.U

    when (chienQueue.io.deq.fire()) {
      rootIndex := chienQueue.io.deq.bits
    }
    state := sErrorCorrection1
  }

  when (state === sErrorCorrection1) {
    errorMagReg := 0.U
    when (outCnt === p.n.asUInt()) {
      outCnt := 0.U
      state := sInit
      outValidReg := false.B
    }
    .elsewhen (outCnt === rootIndex - 1.U) {
      state := sErrorCorrection2
      outCnt := outCnt + 1.U
      outValidReg := false.B
    }
    .otherwise {
      outCnt := outCnt + 1.U
      outValidReg := true.B
    }
    outBitsReg := inputQueue.io.deq.bits
  }

  when (state === sErrorCorrection2) {
    when (oCmp.io.in.fire()) {
      oInCnt := oInCnt + 1.U
      for (i <- 0 until p.n - p.k) {
        OBRegs(i + 1) := OBRegs(i)
      }
      OBRegs(0) := OBRegs(p.n - p.k)
    }

    when (tDerivCmp.io.in.fire()) {
      tDerivInCnt := tDerivInCnt + 1.U
      for (i <- 0 until p.n - p.k - 1) {
        TBDerivRegs(i + 1) := TBDerivRegs(i)
      }
      TBDerivRegs(0) := TBDerivRegs(p.n - p.k - 1)
    }

    when (oCmp.io.out.fire()) {
      oResult := oCmp.io.out.bits
      oResultFired := true.B
    }

    when (tDerivCmp.io.out.fire()) {
      tDerivResult := tDerivCmp.io.out.bits
      tDerivResultFired := true.B
    }

    when (oResultFired && tDerivResultFired) {
      state := sErrorCorrection0
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
