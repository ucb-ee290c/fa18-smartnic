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

      val result = Seq.fill(symbolWidth)(
        Wire(UInt((2 * symbolWidth - 1).W))
      ).zipWithIndex.foldRight(0.U) {
        case ((nextWire, idx), prevWire) => {
          val currentMultResult = prevWire ^ Mux(op2(idx), op1 << idx, 0.U)

          if (idx == 0) {
            nextWire := currentMultResult
          }
          else {
            nextWire := Mux(currentMultResult(idx + symbolWidth - 1),
                          currentMultResult ^ (fConst.asUInt() << (idx - 1)),
                          currentMultResult)
          }
          nextWire
        }
      }
      result
    }

    def shlByOne(a: UInt): UInt = {
      val mask = math.pow(2, symbolWidth).toInt - 1
      val aShifted = a.asTypeOf(UInt((symbolWidth + 1).W)) << 1
      val result = Mux(aShifted(symbolWidth),
        (aShifted & mask.asUInt()) ^ (fConst.asUInt() & mask.asUInt()),
        aShifted)
      result
    }

    // TODO: pipeline this operation to improve timing
    def inv(a: UInt): UInt = {
      val op = a.asTypeOf(UInt(symbolWidth.W))
      val numVals = math.pow(2, symbolWidth).toInt - 1

      val rootsFromOp = Seq.fill(numVals)(Wire(UInt(symbolWidth.W))).scan(op) {
        (prevWire, nextWire) => {
          nextWire := shlByOne(prevWire)
          nextWire
        }
      }

      val rootsFromOne = Seq.fill(numVals)(Wire(UInt(symbolWidth.W))).scan(1.U) {
        (prevWire, nextWire) => {
          nextWire := shlByOne(prevWire)
          nextWire
        }
      }

      val rootsMasked = rootsFromOp.zip(rootsFromOne).map {
        case (rootFromOp, rootFromOne) =>
          Mux(rootFromOp === 1.U, rootFromOne, 0.U)
      }

      val result = rootsMasked.reduce(_ | _)
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

  io.in.ready := inReadyReg
  io.out.valid := outValidReg

  val (inCntVal, inCntDone) = Counter(io.in.fire(), p.k)
  val (outCntVal, outCntDone) = Counter(io.out.fire(), p.n)

  when (inCntDone) {
    inReadyReg := false.B
  }
  .elsewhen (io.in.fire()) {
    outValidReg := true.B
  }

  when (outCntDone) {
    outValidReg := false.B
    inReadyReg := true.B
  }

  val Regs = RegInit(VecInit(Seq.fill(p.n - p.k)(0.U(p.symbolWidth.W))))
  val inputBitsReg = RegInit(0.U(p.symbolWidth.W))
  when (io.in.fire()) {
    inputBitsReg := io.in.bits
  }
  .otherwise {
    inputBitsReg := 0.U
  }

  // Make sure the arithmetic operations are correct (in Galois field)
  val feedback = Mux(outCntVal < p.k.asUInt(),
                     inputBitsReg ^ Regs(p.n - p.k - 1), 0.U)

  Regs.zipWithIndex.foldLeft(0.U) {
    case (prevReg, (nextReg, idx)) => {
      nextReg := prevReg ^ p.GFOp.mul(feedback, p.gCoeffs(idx).asUInt())
      nextReg
    }
  }

  io.out.bits := Mux(outCntVal < p.k.asUInt(),
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
  }
  .otherwise {
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
                  val numInputs: Int,
                  val reverse: Boolean = false)
  extends Module {
  val io = IO(new Bundle {
    val coeffs = Vec(numCells, Input(UInt(p.symbolWidth.W)))
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  def getNumCells(): Int = {
    numCells
  }

  val cells = Seq.fill(numCells)(Module(new PolyCell(p)))
  val sIn :: sCompute :: sOut :: Nil = Enum(3)
  val state = RegInit(sIn)

  val (inCntVal, inCntDone) = Counter(io.in.fire(), numInputs)
  val (outCntVal, outCntDone) = Counter(io.out.fire(), numCells)

  io.in.ready := (state === sIn)
  io.out.valid := (state === sOut)

  // Systolic array of PolyCells
  if (reverse) {
    io.out.bits := cells(numCells - 1).io.SOut
    cells.foldLeft(0.U) {
      (prev, next) => {
        next.io.SIn := prev
        next.io.SOut
      }
    }
  }
  else {
    io.out.bits := cells(0).io.SOut
    cells.foldRight(0.U) {
      (next, prev) => {
        next.io.SIn := prev
        next.io.SOut
      }
    }
  }

  cells.zip(io.coeffs) map {
    case (c, coeff) => {
      c.io.running := (state === sIn && io.in.fire()) || (state === sCompute)
      c.io.Rx := io.in.bits
      c.io.coeff := coeff
    }
  }

  switch (state) {
    is (sIn) {
      when (io.in.fire()) {
        when (inCntDone) {
          state := sCompute
        }
      }
    }

    // Have an extra cycle to make sure the computation finishes
    is (sCompute) {
      state := sOut
    }

    is (sOut) {
      when (io.out.fire()) {
        when (outCntDone) {
          state := sIn
        }
      }
    }
  }

}

// TODO:
//  - Bypass this module if the input symbol sequence does not contain
//    any error (syndromes are all zeroes)
//  - Improve performance by eliminating redundant cycles due to zeroes coefficients
//  - Support unrolling if it makes sense
//  - Improve the performance of Key Equation solver
//  - Consider moving parts of the decoder into separate modules
class RSDecoder(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  val rootVals = VecInit(p.Log2Val.map(_.U))

  val syndCmp = Module(new PolyCompute(p, p.n - p.k, p.n))
  val numRoots = math.pow(2, p.symbolWidth).toInt
  val chienSearch = Module(new PolyCompute(p, p.n, p.n - p.k + 1, true))

  val sInit :: sSyndromeCmp :: sKeyEquationSolver :: sChienSearch :: sErrorCorrection0 :: sErrorCorrection1 :: sErrorCorrection2 :: Nil = Enum(7)
  val state = RegInit(sInit)

  val (inCntVal, inCntDone) = Counter(io.in.fire(), p.n)
  val (syndCmpOutCntVal, syndCmpOutCntDone) = Counter(syndCmp.io.out.fire(),
                                                      p.n - p.k)

  // Registers for the Error evaluator polynomial's coefficients
  val evalARegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))
  val evalBRegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))

  // Registers for the Error locator polynomial's coefficients
  val locARegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))
  val locBRegs = RegInit(VecInit(Seq.fill(p.n - p.k + 1)(0.U(p.symbolWidth.W))))
  // Registers for the derivative of Error locator polynomial's coefficients
  val locDerivRegs = RegInit(VecInit(Seq.fill(p.n - p.k)(0.U(p.symbolWidth.W))))

  val theta = evalBRegs(p.n - p.k)
  val gamma = evalARegs(p.n - p.k)

  val degA = RegInit((p.n - p.k).asUInt())
  val degB = RegInit(0.U)

  io.in.ready := (state === sInit)

  // Buffer input data for later correction
  // Note: only need to buffer up to p.k symbols -- which is the length
  // of the original code sequence
  val inputQueue = Module(new Queue(UInt(p.symbolWidth.W), p.k))
  inputQueue.io.enq.bits := io.in.bits
  inputQueue.io.enq.valid := io.in.fire() && (inCntVal < p.k.asUInt())

  syndCmp.io.coeffs := (0 until syndCmp.getNumCells()).map(x => rootVals(x))
  syndCmp.io.in.bits := io.in.bits
  syndCmp.io.in.valid := io.in.fire()
  syndCmp.io.out.ready := (state === sSyndromeCmp)

  chienSearch.io.coeffs := (0 until chienSearch.getNumCells()).map(
                           x => rootVals(numRoots - 1 - x))
  chienSearch.io.in.valid := (state === sChienSearch)
  chienSearch.io.in.bits := Mux(state === sChienSearch, locARegs(p.n - p.k), 0.U)
  chienSearch.io.out.ready := (state === sChienSearch)
  val chienOut = RegNext(chienSearch.io.out.bits)
  val (chienSOutCntVal, chienSOutCntDone) = Counter(chienSearch.io.out.fire(),
                                                    chienSearch.getNumCells())

  // This queue stores the roots of the error location polynomial
  // formed by locARegs
  val chienQueue = Module(new Queue(UInt(p.symbolWidth.W), p.n - p.k))
  chienQueue.io.enq.bits := chienSOutCntVal + 1.U
  // Don't bother fixing parity symbols (up to p.k -- which is the length
  // of the original messages is sufficient.). It does not make sense
  // to carry parity symbols forward at this point
  chienQueue.io.enq.valid := (chienSearch.io.out.bits === 0.U) &&
                             (state === sChienSearch) &&
                             (chienSOutCntVal + 1.U <= p.k.asUInt()) &&
                             (chienSearch.io.out.fire())

  val evalPolyCmp = Module(new PolyCompute(p, 1, p.n - p.k + 1))
  val locDerivCmp = Module(new PolyCompute(p, 1, p.n - p.k))

  val startEvalPolyCmp = RegInit(false.B)
  val startLocDerivCmp = RegInit(false.B)

  val locRootIdx = RegInit(0.U(32.W))
  val evalResult = RegInit(0.U(p.symbolWidth.W))
  val locDerivResult = RegInit(0.U(p.symbolWidth.W))
  val evalResultFired = RegInit(false.B)
  val locDerivResultFired = RegInit(false.B)
  val (evalInCntVal, evalInCntDone) = Counter(evalPolyCmp.io.in.fire(),
                                              p.n - p.k + 1)
  val (locDerivInCntVal, locDerivInCntDone) = Counter(locDerivCmp.io.in.fire(),
                                                      p.n - p.k)

  val rootValIdx = numRoots.asUInt() - 1.U - (p.n.asUInt() - locRootIdx)
  val denom = p.GFOp.mul(locDerivResult, rootVals(rootValIdx))
  val errorMagReg = RegNext(p.GFOp.mul(evalResult, p.GFOp.inv(denom)))

  evalPolyCmp.io.coeffs := (0 until evalPolyCmp.getNumCells()).map(
                           x => rootVals(rootValIdx))
  evalPolyCmp.io.in.valid := startEvalPolyCmp
  evalPolyCmp.io.in.bits := evalARegs(p.n - p.k)
  evalPolyCmp.io.out.ready := state === sErrorCorrection2

  locDerivCmp.io.coeffs := (0 until locDerivCmp.getNumCells()).map(
                           x => rootVals(rootValIdx))
  locDerivCmp.io.in.valid := startLocDerivCmp
  locDerivCmp.io.in.bits := locDerivRegs(p.n - p.k - 1)
  locDerivCmp.io.out.ready := state === sErrorCorrection2

  val correctCnd = (state === sErrorCorrection0) &&
                   (evalResultFired && locDerivResultFired)
  val (outCntVal, outCntDone) = Counter(io.out.fire(), p.k)

  inputQueue.io.deq.ready := correctCnd || (state === sErrorCorrection1 &&
                                             outCntVal =/= locRootIdx - 1.U)
  io.out.valid := inputQueue.io.deq.fire()
  io.out.bits := inputQueue.io.deq.bits ^ errorMagReg

  chienQueue.io.deq.ready := (state === sErrorCorrection0)

  switch (state) {
    is (sInit) {
      evalARegs.foreach (_ := 0.U)
      evalBRegs.foreach (_ := 0.U)
      locARegs.foreach (_ := 0.U)
      locBRegs.foreach (_ := 0.U)
      locDerivRegs.foreach (_ := 0.U)

      evalARegs(p.n - p.k) := 1.U
      locBRegs(0) := 1.U

      degA := (p.n - p.k).asUInt()
      degB := 0.U

      locRootIdx := 0.U
      evalResult := 0.U
      locDerivResult := 0.U
      evalResultFired := false.B
      locDerivResultFired := false.B
      errorMagReg := 0.U
      when (inCntDone) {
        state := sSyndromeCmp
      }
    }

    is (sSyndromeCmp) {
      when (syndCmp.io.out.fire()) {
        when (syndCmp.io.out.bits =/= 0.U) {
          degB := syndCmpOutCntVal
        }

        evalBRegs.dropRight(1).foldRight(syndCmp.io.out.bits) {
          case (nextReg, prevReg) => {
            nextReg := prevReg
            nextReg
          }
        }
      }

      when (syndCmpOutCntDone) {
        state := sKeyEquationSolver
      }
    }

    is (sKeyEquationSolver) {
      when (degA < ((p.n - p.k) / 2).asUInt()) {
        state := sChienSearch
        // odd-th coefficients are zeros due to finite-field
        locDerivRegs.zipWithIndex.filter(_._2 % 2 == 1).map { x => 0.U }
        // even-th coefficients are obtained by the odd-th coefficients
        // of the original polynomial
        locDerivRegs.zip(locARegs.drop(1)).
          zipWithIndex.filter(_._2 % 2 == 0).map { x => x._1._1 := x._1._2 }
      }
      .otherwise {

        when (degA < degB && theta =/= 0.U && gamma =/= 0.U) {
          evalARegs.zip(evalBRegs).map {
            case (aReg, bReg) => {
              aReg := bReg
              bReg := aReg
            }
          }

          locARegs.zip(locBRegs).map {
            case (aReg, bReg) => {
              aReg := bReg
              bReg := aReg
            }
          }

          degA := degB
          degB := degA
        }
        .otherwise {
          when (theta =/= 0.U && gamma =/= 0.U) {
            evalARegs.zip(evalBRegs).map {
              case (aReg, bReg) => {
                aReg := p.GFOp.mul(theta, aReg) ^ p.GFOp.mul(gamma, bReg)
              }
            }

            locARegs.zip(locBRegs).map {
              case (aReg, bReg) => {
                aReg := p.GFOp.mul(theta, aReg) ^ p.GFOp.mul(gamma, bReg)
              }
            }
          }
        }

        when (theta === 0.U) {
          evalBRegs.foldLeft(0.U) {
            case (prev, next) => {
              next := prev
              next
            }
          }
          locBRegs.foldLeft(0.U) {
            case (prev, next) => {
              next := prev
              next
            }
          }
        }

        when (gamma === 0.U) {
          degA := degA - 1.U
          when (degA - 1.U >= ((p.n - p.k) / 2).asUInt()) {
            evalARegs.foldLeft(0.U) {
              case (prev, next) => {
                next := prev
                next
              }
            }
            locARegs.foldLeft(0.U) {
              case (prev, next) => {
                next := prev
                next
              }
            }
          }
        }
      }
    }

    is (sChienSearch) {
      when (chienSearch.io.in.fire()) {
        val lastVal = locARegs(p.n - p.k)
        locARegs.foldLeft(lastVal) {
          (prevReg, nextReg) => {
            nextReg := prevReg
            nextReg
          }
        }
      }

      when (chienSOutCntDone) {
        state := sErrorCorrection0
      }
    }

    is (sErrorCorrection0) {
      evalResultFired := false.B
      locDerivResultFired := false.B
      errorMagReg := 0.U

      when (chienQueue.io.deq.fire()) {
        locRootIdx := chienQueue.io.deq.bits
      }

      when (outCntDone) {
        state := sInit
      }
      .otherwise {
        state := sErrorCorrection1
      }
    }

    is (sErrorCorrection1) {
      errorMagReg := 0.U

      when (outCntDone) {
        state := sInit
      }
      .elsewhen (outCntVal === (locRootIdx - 1.U)) {
        state := sErrorCorrection2
        startEvalPolyCmp := true.B
        startLocDerivCmp := true.B
      }
    }

    is (sErrorCorrection2) {
      when (evalPolyCmp.io.in.fire()) {
        when (evalInCntDone) {
          startEvalPolyCmp := false.B
        }

        val lastVal = evalARegs(p.n - p.k)
        evalARegs.foldLeft(lastVal) {
          (prevReg, nextReg) => {
            nextReg := prevReg
            nextReg
          }
        }
      }

      when (locDerivCmp.io.in.fire()) {
        when (locDerivInCntDone) {
          startLocDerivCmp := false.B
        }

        val lastVal = locDerivRegs(p.n - p.k - 1)
        locDerivRegs.foldLeft(lastVal) {
          (prevReg, nextReg) => {
            nextReg := prevReg
            nextReg
          }
        }
      }

      when (evalPolyCmp.io.out.fire()) {
        evalResult := evalPolyCmp.io.out.bits
        evalResultFired := true.B
      }

      when (locDerivCmp.io.out.fire()) {
        locDerivResult := locDerivCmp.io.out.bits
        locDerivResultFired := true.B
      }

      when (evalResultFired && locDerivResultFired) {
        state := sErrorCorrection0
      }
    }

  }
}

// CREECBus integration with the RSEncoder module
// What to do:
// *** Encoder
//   + Receive write header from upstream block (slave)
//   + Send write header to downstream block (master)
//   + Receive write data from upstream block (slave)
//   + Send *encoded* write data to downstream block (master)
class ECCEncoderTop(val rsParams: RSParams = new RSParams(),
                    val busParams: CREECBusParams = new CREECBusParams()
  ) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(busParams))
    val master = new CREECBus(busParams)
  })

  val numItems = rsParams.n * rsParams.symbolWidth / busParams.dataWidth
  val sRecvHeader :: sRecvData :: sCompute :: sDone :: Nil = Enum(4)
  val state = RegInit(sRecvHeader)

  // TODO: handle the metadata appropriately
  io.master.header.bits.compressed := false.B
  io.master.header.bits.ecc := true.B
  io.master.header.bits.encrypted := false.B

  io.master.header.bits.addr := RegNext(io.slave.header.bits.addr)
  // Modify the beat number based on encoding specification
  io.master.header.bits.len := RegNext((io.slave.header.bits.len + 1.U) *
                                       numItems.asUInt() - 1.U)
  io.master.header.bits.id := RegNext(io.slave.header.bits.id)
  io.master.header.valid := RegNext(io.slave.header.valid)

  io.master.data.bits.id := RegNext(io.slave.data.bits.id)

  val enc = Module(new RSEncoder(rsParams))

  val dataInReg = RegInit(0.U(busParams.dataWidth.W))
  val dataOutReg = RegInit(0.U((rsParams.n * rsParams.symbolWidth).W))

  io.slave.header.ready := state === sRecvHeader

  io.slave.data.ready := state === sRecvData
  io.master.data.valid := state === sDone
  io.master.data.bits.data := dataOutReg(busParams.dataWidth - 1, 0)

  enc.io.in.valid := state === sCompute
  enc.io.in.bits := dataInReg
  enc.io.out.ready := state === sCompute

  val numBeats = RegInit(0.U(32.W))

  val (encInCntVal, encInCntDone) = Counter(enc.io.in.fire(), rsParams.k)
  val (encOutCntVal, encOutCntDone) = Counter(enc.io.out.fire(), rsParams.n)
  val (itemCntVal, itemCntDone) = Counter(io.master.data.fire(), numItems)
  // Cannot use Counter for this because the maximum value is not statically known
  val beatCnt = RegInit(0.U(32.W))

  switch (state) {
    is (sRecvHeader) {
      beatCnt := 0.U
      dataInReg := 0.U
      dataOutReg := 0.U

      when (io.slave.header.fire()) {
        state := sRecvData
        numBeats := io.slave.header.bits.len
      }
    }

    is (sRecvData) {
      when (io.slave.data.fire()) {
        state := sCompute
        dataInReg := io.slave.data.bits.data
        beatCnt := beatCnt + 1.U
      }
    }

    is (sCompute) {
      when (enc.io.in.fire()) {
        dataInReg := (dataInReg >> rsParams.symbolWidth)
      }

      when (enc.io.out.fire()) {
        when (encOutCntDone) {
          state := sDone
        }
        dataOutReg := (dataOutReg << rsParams.symbolWidth) + enc.io.out.bits
      }
    }

    is (sDone) {
      when (io.master.data.fire()) {
        dataOutReg := (dataOutReg >> busParams.dataWidth)
        // May take multiple cycles to send the output data
        // if it is larger than the bus data width
        when (itemCntDone) {
          when (beatCnt === numBeats - 1.U) {
            // If all data beats have been processed, receive the next header
            state := sRecvHeader
          }
          .otherwise {
            // Process the next data beat
            state := sRecvData
          }
        }
      }
    }

  }
}

// CREECBus integration with the RSDecoder module
// What to do:
// *** Decoder
//   + Receive read header from downstream block (slave)
//   + Send read header to upstream block (master)
//   + Receive read data from downstream block (slave)
//   + Send *decoded* read data to upstream block (master)
class ECCDecoderTop(val rsParams: RSParams = new RSParams(),
                    val busParams: CREECBusParams = new CREECBusParams()
  ) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(busParams))
    val master = new CREECBus(busParams)
  })

  // TODO: handle the metadata appropriately
  io.master.header.bits.compressed := false.B
  io.master.header.bits.ecc := true.B
  io.master.header.bits.encrypted := false.B

  io.master.header.bits.addr := RegNext(io.slave.header.bits.addr)
  io.master.header.bits.len := RegNext(io.slave.header.bits.len)
  io.master.header.bits.id := RegNext(io.slave.header.bits.id)

  io.master.data.bits.id := RegNext(io.slave.data.bits.id)

  val numItems = rsParams.n * rsParams.symbolWidth / busParams.dataWidth
  val sRecvHeader :: sRecvData :: sCompute :: sDone :: Nil = Enum(4)
  val state = RegInit(sRecvHeader)

  val dec = Module(new RSDecoder(rsParams))

  val dataInReg = RegInit(0.U((rsParams.n * rsParams.symbolWidth).W))
  val dataOutReg = RegInit(0.U(busParams.dataWidth.W))

  io.slave.header.ready := state === sRecvHeader
  io.master.header.valid := state === sRecvData

  io.slave.data.ready := state === sRecvData
  io.master.data.valid := state === sDone
  io.master.data.bits.data := dataOutReg

  val numBeats = RegInit(0.U(32.W))

  val (decInCntVal, decInCntDone) = Counter(dec.io.in.fire(), rsParams.n)
  val (decOutCntVal, decOutCntDone) = Counter(dec.io.out.fire(), rsParams.k)
  val (itemCntVal, itemCntDone) = Counter(io.slave.data.fire(), numItems)
  // Cannot use Counter for this because the maximum value is not statically known
  val beatCnt = RegInit(0.U(32.W))

  dec.io.in.valid := state === sCompute
  dec.io.in.bits := dataInReg(rsParams.symbolWidth - 1, 0)
  dec.io.out.ready := state === sCompute

  switch (state) {
    is (sRecvHeader) {
      beatCnt := 0.U
      dataInReg := 0.U
      dataOutReg := 0.U

      when (io.slave.header.fire()) {
        state := sRecvData
        numBeats := io.slave.header.bits.len
      }
    }

    is (sRecvData) {
      when (io.slave.data.fire()) {
        beatCnt := beatCnt + 1.U
        dataInReg := (dataInReg << busParams.dataWidth) + io.slave.data.bits.data
        // May take multiple cycles to receive input data
        // if it is larger than the bus data width
        when (itemCntDone) {
          state := sCompute
        }
      }
    }

    is (sCompute) {
      when (dec.io.in.fire()) {
        dataInReg := (dataInReg >> rsParams.symbolWidth)
      }

      when (dec.io.out.fire()) {
        when (decOutCntDone) {
          state := sDone
        }
        dataOutReg := (dataOutReg << rsParams.symbolWidth) + dec.io.out.bits
      }
    }

    is (sDone) {
      when (io.master.data.fire()) {
        when (beatCnt === numBeats) {
          // If all data beats have been processed, receive the next header
          state := sRecvHeader
        }
        .otherwise {
          // Process the next data beat
          state := sRecvData
        }
      }
    }

  }
}

class ECCEncoderTopModel(val rsParams: RSParams = new RSParams(),
                         val busParams: CREECBusParams = new CREECBusParams()
  ) extends SoftwareModel[CREECLowLevelTransaction, CREECLowLevelTransaction] {
  override def process(in: CREECLowLevelTransaction): Seq[CREECLowLevelTransaction] = {
    val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)
    val numItems = rsParams.n * rsParams.symbolWidth / busParams.dataWidth

    in match {
      case t: CREECHeaderBeat =>
        val newLen = (t.len + 1) * numItems - 1
        Seq(CREECHeaderBeat(newLen, t.id, t.addr)(busParams))
      case t: CREECDataBeat =>
        // Convert signed to unsigned.
        // FIXME: Is there a better way?
        val inputMsgs = t.data.map(x => {
          val e = if (x.toInt < 0) { x.toInt + 256 } else { x.toInt }
            e
        })

        // FIXME: why is 'reverse' needed here?
        val encodedMsgs = rs.encode(inputMsgs).map(_.toByte).reverse
        val encodedMsgsGrp = encodedMsgs.grouped(busParams.bytesPerBeat).toSeq
        encodedMsgsGrp.map (x => CREECDataBeat(x, 0)(busParams)).toSeq
    }
  }
}

class ECCDecoderTopModel(val rsParams: RSParams = new RSParams(),
                         val busParams: CREECBusParams = new CREECBusParams()
  ) extends SoftwareModel[CREECHighLevelTransaction, CREECHighLevelTransaction] {
  override def process(in: CREECHighLevelTransaction): Seq[CREECHighLevelTransaction] = {
    val rs = new RSCode(rsParams.n, rsParams.k, rsParams.symbolWidth)
    val inputMsgs = in.data.map(_.toInt)
    val decodedMsgs = rs.decode(inputMsgs).map(_.toByte)
    Seq(CREECHighLevelTransaction(decodedMsgs, in.addr))
  }
}
