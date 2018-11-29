// See README.md for license details.

package ecc

import chisel3._
import chisel3.util._
import interconnect._

// This module evaluates a polynomial at *eVal*
// The coefficients are supplied via SIn in successive clock cycles
class PolyCell(val p: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val running = Input(Bool())
    val SIn = Input(UInt(p.symbolWidth.W))
    val eVal = Input(UInt(p.symbolWidth.W))
    val Rx = Input(UInt(p.symbolWidth.W))

    val SOut = Output(UInt(p.symbolWidth.W))
  })

  // Reg0 is for registering the input coefficient
  val Reg0 = RegInit(0.U(p.symbolWidth.W))

  // Reg1 is for accumulating the result
  val Reg1 = RegInit(0.U(p.symbolWidth.W))

  // Reg 2 is for output
  val Reg2 = RegInit(0.U(p.symbolWidth.W))

  when (io.running) {
    Reg0 := io.Rx
    Reg1 := p.GFOp.mul((Reg0 ^ Reg1), io.eVal)
  }
  .otherwise {
    // The registers should not be updated when the cell is not running
    Reg0 := 0.U
    Reg1 := 0.U
  }

  // If not runnning, the cell just forwards the input to the output
  Reg2 := Mux(io.running, (Reg0 ^ Reg1), io.SIn)

  io.SOut := Reg2
}

// This module evaluates a polynomial at multiple evaluation values
// in a systolic-array fashion.
// The number of polynomials is configured via the parameter *numCells*.
// Each cell has an assigned *eVal* for evaluation
// The coefficients of each polynomial are supplied by the input signal
// in successive clock cycles.
// This implementation is based on Horner's method
class PolyCompute(val p: RSParams = new RSParams(),
                  val numCells: Int,
                  val numInputs: Int,
                  val reverse: Boolean = false)
  extends Module {
  val io = IO(new Bundle {
    val eVals = Vec(numCells, Input(UInt(p.symbolWidth.W)))
    val in = Flipped(new DecoupledIO(UInt(p.symbolWidth.W)))
    val out = new DecoupledIO(UInt(p.symbolWidth.W))
  })

  // Helper function to improve readability
  def getNumCells(): Int = {
    numCells
  }

  val cells = Seq.fill(numCells)(Module(new PolyCell(p)))

  // There are 3 states:
  //   - sIn: stream the inputs to the cells
  //   - sCompute: simply a buffer state to complete the computation
  //   - sOut: stream the outputs out of the cells
  val sIn :: sCompute :: sOut :: Nil = Enum(3)
  val state = RegInit(sIn)

  // Counters to keep track of the progress
  val (inCntVal, inCntDone) = Counter(io.in.fire(), numInputs)
  val (outCntVal, outCntDone) = Counter(io.out.fire(), numCells)

  io.in.ready := (state === sIn)
  io.out.valid := (state === sOut)

  // 1D Systolic array of PolyCells
  // The cells are connected in a chain
  // We can specify the direction of the output flow with *reverse*
  if (reverse) {
    // The first output is from the last cell
    io.out.bits := cells(numCells - 1).io.SOut
    cells.foldLeft(0.U) {
      (prev, next) => {
        next.io.SIn := prev
        next.io.SOut
      }
    }
  }
  else {
    // The first output is from the first cell
    io.out.bits := cells(0).io.SOut
    cells.foldRight(0.U) {
      (next, prev) => {
        next.io.SIn := prev
        next.io.SOut
      }
    }
  }

  cells.zip(io.eVals) map {
    case (c, eVal) => {
      c.io.running := (state === sIn && io.in.fire()) || (state === sCompute)
      c.io.Rx := io.in.bits
      c.io.eVal := eVal
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

// RSDecoder accepts n symbols
// It produces k *corrected* symbols (dropping (n - k) parity symbols)
// Each symbol has a width of *symbolWidth*
// RSDecoder has the following computational blocks:
//   (1) Syndrome Computation
//   (2) Key Equation Solver
//   (3) Chien Search
//   (4) Error Correction
// (1), (3), and (4) requires evaluating some polynomials, hence the PolyCompute
// module is used. For (2), we implement the Modified Euclidean Algorithm to find
// the error location and error evaluation values. The design tries to avoid
// using GF division/inversion as much as possible, but instead cross
// multiplication. We end up with a single inversion used in (4)
// TODO:
//  - Bypass this module if the input symbol sequence does not contain
//    any error (syndromes are all zeroes)
//  - Improve performance by eliminating redundant cycles due to zeroes coefficients
//  - Support unrolling if it makes sense
//  - Improve the performance of Key Equation solver
//  - Consider moving parts of the decoder into separate modules
class RSDecoder(val rsParams: RSParams = new RSParams()) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new DecoupledIO(UInt(rsParams.symbolWidth.W)))
    val out = new DecoupledIO(UInt(rsParams.symbolWidth.W))
  })

  // a^0, a^1, a^2, ..., a^(2**symbolWidth - 1)
  val rootVals = VecInit(rsParams.Log2Val.map(_.U))
  val numRoots = math.pow(2, rsParams.symbolWidth).toInt

  // This PolyCompute is for syndrome computation
  // It has n - k cells with n inputs
  val syndCmp = Module(new PolyCompute(rsParams, rsParams.n - rsParams.k,
                                       rsParams.n))

  // This PolyCompute is for Chien Search
  // It has n cells with n - k + 1 inputs
  // Note the reverse here -- because we want to read the result
  // from the high index to the low index symbol
  val chienSearch = Module(new PolyCompute(rsParams, rsParams.n,
                                           rsParams.n - rsParams.k + 1, true))

  // There are 7 states
  //   - sInit: for reading input data
  //   - sSyndromeCmp: for computing n syndromes
  //   - sKeyEquationSolver: for solving the key equation using M.E.A
  //   - sChienSearch: for the Chien search process
  //   - sErrorCorrection{0-2}: for correcting the output symbols
  val sInit :: sSyndromeCmp :: sKeyEquationSolver :: sChienSearch :: sErrorCorrection0 :: sErrorCorrection1 :: sErrorCorrection2 :: Nil = Enum(7)
  val state = RegInit(sInit)

  val (inCntVal, inCntDone) = Counter(io.in.fire(), rsParams.n)
  val (syndCmpOutCntVal, syndCmpOutCntDone) = Counter(syndCmp.io.out.fire(),
                                                      rsParams.n - rsParams.k)

  // Registers for the Error evaluator polynomial's coefficients
  val evalARegs = RegInit(VecInit(Seq.fill(rsParams.n - rsParams.k + 1)(
                    0.U(rsParams.symbolWidth.W))))
  val evalBRegs = RegInit(VecInit(Seq.fill(rsParams.n - rsParams.k + 1)(
                    0.U(rsParams.symbolWidth.W))))

  // Registers for the Error locator polynomial's coefficients
  val locARegs = RegInit(VecInit(Seq.fill(rsParams.n - rsParams.k + 1)(
                   0.U(rsParams.symbolWidth.W))))
  val locBRegs = RegInit(VecInit(Seq.fill(rsParams.n - rsParams.k + 1)(
                   0.U(rsParams.symbolWidth.W))))
  // Registers for the derivative of Error locator polynomial's coefficients
  val locDerivRegs = RegInit(VecInit(Seq.fill(rsParams.n - rsParams.k)(
                       0.U(rsParams.symbolWidth.W))))

  val theta = evalBRegs(rsParams.n - rsParams.k)
  val gamma = evalARegs(rsParams.n - rsParams.k)

  // The degree of the polynomial formed by evalARegs
  val degA = RegInit((rsParams.n - rsParams.k).asUInt())
  // The degree of the polynomial formed by eValBRegs
  val degB = RegInit(0.U)

  io.in.ready := (state === sInit)

  // Buffer input data for later correction
  // Note: only need to buffer up to k symbols -- which is the length
  // of the original code sequence
  val inputQueue = Module(new Queue(UInt(rsParams.symbolWidth.W), rsParams.k))
  inputQueue.io.enq.bits := io.in.bits
  inputQueue.io.enq.valid := io.in.fire() && (inCntVal < rsParams.k.asUInt())

  // We are going to evaluate the syndrome polynomial with the rootVals
  syndCmp.io.eVals := (0 until syndCmp.getNumCells()).map(x => rootVals(x))
  syndCmp.io.in.bits := io.in.bits
  syndCmp.io.in.valid := io.in.fire()
  syndCmp.io.out.ready := (state === sSyndromeCmp)

  // We are going to evaluate the syndrome polynomial with the rootVals
  chienSearch.io.eVals := (0 until chienSearch.getNumCells()).map(
                           x => rootVals(numRoots - 1 - x))
  chienSearch.io.in.valid := (state === sChienSearch)
  chienSearch.io.in.bits := Mux(state === sChienSearch,
                              locARegs(rsParams.n - rsParams.k), 0.U)
  chienSearch.io.out.ready := (state === sChienSearch)
  val chienOut = RegNext(chienSearch.io.out.bits)
  val (chienSOutCntVal, chienSOutCntDone) = Counter(chienSearch.io.out.fire(),
                                                    chienSearch.getNumCells())

  // This queue stores the roots of the error location polynomial
  // formed by locARegs
  val chienQueue = Module(new Queue(UInt(rsParams.symbolWidth.W),
                     rsParams.n - rsParams.k))
  chienQueue.io.enq.bits := chienSOutCntVal + 1.U
  // Don't bother fixing parity symbols (up to k -- which is the length
  // of the original messages is sufficient.). It does not make sense
  // to carry parity symbols forward at this point
  chienQueue.io.enq.valid := (chienSearch.io.out.bits === 0.U) &&
                             (state === sChienSearch) &&
                             (chienSOutCntVal + 1.U <= rsParams.k.asUInt()) &&
                             (chienSearch.io.out.fire())

  // This PolCompute is used for evaluating the polynomial
  // formed by evalARegs
  // It has one cell (because we only need to evaluate it
  // at a rootVal corresponding to the error location at a time)
  // It has n - k + 1 inputs -- which is the number of evalARegs
  val evalPolyCmp = Module(new PolyCompute(rsParams, 1,
                                           rsParams.n - rsParams.k + 1))

  // This PolCompute is used for evaluating the polynomial
  // formed by locDerivRegs
  // It has one cell (because we only need to evaluate it
  // at a rootVal corresponding to the error location at a time)
  // It has n - k inputs -- which is the number of locDerivRegs
  val locDerivCmp = Module(new PolyCompute(rsParams, 1,
                                           rsParams.n - rsParams.k))

  // Keeping track of when the poly computation starts
  val startEvalPolyCmp = RegInit(false.B)
  val startLocDerivCmp = RegInit(false.B)

  val errorLoc = RegInit(0.U(32.W))
  val evalResult = RegInit(0.U(rsParams.symbolWidth.W))
  val locDerivResult = RegInit(0.U(rsParams.symbolWidth.W))
  val evalResultFired = RegInit(false.B)
  val locDerivResultFired = RegInit(false.B)
  val (evalInCntVal, evalInCntDone) = Counter(evalPolyCmp.io.in.fire(),
                                              rsParams.n - rsParams.k + 1)
  val (locDerivInCntVal, locDerivInCntDone) = Counter(locDerivCmp.io.in.fire(),
                                                      rsParams.n - rsParams.k)

  val rootValIdx = numRoots.asUInt() - 1.U - (rsParams.n.asUInt() - errorLoc)

  // error magnitude = (eval) / (locDeriv * root)
  // This can introduce a long critical path
  val denom = rsParams.GFOp.mul(locDerivResult, rootVals(rootValIdx))
  val errorMagReg = RegNext(rsParams.GFOp.mul(evalResult,
                                              rsParams.GFOp.inv(denom)))

  evalPolyCmp.io.eVals := (0 until evalPolyCmp.getNumCells()).map(
                           x => rootVals(rootValIdx))
  evalPolyCmp.io.in.valid := startEvalPolyCmp
  evalPolyCmp.io.in.bits := evalARegs(rsParams.n - rsParams.k)
  evalPolyCmp.io.out.ready := state === sErrorCorrection2

  locDerivCmp.io.eVals := (0 until locDerivCmp.getNumCells()).map(
                           x => rootVals(rootValIdx))
  locDerivCmp.io.in.valid := startLocDerivCmp
  locDerivCmp.io.in.bits := locDerivRegs(rsParams.n - rsParams.k - 1)
  locDerivCmp.io.out.ready := state === sErrorCorrection2

  // This condition is true when the eval polynomial and the locDeriv polynomial
  // have been computed
  val correctCnd = (state === sErrorCorrection0) &&
                   (evalResultFired && locDerivResultFired)
  val (outCntVal, outCntDone) = Counter(io.out.fire(), rsParams.k)

  // Here we try to emit the output and correct them if needed
  // At each cycle, a new output is fired. If it is correct
  // (determined by index position not matching errorLoc), it is
  // fired as is. If not, we will halt the output generation until
  // we can compute the error magnitude and fix the output
  inputQueue.io.deq.ready := correctCnd || (state === sErrorCorrection1 &&
                                             outCntVal =/= errorLoc - 1.U)
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

      evalARegs(rsParams.n - rsParams.k) := 1.U
      locBRegs(0) := 1.U

      degA := (rsParams.n - rsParams.k).asUInt()
      degB := 0.U

      errorLoc := 0.U
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

        // Store the results of syndrome computation to evalBRegs
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

    // This is based on the Modified Euclidean Algorithm
    // The final results are stored in evalARegs and locARegs
    is (sKeyEquationSolver) {
      when (degA < ((rsParams.n - rsParams.k) / 2).asUInt()) {
        state := sChienSearch
        // odd-th coefficients are zeros due to finite-field
        locDerivRegs.zipWithIndex.filter(_._2 % 2 == 1).map { x => 0.U }
        // even-th coefficients are obtained by the odd-th coefficients
        // of the original polynomial
        locDerivRegs.zip(locARegs.drop(1)).
          zipWithIndex.filter(_._2 % 2 == 0).map { x => x._1._1 := x._1._2 }
      }
      .otherwise {
        // This simply computes the polynomial division
        // using cross multiplications. Avoid inversion
        // because it is costly
        // Note that right now this implmentation requires
        // variant cycles based on the input; it may be
        // susceptible to infinite loop
        // TODO: ensure that it won't happen
        // TODO: A better solution is to use an latency-invariant algorithm

        // Swap the dividend and divisor
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
                aReg := rsParams.GFOp.mul(theta, aReg) ^
                        rsParams.GFOp.mul(gamma, bReg)
              }
            }

            locARegs.zip(locBRegs).map {
              case (aReg, bReg) => {
                aReg := rsParams.GFOp.mul(theta, aReg) ^
                        rsParams.GFOp.mul(gamma, bReg)
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
          when (degA - 1.U >= ((rsParams.n - rsParams.k) / 2).asUInt()) {
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
        val lastVal = locARegs(rsParams.n - rsParams.k)
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
        errorLoc := chienQueue.io.deq.bits
      }

      when (outCntDone) {
        state := sInit
      }
      .otherwise {
        state := sErrorCorrection1
      }
    }

    // Check whether the current output index matches the error location
    is (sErrorCorrection1) {
      errorMagReg := 0.U

      when (outCntDone) {
        state := sInit
      }
      .elsewhen (outCntVal === (errorLoc - 1.U)) {
        state := sErrorCorrection2
        startEvalPolyCmp := true.B
        startLocDerivCmp := true.B
      }
    }

    // Ignite the polynomial computation of eval and locDeriv
    is (sErrorCorrection2) {
      when (evalPolyCmp.io.in.fire()) {
        when (evalInCntDone) {
          startEvalPolyCmp := false.B
        }

        val lastVal = evalARegs(rsParams.n - rsParams.k)
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

        val lastVal = locDerivRegs(rsParams.n - rsParams.k - 1)
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

// CREECBus integration with the RSDecoder module
// What to do:
// *** Decoder
//   + Receive read header from downstream block (slave)
//   + Send read header to upstream block (master)
//   + Receive read data from downstream block (slave)
//   + Send *decoded* read data to upstream block (master)
class ECCDecoderTop(val rsParams: RSParams,
                    val busInParams: BusParams,
                    val busOutParams: BusParams,
  ) extends Module {
  val io = IO(new Bundle {
    val slave = Flipped(new CREECBus(busInParams))
    val master = new CREECBus(busOutParams)
  })

  // There are four states
  //  - sRecvHeader: for accepting the header from the slave port
  //  - sSendHeader: for sending the header to the master port
  //  - sRecvData: for accepting the data from the slave port
  //  - sCompute: RS deconding
  //  - sSendData: send the decoded data to the master port
  val sRecvHeader :: sSendHeader :: sRecvData :: sCompute :: sSendData :: Nil = Enum(5)
  val state = RegInit(sRecvHeader)

  val dec = Module(new RSDecoder(rsParams))

  val dataInReg = RegInit(0.U(busInParams.dataWidth.W))
  val dataOutReg = RegInit(0.U(busOutParams.dataWidth.W))
  val headerReg = Reg(chiselTypeOf(io.slave.header.bits))
  val idReg = RegInit(0.U)

  io.master.data.bits.id := idReg
  io.master.header.bits <> headerReg

  // Unset metadata for ECC
  io.master.header.bits.ecc := false.B
  io.master.header.bits.eccPadBytes := 0.U
  io.master.header.valid := state === sSendHeader

  io.slave.header.ready := state === sRecvHeader

  io.slave.data.ready := state === sRecvData
  io.master.data.valid := state === sSendData
  io.master.data.bits.data := dataOutReg

  // Various counters for keeping track of progress
  val (decInCntVal, decInCntDone) = Counter(dec.io.in.fire(), rsParams.n)
  val (decOutCntVal, decOutCntDone) = Counter(dec.io.out.fire(), rsParams.k)
  // Cannot use Counter for this because the maximum value is not statically known
  val beatCnt = RegInit(0.U(32.W))
  val numBeats = RegInit(0.U(32.W))

  dec.io.in.valid := state === sCompute
  dec.io.in.bits := dataInReg(rsParams.symbolWidth - 1, 0)
  dec.io.out.ready := state === sCompute

  switch (state) {
    is (sRecvHeader) {
      // Properly reset registers to prepare for the next input
      beatCnt := 0.U
      dataInReg := 0.U
      dataOutReg := 0.U

      when (io.slave.header.fire()) {
        state := sSendHeader
        numBeats := io.slave.header.bits.len + 1.U
        headerReg := io.slave.header.bits
      }
    }

    is (sSendHeader) {
      when (io.master.header.fire()) {
        state := sRecvData
      }
    }

    is (sRecvData) {
      when (io.slave.data.fire()) {
        beatCnt := beatCnt + 1.U
        dataInReg := io.slave.data.bits.data
        state := sCompute
      }
    }

    is (sCompute) {
      // Slice the dataInReg register into smaller chunks of size
      // *symbolWidth* for the decoding process
      when (dec.io.in.fire()) {
        dataInReg := (dataInReg >> rsParams.symbolWidth)
      }

      when (dec.io.out.fire()) {
        when (decOutCntDone) {
          state := sSendData
        }

        // Note the endianness
        // The first decoding output is the LSB
        // The last decoding output is the MSB
        val shiftAmt = busOutParams.dataWidth - rsParams.symbolWidth
        dataOutReg := (dataOutReg >> rsParams.symbolWidth) |
          (dec.io.out.bits << shiftAmt)
      }
    }

    is (sSendData) {
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
