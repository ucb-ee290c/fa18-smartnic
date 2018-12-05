package ecc

// Software implementation of the Reed-Solomon encoder & decoder
class RSCode(numSyms: Int, numMsgs: Int, symbolWidth: Int,
             verbose: Boolean = false) {
  val numRoots = BigInt(2).pow(symbolWidth).toInt
  val numPars = numSyms - numMsgs
  var Log2Val: Array[Int] = new Array[Int](numRoots)
  var Val2Log: Array[Int] = new Array[Int](numRoots)

  // Primitive Polynomial
  val fConst = {
    symbolWidth match {
      // x^3 + x + 1
      case 3 => Integer.parseInt("1011", 2)
      // x^4 + x + 1
      case 4 => Integer.parseInt("10011", 2)
      // x^5 + x^2 + 1
      case 5 => Integer.parseInt("100101", 2)
      // x^6 + x + 1
      case 6 => Integer.parseInt("1000011", 2)
      // x^7 + x^3 + 1
      case 7 => Integer.parseInt("10001001", 2)
      // x^8 + x^4 + x^3 + x^2 + 1
      case 8 => Integer.parseInt("100011101", 2)
      // x^10 + x^3 + 1
      case 10 => Integer.parseInt("10000001001", 2)
      // x^16 + x^12 + x^3 + x + 1
      case 16 => Integer.parseInt("10001000000001011", 2)
      case _ => 0
    }
  }

  require(fConst != 0, "Unsupported symbol width!")

  Log2Val(0) = 1
  Log2Val(1) = 2 // according to the spec, usually choose a^1 to be 2
  for (i <- 2 until numRoots) {
    Log2Val(i) = Log2Val(i - 1) << 1
    if (Log2Val(i) >= numRoots) {
      Log2Val(i) = (Log2Val(i) % numRoots) ^ (fConst % numRoots)
    }
  }

  for (i <- 0 until numRoots) {
    Val2Log(Log2Val(i)) = i
  }

  def add(a: Int, b: Int): Int = {
    a ^ b
  }

  def mul(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      0
    } else {
      Log2Val((Val2Log(a) + Val2Log(b)) % (numRoots - 1))
    }
  }

  def pow(a: Int, n: Int): Int = {
    (0 until n).foldLeft(1) { (prev, next) => mul(prev, a) }
  }

  def printValRootTable() {
    for (i <- 0 until numRoots) {
      printf("Val2Log(%d) = %d\n", i, Val2Log(i))
    }
  }

  def printLogRootTable() {
    for (i <- 0 until numRoots) {
      printf("Log2Val(%d) = %d\n", i, Log2Val(i))
    }
  }

  def inv(a: Int): Int = {
    //pow(a, numRoots - 2)
    var tmp = a
    var result = 1
    while (tmp != 1) {
      tmp = tmp << 1
      if (tmp >= numRoots) {
        tmp = (tmp % numRoots) ^ (fConst % numRoots)
      }
      result = result << 1
      if (result >= numRoots) {
        result = (result % numRoots) ^ (fConst % numRoots)
      }

    }
    result
  }

  val invTable = (1 until numRoots) map { x => inv(x) }

  // Generator Polynomial
  // g(X) = (X + a^1)(X + a^2)(X + a^3) ... (X + a^numPars)
  //      = gCoeffs(0) + gCoeffs(1) * X^1 + gCoeffs(2) * X^2 + ... + gCoeffs(numPars) * X^numPars
  val gCoeffs = {
    val powSets = (0 to numPars - 1).toSet[Int].subsets.map(_.toList).toList
    var coeffs = new Array[Int](numPars)

    for (i <- 0 until powSets.size) {
      val coeffIdx = numPars - powSets(i).size
      if (coeffIdx < numPars) {
        val powSum = powSets(i).reduce(_ + _) % (numRoots - 1)
        coeffs(coeffIdx) = add(coeffs(coeffIdx), Log2Val(powSum))
      }
    }
    coeffs
  }

  def encode(msgs: Seq[Int]): Seq[Int] = {
    var pars: Array[Int] = new Array[Int](numPars)
    for (i <- 0 until numMsgs) {
      val tmp = pars.map(x => x)
      val feedback = add(msgs(i), pars(numPars - 1))

      for (j <- 0 until numPars) {
        if (j == 0) {
          pars(j) = mul(feedback, gCoeffs(j))
        } else {
          pars(j) = add(mul(feedback, gCoeffs(j)), tmp(j - 1))
        }
      }
    }
    // Append input messages with the generated parity symbols
    msgs ++ pars.reverse
  }

  def noiseGen(syms: Seq[Int], numErrorSyms: Int): Seq[Int] = {
    val noiseSyms = new Array[Int](numSyms)
    for (i <- 0 until numSyms) {
      noiseSyms(i) = syms(i)
    }

    // Randomly pick locations for introducing error symbols
    // It's okay if we pick the same location multiple times
    // as long as the number of errorneous locations does not
    // exceed *numErrorSyms*
    for (i <- 0 until numErrorSyms) {
      val errorIdx = scala.util.Random.nextInt(numSyms - 1)
      noiseSyms(errorIdx) = scala.util.Random.nextInt(numRoots - 1)
    }

    noiseSyms
  }

  // This function computes the following polynomial
  // coeffs(0) * X^(0) + coeffs(1) * X^(1) + ... + coeffs(d - 1) * X^(d - 1)
  // where X = variable, d = coeffs.size
  def evaluatePoly(coeffs: Seq[Int], variable: Int): Int = {
    val degree = coeffs.size
    var result = 0
    for (i <- 0 until degree) {
      result = result ^ mul(coeffs(i), pow(variable, i))
    }
    result
  }

  // A correct symbol sequence forms a polynomial that has roots
  // at a^0, a^1, ..., a^(numPars - 1)
  def syndromeCompute(syms: Seq[Int]): (Seq[Int], Boolean) = {
    var syndromes = List[Int]()
    val revSyms = syms.reverse
    var foundNZSym = false
    // Syndrome computation
    for (i <- 0 until numPars) {
      val res = evaluatePoly(revSyms, Log2Val(i))
      syndromes = syndromes :+ res
      if (res != 0) {
        foundNZSym = true
      }
    }
    (syndromes, foundNZSym)
  }

  def verifySyms(syms: Seq[Int]): Boolean = {
    syndromeCompute(syms)._2 == false
  }

  def findDeg(coeffs: Seq[Int]): Int = {
    var degVal = 0
    for (i <- 0 until coeffs.size) {
      if (coeffs(i) != 0) {
        degVal = i
      }
    }
    degVal
  }

  def decode(inSyms: Seq[Int]): Seq[Int] = {
    val (syndromes, foundNZSym) = syndromeCompute(inSyms)
    if (!foundNZSym) {
      // the input symbol sequence is bug-free
      inSyms
    } else {
      val size = numPars + 1
      var evaluatorsA = new Array[Int](size)
      var evaluatorsB = new Array[Int](size)
      var locatorsA = new Array[Int](size)
      var locatorsB = new Array[Int](size)

      evaluatorsA(numPars) = 1

      for (i <- 0 until numPars) {
        evaluatorsB(i) = syndromes(i)
      }
      locatorsB(0) = 1

      var degA = findDeg(evaluatorsA)
      var degB = findDeg(evaluatorsB)
      var numIters = 0

      if (verbose) {
        for (i <- 0 until size) {
          printf("evalB(%d) = %d\n", i, evaluatorsB(i))
        }
        printf("[initial] degA = %d, degB = %d\n", degA, degB)
      }

      while (degA >= numPars / 2) {
        numIters = numIters + 1

        // Let's swap!
        if (degA < degB && evaluatorsB(size - 1) != 0 &&
                           evaluatorsA(size - 1) != 0) {
          val tmpEvalA = evaluatorsA.map(x => x)
          val tmpLocA = locatorsA.map(x => x)
          for (i <- 0 until size) {
            evaluatorsA(i) = evaluatorsB(i)
            locatorsA(i) = locatorsB(i)
          }
          for (i <- 0 until size) {
            evaluatorsB(i) = tmpEvalA(i)
            locatorsB(i) = tmpLocA(i)
          }
          val tmpdeg = degA
          degA = degB
          degB = tmpdeg
        }

        val theta = evaluatorsB(size - 1)
        val gamma = evaluatorsA(size - 1)

        if (theta != 0 && gamma != 0) {
          for (i <- 0 until size) {
            evaluatorsA(i) = mul(theta, evaluatorsA(i)) ^
                             mul(gamma, evaluatorsB(i))
            locatorsA(i) = mul(theta, locatorsA(i)) ^
                             mul(gamma, locatorsB(i))

          }
        }

        if (theta == 0) {
          val tmpEvalB = evaluatorsB.map(x => x)
          evaluatorsB(0) = tmpEvalB(size - 1)
          for (i <- 0 until size - 1) {
            evaluatorsB(i + 1) = tmpEvalB(i)
          }

          val tmpLocB = locatorsB.map(x => x)
          locatorsB(0) = tmpLocB(size - 1)
          for (i <- 0 until size - 1) {
            locatorsB(i + 1) = tmpLocB(i)
          }

        }

        if (gamma == 0) {
          degA = degA - 1
          // Don't shift at the last iteration
          if (degA >= numPars / 2) {
            val tmpEvalA = evaluatorsA.map(x => x)
            evaluatorsA(0) = tmpEvalA(size - 1)
            for (i <- 0 until size - 1) {
              evaluatorsA(i + 1) = tmpEvalA(i)
            }

            val tmpLocA = locatorsA.map(x => x)
            locatorsA(0) = tmpLocA(size - 1)
            for (i <- 0 until size - 1) {
              locatorsA(i + 1) = tmpLocA(i)
            }
          }
        }
      }

      if (verbose) {
        printf("Check Key Equation Solver result: done after %d iters\n",
               numIters)
        for (i <- 0 until size) {
          printf("[%d] eval=%d, loc=%d\n", i, evaluatorsA(i), locatorsA(i))
        }
      }

      val locatorsDeriv = new Array[Int](numPars)
      for (i <- 0 until numPars) {
        if (i % 2 == 1) {
          locatorsDeriv(i) = 0
        }
        else {
          locatorsDeriv(i) = locatorsA(i + 1)
        }
      }

      // Chien search
      var chienRootIndices = List[Int]()
      for (i <- numSyms - 1 to 0 by - 1) {
        val res = evaluatePoly(locatorsA, Log2Val(numRoots - 1 - i))
        if (res == 0) {
          chienRootIndices = chienRootIndices :+ (numSyms - i)
          if (verbose) {
            printf("found Chien root index: %d\n", numSyms - i)
          }
        }

      }

      // Error correction
      var correctedSyms = new Array[Int](numSyms)
      for (i <- 0 until numSyms) {
        correctedSyms(i) = inSyms(i)
      }

      for (i <- 0 until chienRootIndices.size) {
        val idx = chienRootIndices(i)
        val chienRootVal = Log2Val(numRoots - 1 - (numSyms - idx))
        correctedSyms(idx - 1) = correctedSyms(idx - 1) ^
          mul(evaluatePoly(evaluatorsA, chienRootVal),
              invTable(mul(chienRootVal, evaluatePoly(locatorsDeriv, chienRootVal)) - 1))
      }

      correctedSyms
    }
  }

}
