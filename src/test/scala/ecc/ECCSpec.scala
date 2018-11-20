package ecc
import scala.math
import org.scalatest.{FlatSpec, Matchers}

// This only tests the Reed-Solomon encoder
class ECCSpec extends FlatSpec with Matchers {
  behavior of "ECC"

  val numTrials = 10
  var trials: List[(Seq[Int], Array[Int], Seq[Int])] = List()
  val verbose = if (numTrials <= 10) {
    true
  }
  else {
    false
  }

  // RS(16, 8)
  val numSymbols = 16
  val numMsgs = 8
  val symbolWidth = 8
  val rs = new RSCode(numSymbols, numMsgs, symbolWidth, verbose)


  // Generate test code sequences
  for (t <- 0 until numTrials) {
    if (verbose) {
      printf("===TRIAL %d\n", t)
    }

    var msgs = Seq.fill(numMsgs) {
      scala.util.Random.nextInt(rs.numRoots - 1)
    }

    // Running software RS Encoder
    val swSyms = rs.encode(msgs)
    if (verbose) {
      for (i <- 0 until swSyms.size) {
        printf("swSyms(%d) = %d\n", i, swSyms(i))
      }
    }

    // Need to pass this test to go further
    require(rs.verifySyms(swSyms), "Incorrect software RS encoder!")

    var buggySyms = new Array[Int](numSymbols)
    for (i <- 0 until numSymbols) {
      buggySyms(i) = swSyms(i)
    }

    // Randomly pick locations for introducing error symbols
    // It's okay if we pick the same location multiple times
    // as long as the number of errorneous locations does not
    // exceed *maxNumErrorSyms*
    val maxNumErrorSyms = (numSymbols - numMsgs) / 2
    for (i <- 0 until maxNumErrorSyms) {
      val errorIdx = scala.util.Random.nextInt(numSymbols - 1)
      buggySyms(errorIdx) = scala.util.Random.nextInt(rs.numRoots - 1)
    }

    if (verbose) {
      for (i <- 0 until numSymbols) {
        printf("buggySyms(%d) = %d\n", i, buggySyms(i))
      }
    }

    // Running software RS Decoder
    val swCorrectedSyms = rs.decode(buggySyms)
    if (verbose) {
      for (i <- 0 until swCorrectedSyms.size) {
        printf("swCorrectedSyms(%d) = %d\n", i, swCorrectedSyms(i))
      }
    }

    // Need to pass this test to go further
    require(rs.verifySyms(swCorrectedSyms), "Incorrect software RS decoder!")

    val item = (swSyms, buggySyms, swCorrectedSyms)
    trials = trials :+ item
  }

  val rsParams = RSParams(
    n = numSymbols,
    k = numMsgs,
    symbolWidth = symbolWidth,
    gCoeffs = rs.gCoeffs,
    fConst = rs.fConst,
    rs.Log2Val,
    rs.Val2Log
  )
}
