package ecc
import org.scalatest.{FlatSpec, Matchers}

class ECCSpec extends FlatSpec with Matchers {
  behavior of "ECC"

  val numTrials = 40
  var trials: List[(Seq[Int], Seq[Int], Seq[Int])] = List()
  val verbose = if (numTrials <= 10) {
    true
  }
  else {
    false
  }

  // RS(16, 8, 8)
  val numSymbols = 16
  val numMsgs = 8
  val symbolWidth = 8
  val rs = new RSCode(numSymbols, numMsgs, symbolWidth, verbose)


  // Generate test code sequences
  for (t <- 0 until numTrials) {
    if (verbose) {
      printf("===TRIAL %d\n", t)
    }

    val msgs = Seq.fill(numMsgs) {
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

    val maxNumErrorSyms = (numSymbols - numMsgs) / 2
    val noisySyms = rs.noiseGen(swSyms, maxNumErrorSyms)

    if (verbose) {
      for (i <- 0 until numSymbols) {
        printf("noisySyms(%d) = %d\n", i, noisySyms(i))
      }
    }

    // Running software RS Decoder
    val swCorrectedSyms = rs.decode(noisySyms)
    if (verbose) {
      for (i <- 0 until swCorrectedSyms.size) {
        printf("swCorrectedSyms(%d) = %d\n", i, swCorrectedSyms(i))
      }
    }

    // Need to pass this test to go further
    require(rs.verifySyms(swCorrectedSyms), "Incorrect software RS decoder!")

    val item = (swSyms, noisySyms, swCorrectedSyms)
    trials = trials :+ item
  }

  val rsParams = RSParams(
    n = numSymbols,
    k = numMsgs,
    symbolWidth = symbolWidth,
    gCoeffs = rs.gCoeffs,
    fConst = rs.fConst,
    rs.Log2Val,
    rs.invTable
  )
}
