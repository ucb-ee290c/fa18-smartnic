package compression

/*
 * Golden model for basic compression.
 */
object CompressionUtils {
  def differential(input: Seq[Byte], encode: Boolean): Seq[Byte] = {
    var output = Seq[Byte]()
    var prev = 0.toByte
    for (i <- input.indices) {
      output = output :+ (if (encode) (input(i) - prev).toByte else (input(i) + prev).toByte)
      prev = if (encode) input(i) else output(i)
    }
    output
  }

  private def runLengthEncode(input: Seq[Byte]): Seq[Byte] = {
    var output = Seq[Byte]()
    var run = 0
    for (b <- input) {
      if (b == 0) {
        if (run == 0) {
          output = output :+ 0.toByte
        }
        if(run == 255) {
          output = output :+ 255.toByte
          run = 0
        }
        else {
          run += 1
        }
      } else {
        if (run != 0) {
          output = output :+ (run - 1).toByte
        }
        output = output :+ b
        run = 0
      }
    }
    if (run != 0)
      output = output :+ (run - 1).toByte
    output
  }

  private def runLengthDecode(input: Seq[Byte]): Seq[Byte] = {
    var output = Seq[Byte]()
    var expand = false
    for (b <- input.map(x => (x + 256) % 256)) {
      if (expand) {
        output = output ++ Seq.fill(b + 1)(0.toByte)
        expand = false
      } else if (b == 0) {
        expand = true
      } else {
        output = output :+ b.toByte
      }
    }
    output
  }

  def runLength(input: Seq[Byte], encode: Boolean): Seq[Byte] = {
    if(encode)
      runLengthEncode(input)
    else
      runLengthDecode(input)
  }

  def compress(input: Seq[Byte], compress: Boolean): Seq[Byte] = {
    if(compress)
      runLength(differential(input, encode = true), encode = true)
    else
      differential(runLength(input, encode = false), encode = false)
  }
}