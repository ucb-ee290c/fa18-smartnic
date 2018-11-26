package compression

/*
 * Golden model for basic compression.
 */
object CompressionUtils {
  def differential(input: Seq[Byte], encode: Boolean): Seq[Byte] = {
    var output = List[Byte]()
    var prev = 0.toByte
    for (i <- input.indices) {
      output = output :+ (if (encode) (input(i) - prev).toByte else (input(i) + prev).toByte)
      prev = if (encode) input(i) else output(i)
    }
    output
  }

  private def runLengthEncode(input: Seq[Byte]): Seq[Byte] = {
    var output = List[Byte]()
    var run = 0
    for (i <- input.indices) {
      if (input(i) == 0) {
        if (run == 0) {
          output = output :+ 0.toByte
        }
        run += 1
      } else {
        if (run != 0) {
          output = output :+ (run - 1).toByte
        }
        output = output :+ input(i)
        run = 0
      }
    }
    if (run != 0)
      output = output :+ (run - 1).toByte
    output
  }

  private def runLengthDecode(input: Seq[Byte]): Seq[Byte] = {
    var output = List[Byte]()
    var expand = false
    for (i <- input.indices) {
      if (expand) {
        output = output ++ List.fill(input(i) + 1)(0.toByte)
        expand = false
      } else if (input(i) == 0) {
        expand = true
      } else {
        output = output :+ input(i)
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