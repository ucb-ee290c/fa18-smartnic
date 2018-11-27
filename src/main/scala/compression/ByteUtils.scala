package compression

/*
 * Functions for manipulating bytes.
 */
object ByteUtils {
  /*
   * Compacts 8 bytes into a 64-bit int.
   */
  def squish(bytes: Seq[Byte]): BigInt = {
    require(bytes.length == 8)
    var squished = BigInt(0)
    for (i <- 0 until 8) {
      squished |= (BigInt(bytes(i)) & 0xFF) << (8 * i)
    }
    squished
  }

  /*
   * Unpacks a 64-bit int into 8 bytes.
   */
  def unsquish(squished: BigInt): Seq[Byte] = {
    var bytes: List[Byte] = List[Byte]()
    for (i <- 0 until 8) {
      bytes = bytes :+ ((squished & (BigInt(0xFF) << (8 * i))) >> (8 * i)).toByte
    }
    bytes
  }
}