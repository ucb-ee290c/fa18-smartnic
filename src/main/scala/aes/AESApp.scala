package aes

import chisel3._

/**
  * Make an unapply function for the argument parser.
  * It allows us to match on parameters that are integers
  */
object Int {
  def unapply(v: String): Option[Int] = {
    try {
      Some(v.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }
}


/**
 * Define entry point for CORDIC generator
 */
object AESApp extends App {
  val usage = s"""Cordic arguments:
  |--xy <Int>\t\tWidth of x and y
  |""".stripMargin

  /**
   * Parse arguments
   *
   * Some arguments are used by the cordic generator and are used to construct a FixedCordicParams object.
   * The rest get returned as a List[String] to pass to the Chisel driver
   *
   */
//  def argParse(args: List[String], params: FixedCordicParams): (List[String], FixedCordicParams) = {
//    args match {
//      case "--help" :: tail =>
//        println(usage)
//        val (newArgs, newParams) = argParse(tail, params)
//        ("--help" +: newArgs, newParams)
//      case "--xy" :: Int(xy) :: tail => argParse(tail, params.copy(xyWidth = xy))
//      case "--stagesPerCycle" :: Int(spc) :: tail => argParse(tail, params.copy(stagesPerCycle = spc))
//      case chiselOpt :: tail => {
//        val (newArgs, newParams) = argParse(tail, params)
//        (chiselOpt +: newArgs, newParams)
//      }
//      case Nil => (args, params)
//    }
//  }
//  val defaultParams = FixedCordicParams(
//    xyWidth = 12,
//    zWidth = 12,
//    stagesPerCycle = 1
//  )
//  val (chiselArgs, params) = argParse(args.toList, defaultParams)
  // Run the Chisel driver to generate a cordic
  //Driver.execute(Array("None"), () => new AES128Combinational())
  Driver.execute(Array("None"), () => new AES128TimeInterleave())
  //Driver.execute(Array("None"), () => new AES128())

  //Driver.execute(Array("None"), () => new InvAES128Combinational())
  Driver.execute(Array("None"), () => new InvAES128())

  //Driver.execute(Array("None"), () => new AESTopCombinational())
  //Driver.execute(Array("None"), () => new AESTopTimeInterleave())
  Driver.execute(Array("None"), () => new AESTopFullTimeInterleave())

  Driver.execute(Array("None"), () => new AESTopCREECBus(new AESBusParams))
}
