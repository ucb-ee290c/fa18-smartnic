package aes

import chisel3._
import interconnect.BusParams

/**
  * Generate Verilog for AES blocks for synthesis runs
  */
object AESApp extends App {
  Driver.execute(Array("None"), () => new AES128TimeInterleave())
  Driver.execute(Array("None"), () => new InvAES128())
  Driver.execute(Array("None"), () => new AESTopFullTimeInterleave())
  Driver.execute(Array("None"), () => new AESTopCREECBus(BusParams.aes))
}
