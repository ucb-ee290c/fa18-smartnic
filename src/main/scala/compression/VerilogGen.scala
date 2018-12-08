package compression

import chisel3.Driver
import interconnect.BusParams

object VerilogGen extends App {
  implicit val creecParams: BusParams = BusParams.creec
  Driver.execute(Array("None"), () => new Compressor(BusParams.blockDev, compress = true))
}