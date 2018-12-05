package compression

import chisel3.Driver
import interconnect.{BusParams, CREECBusParams}

object VerilogGen extends App {
  implicit val creecParams: CREECBusParams = new CREECBusParams
  Driver.execute(Array("None"), () => new Compressor(BusParams.blockDev, compress = true))
}