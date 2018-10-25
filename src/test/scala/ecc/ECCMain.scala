package ecc

import chisel3._

/**
  * test:runMain ecc.ECCMain
  * To see all command line options use:
  * test:runMain ecc.ECCMain --help
  * To run with verilator:
  * test:runMain ecc.ECCMain --backend-name verilator
  * To run with verilator from your terminal shell use:
  * sbt 'test:runMain ecc.ECCMain --backend-name verilator'
  */
object ECCMain extends App {
  iotesters.Driver.execute(args, () => new ECC) {
    c => new ECCUnitTester(c)
  }
}

/**
  * To run from sbt
  * test:runMain ecc.ECCRepl
  * To run from sbt and see the half a zillion options try
  * test:runMain ecc.ECCRepl --help
  */
object ECCRepl extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new ECC)
}
