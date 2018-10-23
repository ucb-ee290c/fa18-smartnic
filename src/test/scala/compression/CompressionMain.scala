package compression

import chisel3._

/**
  * test:runMain compression.CompressionMain
  * To see all command line options use:
  * test:runMain compression.CompressionMain --help
  * To run with verilator:
  * test:runMain compression.CompressionMain --backend-name verilator
  * To run with verilator from your terminal shell use:
  * sbt 'test:runMain compression.CompressionMain --backend-name verilator'
  */
object CompressionMain extends App {
  iotesters.Driver.execute(args, () => new Compressor) {
    c => new CompressionUnitTester(c)
  }
}

/**
  * To run from sbt
  * test:runMain compression.CompressionRepl
  * To run from sbt and see the half a zillion options try
  * test:runMain compression.CompressionRepl --help
  */
object CompressionRepl extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new Compressor)
}