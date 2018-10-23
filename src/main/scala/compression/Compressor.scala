// See README.md for license details.

package compression

import chisel3._

class Compressor extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })

  io.out := io.in
}
