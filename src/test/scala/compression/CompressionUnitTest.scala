package compression

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

object CompressionUtils {
  private def differential(input: List[Byte], encode: Boolean): List[Byte] = {
    var output = List[Byte]()
    var prev = 0.toByte
    for (i <- 0 until input.length) {
      output = output :+ (if (encode) (input(i) - prev).toByte else (input(i) + prev).toByte)
      prev = if (encode) input(i) else output(i)
    }
    output
  }

  def differentialEncode(input: List[Byte]): List[Byte] = {
    differential(input, true)
  }

  def differentialDecode(input: List[Byte]): List[Byte] = {
    differential(input, false)
  }

  /*TODO all these functions have the form {create output, use 1 state variable,
   *  loop through input, apply some function that assigns to the output from
   *  the input based on the state, and return the output}. Generalize this.
   */

  def runLengthEcode(input: List[Byte]): List[Byte] = {
    var output = List[Byte]()
    var run = 0
    for (i <- 0 until input.length) {
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

  def runLengthDecode(input: List[Byte]): List[Byte] = {
    var output = List[Byte]()
    var expand = false
    for (i <- 0 until input.length) {
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
}

class DifferentialEncoderTester(c: Coder) extends PeekPokeTester(c) {
  //  var myList = List(5, 6, 7, 6, 6, 6, 6, 6, 6, 6, 7, 6, 7, 5, 6, 7, 7, 6, 5, 6, 5).map{_.toByte}
  //  println(myList.toString())
  //  myList = CompressionFunctions.differentialEncode(myList)
  //  println(myList.toString())
  //  myList = CompressionFunctions.runLengthEcode(myList)
  //  println(myList.toString())
  //  myList = CompressionFunctions.runLengthDecode(myList)
  //  println(myList.toString())
  //  myList = CompressionFunctions.differentialDecode(myList)
  //  println(myList.toString())
  //TODO
}

class DifferentialDecoderTester(c: Coder) extends PeekPokeTester(c) {
  //TODO
}

class RunLengthEncoderTester(c: RunLengthEncoder) extends PeekPokeTester(c) {
  val input = List(2, 3, 0, 0, 0, 0, 0, 5, 7, 0, 8, 9, 0, 0, 0, 1).map {
    _.toByte
  }
  var output = List[BigInt]()
  poke(c.io.out.ready, true)
  poke(c.io.in.valid, true)
  var i = 0
  while (i < input.length) {
    poke(c.io.in.bits, input(i))
    step(1)
    if (peek(c.io.out.valid) != BigInt(0)) {
      output = output :+ peek(c.io.out.bits)
      i = i + 1
    } else if (peek(c.io.in.ready) != BigInt(0)) {
      output = output :+ peek(c.io.out.bits)
      i = i + 1
    }
  }
  println(input.toString())
  println(output.toString())
  println(CompressionUtils.runLengthEcode(input).toString())

  //  def oneInput(i: Int) = {
  //    poke(c.io.in.bits, i)
  //
  //    poke(c.io.in.valid, true)
  //    step(1)
  //    poke(c.io.in.valid, false)
  //
  //    step(4)
  //
  //    poke(c.io.out.ready, true)
  //    step(1)
  //    poke(c.io.out.ready, false)
  //
  //    step(4)
  //  }
  //
  //  oneInput(2)
  //  oneInput(3)
  //  oneInput(0)
  //  oneInput(0)
  //  oneInput(0)
  //  oneInput(0)
  //  oneInput(0)
  //  oneInput(5)
  //  oneInput(7)
}

/**
  * From within sbt use:
  * testOnly example.test.CompressionTester
  * From a terminal shell use:
  * sbt 'testOnly example.test.CompressionTester'
  */
class CompressionTester extends ChiselFlatSpec {
  //TODO: rewrite this
  //  "DifferentialCoderTester" should "encode" in {
  //    Driver(() => new Coder, "firrtl") {
  //      c => new DifferentialEncoderTester(c)
  //    } should be(true)
  //  }

  //TODO: rewrite this
  //  "DifferentialCoderTester" should "decode" in {
  //    Driver(() => new Coder(new CoderParams(false)), "firrtl") {
  //      c => new DifferentialDecoderTester(c)
  //    } should be(true)
  //  }

  "RunLengthCoder" should "decode" in {
    Driver.execute(Array("-fiwv", "--backend-name", "firrtl", "--target-dir", "test_run_dir/creec", "--top-name", "creec"), () => new RunLengthEncoder) {
      c => new RunLengthEncoderTester(c)
    } should be(true)
    //    Driver(() => new RunLengthEncoder(), "firrtl") {
    //      c => new RunLengthEncoderTester(c)
    //    } should be(true)
  }

  //  "running with --fint-write-vcd" should "create a vcd file from your test" in {
  //    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new Compressor) {
  //      c => new CompressionUnitTester(c)
  //    } should be(true)
  //  }
}