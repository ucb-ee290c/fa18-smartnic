package compression

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

/*
 * Golden model for basic compression.
 */
object CompressionUtils {
  private def differential(input: List[Byte], encode: Boolean): List[Byte] = {
    var output = List[Byte]()
    var prev = 0.toByte
    for (i <- input.indices) {
      output = output :+ (if (encode) (input(i) - prev).toByte else (input(i) + prev).toByte)
      prev = if (encode) input(i) else output(i)
    }
    output
  }

  def differentialEncode(input: List[Byte]): List[Byte] = {
    differential(input, encode = true)
  }

  def differentialDecode(input: List[Byte]): List[Byte] = {
    differential(input, encode = false)
  }

  /*TODO all these functions have the form {create output, use 1 state variable,
   *  loop through input, apply some function that assigns to the output from
   *  the input based on the state, and return the output}. Generalize this.
   */

  def runLengthEcode(input: List[Byte]): List[Byte] = {
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

  def runLengthDecode(input: List[Byte]): List[Byte] = {
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
}

/*
 * Tests byte-steam run-length encoder.
 */
class RunLengthEncoderTester(c: RunLengthEncoder) extends PeekPokeTester(c) {
  var inputs: List[List[Byte]] = List(
    List(0, 0, 0, 0, 0, 0, 0, 0, 45),
    List(2, 3, 0, 0, 0, 0, 0, 5, 7, 0, 8, 9, 0, 0, 0, 1),
    List(9, 9, 8, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0),
    List(0),
    List(0, 0, 0, 0),
    List(0, 5, 0, 5, 0, 5, 0)
  )
  for (input <- inputs) {
    test(input)
  }

  def test(input: List[Byte]) = {
    var output: List[Byte] = List[Byte]()
    val expectedOutput = CompressionUtils.runLengthEcode(input)
    poke(c.io.out.ready, true)
    poke(c.io.in.valid, true)
    var i = 0
    while (i < input.length || output.length < expectedOutput.length) {
      if (i < input.length) {
        if (peek(c.io.in.ready) != BigInt(0)) {
          poke(c.io.in.valid, true)
          poke(c.io.in.bits.byte, input(i))
          poke(c.io.in.bits.flag, i == input.length - 1)
          i = i + 1
        }
      }
      if (peek(c.io.out.valid) != BigInt(0)) {
        output = output :+ peek(c.io.out.bits).toByte
      }
      step(1)
      if(i > 50)
        expect(false, "took too long.")
    }
//    println("in: " + input.toString())
//    println("out: " + output.toString())
//    println("exp: " + expectedOutput.toString())
    expect(output == expectedOutput, "actual output did not match expected output.")
  }
}

class DifferentialEncoderTester(c: Coder) extends PeekPokeTester(c) {
  //TODO
}

class DifferentialDecoderTester(c: Coder) extends PeekPokeTester(c) {
  //TODO
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

  "RunLengthCoder" should "encode" in {
    Driver.execute(Array("-fiwv", "--backend-name", "treadle", "--tr-write-vcd", "--target-dir", "test_run_dir/creec", "--top-name", "creec"), () => new RunLengthEncoder) {
      c => new RunLengthEncoderTester(c)
    } should be(true)
  }
}