package compression

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

object CompressionFunctions {
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
    if(run != 0)
      output = output :+ (run - 1).toByte
    output
  }

  def runLengthDecode(input: List[Byte]): List[Byte] = {
    var output = List[Byte]()
    var expand = false
    for(i <- 0 until input.length) {
      if(expand) {
        output = output ++ List.fill(input(i) + 1)(0.toByte)
        expand = false
      } else if(input(i) == 0) {
        expand = true
      } else {
        output = output :+ input(i)
      }
    }
    output
  }
}

class DifferentialEncoderTester(c: Coder) extends PeekPokeTester(c) {
//  var myList = List.fill(10)(45.toByte)
  var myList = List(5, 6, 7, 6, 6, 6, 6, 6, 6, 6, 7, 6, 7, 5, 6, 7, 7, 6, 5, 6, 5).map{_.toByte}
  println(myList.toString())
  myList = CompressionFunctions.differentialEncode(myList)
  println(myList.toString())
  myList = CompressionFunctions.runLengthEcode(myList)
  println(myList.toString())
  myList = CompressionFunctions.runLengthDecode(myList)
  println(myList.toString())
  myList = CompressionFunctions.differentialDecode(myList)
  println(myList.toString())
}

/**
  * From within sbt use:
  * testOnly example.test.CompressionTester
  * From a terminal shell use:
  * sbt 'testOnly example.test.CompressionTester'
  */
class CompressionTester extends ChiselFlatSpec {
  "DifferentialCoderTester" should "encode" in {
    Driver(() => new Coder, "firrtl") {
      c => new DifferentialEncoderTester(c)
    } should be(true)
  }

//  "running with --fint-write-vcd" should "create a vcd file from your test" in {
//    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new Compressor) {
//      c => new CompressionUnitTester(c)
//    } should be(true)
//  }
}