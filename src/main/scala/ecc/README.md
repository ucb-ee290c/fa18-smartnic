# ECC Chisel Blocks

This directory contains the Reed-Solomon Error-Code Correction (ECC) modules. The implementations of RS Encoder and RS Decoder are provided in Chisel. A software version is also written in Scala for verification. The implementations are statically configurable/parameterizable with the desired number of input symbols and number of output symbols.

As an example, with an RS configuration of (n, k, b) whereas n is the number of output symbols, k is the number of input symbols (implying n - k generated parity symbols), and b is the bitwidth of a symbol, the Encoder accepts k input symbols and produces n output symbols. On the other hand, the Decoder accepts n input symbols and produces k output symbols (without the parity symbols). According to the nature of RS code, the Decoder can correct up to (n - k) / 2 errorneous input symbols.

## File overview

- RSCode.scala: contains the software implementation of Reed-Solomon code in Scala.
- RSParams.scala: contains the Reed-Solomon parameters for the Chisel blocks as well as the hand-crafted Galois-Field operations (multiplication and inversion) in Chisel.
- RSEncoder.scala: contains the Chisel implementation of Reed-Solomon encoder.
- RSDecoder.scala: contains the Chisel implementation of Reed-Solomon decoder.
- CREECBusECCModel: contains the software models of the RS encoder and decoder for CREECBus transaction modeling.

## Module overview

Almost all the modules accepts *RSParams* as a main parameter. *RSParams* defines the RS code configuration (n, k, b) as illustrated above. In addition, other data field are also supplied such as generator coefficients `gCoeffs` or the root tables `Log2Val`, `Val2Log` for fast lookup and reduce the burden of adding extra hardware logic to compute them. These data fields are generated statically by RSCode and provide to the Chisel blocks as parameters.

### RSEncoder

- Parameters: *RSParams*.

- IO:

  + *in*: decoupled IO input
  + *out*: decoupled IO output

- General description: this block uses Linear-Feedback Shift Register to implement Reed-Solomon encoding algorithm. The number of registers equals to number of parity symbols (n - k). The blocks passes through k original input symbols to the output initially, followed by (n - k) computed parity symbols.

### ECCEncoderTop

- Parameters: *RSParams*, *BusParams* (in), *BusParams* (out).

- IO:
  + *slave*: CREECBus input
  + *master*: CREECBus output

- General description: this block is a CREECBus-wrapper module of the core Encoding logic that communicates with the rest of the system. It accepts CREECBus transaction from the slave port, performs encoding computation, and fires new CREECBus transaction with **encoded** data to the master. Notably, the master data has a different bitwidth from the slave data -- this is because of the appendation of the generate parity symbols.

### RSDecoder

- Parameters: *RSParams*.

- IO:
  + *in*: decoupled IO input
  + *out*: decoupled IO output

- General description:
  This block accepts n input symbols and produces k output symbols (stripping the parity symbols). As in general implementation of Reed-Solomon decoding algorithm, there are four phases: Syndrome Computation, Key Equation Solver, Chien Search, and Error Correction. The Syndrome Computation determines whether the n input symbols are correct (zero syndrome). If no error is found, the block simply forwards the first k input symbols to the output port. Otherwise, Key Equation Solver is invoked. For our implementation, Modified Euclidean Algorithm is chosen for this task. It involves polynomial division operation which can be avoided by using cross multiplication technique. Finally, the Chien Search locates the error symbols, and the Error Correction fixes them.

  The main limitation of our Decoder is the long critical path that involves one inversion, since our GF inversion design is not efficient (basically chainning many GF multipliers together). Future work entails pipelining this operation, or simply using a lookup table for inversion.

  Furthermore, this block heavily use polynomial evaluations for many tasks, such as Syndrome Computation, Chien Search, and Error Correction. Polynomial evaluation is a process of computing a value of a polynomial given its coefficients and evaluation point. This computation can be effectively implemented based on Horner's method. The subblocks **PolyCell** and **PolyCompute** are used for polynomial evaluation, and they are reusable in many places of the Decoding logic.

#### PolyCell

- Parameters: *RSParams*

- IO:
  + *running*: checks if the cell is running
  + *SIn*: pass-through input from previous cell
  + *Eval*: evaluation point
  + *Rx*: streams a coefficient to the cell
  + *SOut*: output of the cell

- General description: this is a primitive block that is instantiated by **PolyComupte**. This block evaluates a polynomial based on the input coefficients and the evaluation point. The result is available after all the coefficients have been streamed in. This block can also serve as a pass-through block to forward the data from one neighboring block to another.

#### PolyCompute

- Parameters: *RSParams*, *numCells*, *numInputs*, *reverse*

- IO:
  + *eVals*: a vector of evaluation points
  + *in*: decoupled IO input
  + *out*: decoupled IO output

- General description: this block computes a polynomial at multiple evaluation points concurrently. It does so by instantiating **PolyCell** modules in an 1D systolic array fashion. The number of **PolyCell** modules equals the number of evaluation points and is configured by the parameter *numCells*. The input accepts *numInputs* coefficients. The direction of cell flow is also configurable via *reverse*. If the parameter is false, the data flows from the first cell to the last cell, and then to the output of the block, and vice versa. This comes handy since there might be situation where one needs a particular order of polynomial evaluation results. For example, Chien search requires a different result ordering than syndrome computation. This configuration gets rid of the hassle of manually reordering the data.

### ECCDecoderTop

- Parameters: *RSParams*, *BusParams*, *BusParams*

- IO:
  + *slave*: CREECBus input
  + *master*: CREECBus output

- General description: similar to **ECCEncoderTop**, this blocks is a CREECBus-wrapper of the core decoding logic. The slave port accepts a CREECBus transaction, then the module performs decoding computation, and sends the **decoded** result to the *master*. Also, the master data width is different from the slave data width due to the stripping of parity symbols.
