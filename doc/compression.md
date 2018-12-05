# Compression Chisel Blocks

The compression scheme implemented in this repository is basic differential + run-length encoding.

## Compression Scheme Overview

### Differential Encoding

A sequence of bytes is transformed into an initial byte followed by the difference between each of the successive bytes. For example, the sequence

`[3, 4, 7, 9, 2, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 7, 1, 1, 1, 1, 1, 1, 3, 2, 2, 1]`

would be converted to

`[3, 1, 3, 2,-7, 3,-1, 0, 0, 0, 0, 0, 0, 0, 0, 3,-6, 0, 0, 0, 0, 0, 2,-1, 0,-1]`.

This changes any sequence of repeated values into a sequence of zeros. It can also reduce the number of unique symbols required to represent data, but that is not relevant to this particular scheme.

### Run-Length Encoding

Each sequence of zeros within the data is converted to a single zero followed by a number of additional zeros. For example, the sequence

`[3, 1, 3, 2,-7, 3,-1, 0, 0, 0, 0, 0, 0, 0, 0, 3,-6, 0, 0, 0, 0, 0, 2,-1, 0,-1]`

would be converted to

`[3, 1, 3, 2,-7, 3,-1, 0, 7, 3,-6, 0, 4, 2,-1, 0, 0,-1]`.

This in conjunction with differential encoding compresses any sequence of repeated bytes. There is one caveat here, in that a single zero actually expands to two zeros (as seen at the end of the above sequence).

### Decompression

The compression process outlined above is lossless, meaning that it can be reversed. It is simple to see how the two encodings can be undone.

## Code Overview

### `CompressionUtils.scala`

This file contains software implementations of the compression functions. The function `compress(input: Seq[Byte], compress: Boolean)` is the main top-level compression function.

### `Compressor.scala`

This file contains the Chisel code for the compressor. It is broken into the following modules:

- `RunLengthCoder`
  - Accepts one byte at a time and performs run-length encoding/decoding.
  - Uses a 4-state machine to accept bytes and either produce output or not.
    - The `sStutter` state is used to send an additional byte when the end of a run is reached.
- `DifferentialCoder`
  - Accepts an entire beat of data at once, and encodes/decodes it.
  - Needs to be told when the last beat is, so that it can reset for the next string of data.
- `CREECDifferentialCoder`
  - Instantiates `DifferentialCoder`.
  - Deals with CREEC beats and passes them through the coder.
- `BasicFIFO`
  - Represents a basic and not-fully-functional FIFO queue.
  - This should be replaced with a Chisel `Queue`.
- `CREECRunLengthCoder`
  - Similarly to `CREECDifferentialCoder`, instantiates `RunLengthCoder`.
  - Deals with CREEC beats and passes them through the coder.
- `CREECCoder`
  - This is just a module that allows any of the CREEC-level modules to fit into a tester.
- `Compressor`
  - Hooks up a `CREECDifferentialCoder` and a `CREECRunLengthCoder` in the correct order with the correct operation modes to perform the full compression/decompression operation.

It also contains the software models for the three types of coding (differential, run-length, and both). These are `CREECDifferentialCoderModel`, `CREECRunLengthCoderModel`, and `CompressorModel`.

For testing details, see the test README.
