# ECC Test suite

This directory contains the tests for Reed-Solomon encoding and decoding implemetations.

## File overview

- ECCSpec.scala: generates a test RS configuration *RS(n, k, symbolWidth)*. The user can change *numTrials* to produce as many test vectors (randomly generated) as wanted. Those test vectors first verify if the software implementations function properly, and are passed to the Chisel testers for RTL verification.

- ECCUnitTest.scala: contains unit tests for various ECC Chisel blocks such as **RSEncoder**, **RSDecoder**, **ECCEncoderTop**, and **ECCDecoderTop**. Those unit tests leverage Chisel PeekPoke tester to ensure that the encoder and decoder logic function correctly (i.e., hardware output matches software output provided by **ECCSpec**). Limited bus transaction is also tested (single header with single data beat) to ensure that the blocks meet minimal bus handling requirement.

- CREECBusECCTest.scala: utilizes advanced features from Chisel testers2 for testing the CREECBus-wrapper ECC Encoder and Decoder modules. The modules are tested with the high-level bus transaction modeling. Many aspects of bus handling are verified to ensure the block function correctly when putting into the context of full system integration with many data beats per bus transaction and many bus transactions in-flight -- which is challenging/tedious to model using PeekPoke tester.

## How to run

- For Unit tests

```
sbt 'testOnly ecc.ECCTester'
```

- For CREECBus tests

```
sbt 'testOnly CREECBusECCTest'
```

