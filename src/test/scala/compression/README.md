# Compression Test Suite

## Unit Tests

`CompressionUnitTest.scala` contains peek-poke tests of the compression hardware blocks. `DifferentialCoderTester` and `RunLengthCoderTester` test the non-CREEC blocks, `BasicFIFOTester` tests the FIFO, and `CREECCoderTester` is a generic tester for everything on the CREEC bus.

## Integration Tests

`CREECCompressionModuleTester.scala` contains the model tests at the transaction modelling test level, and `CREECCompressionModelTester.scala` contains the module tests. The model tester verifies hand-written input with hand-written output, while the module tester compares the module output with that of the model. These tests use Testers2 and the Driver/Monitor `CREECAgent` testing system.
