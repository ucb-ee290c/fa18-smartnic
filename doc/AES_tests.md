# AES test suite

This directory contains testing suites for the AES modules. They can be run with `sbt test` or with `sbt testOnly`.
All tests here utilize the PeekPokeTester. Except for `AESNISTTester`, all test suites are designed for
stimulating one test vector per run. Each *Tester is accompanied by a *Spec that helps setup test vectors and
informs sbt of the test.

## AESSubmoduleTesters

These tests run sanity checks on each of the original encryption submodules. Since PeekPokeTester is not designed to
setup multiple DUTs at once, some of these tests create extra modules that instantiate multiple submodules for testing.

Each of the tests hard-codes its test vectors at the time of testing and stimulates the DUT.
Reference values were generated and copied over from a verified Python model.
The outputs are all printed to the screen. At the time of writing, the user is expected to visually verify correctness.

Testers:
- `SubByteTester`: Sanity check for the SubBytes function
- `SubMMTester`: Sanity check for the Rijndael Matrix Multiply in the MixColumns function
- `SubRCONTester`: Validation for RCON generation
- `SubKeyExpansionTester`: Sanity check for a single stage of KeyExpansion
- `SubCipherTester`: Sanity check for a complete encryption cipher (SubBytes, ShiftRows, MixColumns, AddRoundKey)
- `SubStageTester`: Sanity check for the above cipher with KeyExpansion
- `SubKeyScheduleTimeInterleaveTester`: Sanity check for iterative key generation
- `CREECBusAESTest`: Sanity check for the software model used for CREECBus

Note that the `SubStageTester` does not have a reasonable analog in the decryption side, as all the round keys
must be generated before even the first decryption stage.

For module information, see [AES.md](AES.md)

TODO: add `expect` functionality

## AES128Tester

These tests run sanity checks on the complete encryption blocks. `AES128Spec` prepares one hard-coded set of test vectors
and applies it to all three DUTS. These tests include `expect` statements, so human inspection is not required.

Testers:
- `AES128CombinationalTester`: Sanity check for the purely combinational AES128Combinational
- `AES128Tester`: Sanity check for full iterative AES128, where rcon generation, key expansion, and cipher all live in one stage
- `AES128TimeInterleaveTester`: Sanity check for AES128 where key expansion is performed combinationally and the cipher is iterative

## InvAES128SubmoduleTesters

These tests run sanity checks on each of the decryption submodules that were not part of the encryption phase.
As in `AESSubmoduleTesters`, some of these tests create extra modules that setup multiple DUTs at once.

Each of the tests hard-codes its test vectors at the time of testing and stimulates the DUT.
Reference values were generated and copied over from a verified Python model.
The outputs are all printed to the screen. At the time of writing, the user is expected to visually verify correctness.

Testers:
- `InvSubByteTester`: Sanity check for the InvSubBytes function
- `InvSubMMTester`: Sanity check for the Rijndael Inverse Matrix Multiply in the InvMixColumns function
- `InvSubCipherTester`: Sanity check for a complete decryption decipher (InvSubBytes, InvShiftRows, InvMixColumns, AddRoundKey)

TODO: add `expect` functionality


## InvAES128Tester

These tests run sanity checks on the complete decryption blocks. `InvAES128Spec` prepares one hard-coded set of test vectors
and applies it to both DUTS. These tests include `expect` statements, so human inspection is not required.

Testers:
- `InvAES128CombinationalTester`: Sanity check for the purely combinational InvAES128Combinational
- `InvAES128Tester`: Sanity check for InvAES128 where key expansion is performed combinationally and the decipher is iterative

## AESTopTester

These tests validate top-level modules that combine key generation, encryption, and decryption. As with the
above tests, these tests are designed to verify one vector at a time. However, they are setup to run decryption and
encryption asynchronously when possible to demonstrated independent operation despite the shared key schedule.

- `AESTopCombinationalTester`: Purely combinational system
- `AESTopTimeInterleaveTester`: Combinational Key generation, iterative encryption and decryption
- `AESTopFullTimeInterleaveTester`: Pure iterative system

## AESNISTTester

This test uses the Known Answer Tests provided by [NIST](https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program/block-ciphers).
We use the Electronic Codebook AES-128 implementation and so use those tests. This provides 568 test cases.
The test verifies `AESTopFullTimeInterleave`, which is a combined encryption-decryption engine with iterative
encryption, decryption, and key-generation.

The test structure is similar to that of `AESTopFullTimeInterleaveTester.` `AESNISTSpec` reads in the NIST
files, parses them for the test parameters, and creates trial vectors. These are then passed to
`AESNISTFullTimeInterleaveTester`, which instantiates a `AESTopFullTimeInterleave` and stimulates it
with the test vectors.

A notable feature of the test is the byte-reordering in `AESNISTSpec`. This is done to accommodate endianess.
The NIST files provide vectors are byte-wise little-endian, but have the first byte in the leftmost (MSB) slot.
While the DUT expects a 128-bit value, the entire length is little-endian, so the first byte is in the rightmost (LSB) slot.

## CREECBusAESTest

This test suite validates the CREECBus software model and hardware wrappers for AES. It utilizes the test
drivers and monitors developed for CREECBus. Unlike in the above testers, no extra hardware tester classes
are necessary since the drivers and monitors are designed for CREECBus modules while the software model has
built-in functions for testing from their super class `SoftwareModel`.

- `CREECBusAESSWTest`: Verifies the encryption and decryption software models for both high and low level transactions.
Also runs a basic encrypt-decrypt chain to demonstrate block composition.
- `CREECBusAESHWTest`: Verifies the encryption and decryption RTL interfaces individually against the software models,
then runs a simple encrypt-decrypt chain.

