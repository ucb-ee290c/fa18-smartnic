# ECC Chisel Blocks

This directory contains the AES modules. They can be compile to FIRRTL with `sbt compile`, and compiled to Verilog with
`sbt run`.

These blocks are designed for 128-bit Electronic Codebook AES encryption. The proposed use case is
as part of hardware security in a storage system. The storage node will have a unique ID that may serve as
the encryption key.

Expanding to 192- or 256- encryption or utilizing another scheme (e.g. Cipher Blockchain) is relatively simple
given the current work.

Files:
- AESHelper.scala: Helper modules (e.g. hardware multiplies for Rijndael field) and shared modules (e.g. key generation)
- AES.scala: Modules for AES encryption
- InvAES.scala: Modules for AES decryption
- AESTop.scala: Full AES encryption and decryption modules
- AESApp.scala: Helper class to output Verilog.
- AESSWModel.scala: AES Interconnect Software Model for CREECBus


## Useful information
- At the top level, modules expect 128-bit UInt inputs for keys and data. Internally, they are
converted to 16-wide Vecs of bytes. This simplifies internal utilization and code readability. The LSB
of the 128-bit UInt corresponds to the first element in the Vec.
- We suggest using the `AESTopFullTimeInterleave`, as it has the best re-utilization of the provided implementation.
For increased throughput, we suggested instantiated multiple of these modules in parallel.


## Relevant Modules
Without delving too deeply into each module, the relevant "top" level modules are listed below.

### AES.scala
- `AES128Combinational`: Purely combinational encryption module. Only used for functional verification. AVOID implementing this.
- `AES128`: Full time interleaved encryption module. Uses a submodule that combines rcon generation, key expansion, and cipher.
            Uses a decoupled interface for data input and output.
- `AES128TimeInterleave`: Encryption module where key expansion is performed combinationally and the cipher is time interleaved. This is used to
                accomodate a corresponding decryption module, which cannot easily compute round keys and ciphers simulatenously. Note that the
                key generation is modular and can be substituted with a time-interleaved module, as is done below.
                Uses a decoupled interface for data input and output.

### InvAES.scala
- `InvAES128Combinational`: Purely combinational decryption module. Only used for functional verification. AVOID implementing this.
- `InvAES128`: Decryption module where key expansion is performed combinationally and the cipher is time interleaved.
        Uses a decoupled interface for data input and output.

### AESTop.scala
- `AESTopCombinational`: Purely combinational encryption and decryption module.
        Only used for functional verification. AVOID implementing this.
- `AESTopTimeInterleave`: Combinational Key generation, time-interleaved encryption and decryption
        Uses a decoupled interface for data input and output.
- `AESTopFullTimeInterleave`: Pure time-interleaved system
        Uses a decoupled interface for data input, output, and key input.


### AESSWModel.scala
Contains CREECEncryptModel and CREECDecryptModel, which consume CREECBus transaction and process them
using the Javax implementation of AES.
