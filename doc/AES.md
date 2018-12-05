# AES Chisel Blocks

This directory contains the AES modules. They can be compiled to FIRRTL with `sbt compile`, and compiled to Verilog with
`sbt run`.

These blocks are designed for 128-bit Electronic Codebook AES encryption. The proposed use case is
as part of hardware security in a storage system. The storage node will have a unique ID that may serve as
the encryption key.

Expanding to 192- or 256- encryption or utilizing another operating mode (e.g. Cipher Blockchain) is relatively simple
given the current work.

Files:
- AESHelper.scala: Helper modules (e.g. hardware multiplies for Rijndael field) and shared modules (e.g. key generation)
- AES.scala: Modules for AES encryption
- InvAES.scala: Modules for AES decryption
- AESTop.scala: Full AES encryption and decryption modules, and hardware wrapper for [CREECBus](Interconnect_and Bus_Components.md)
- AESApp.scala: Helper class to output Verilog
- AESSWModel.scala: AES Interconnect Software Model for CREECBus

For test information, see [AES_tests.md](AES_tests.md)


## Useful information
- At the top level, modules expect 128-bit UInt inputs for keys and data. Internally, they are
converted to 16-wide Vecs of bytes. This simplifies internal utilization and code readability. The LSB
of the 128-bit UInt corresponds to the first element in the Vec.
- Most top level blocks are implemented with decoupled (ready-valid) interfaces.
- We suggest using the `AESTopFullTimeInterleave` as the top level module, as it has the best re-utilization of the provided implementation.
For increased throughput, we suggested instantiated multiple of these modules in parallel.
- At the time of writing, no major AES blocks have design parameters, as they were not necessary.
For future design exploration, potential parameters may include the number of modules in parallel, or the number
of iterative stages for SubBytes or the entire cipher stage.
- While many blocks are iterative, "time-interleave" refers to the suggestion of creating parallel instances to handle
multiple beats in flight.


## Relevant Modules
Without delving too deeply into each module, the relevant major modules and their IO are listed below.

### AESHelper.scala
- `HWKey`: Mix-in to establish a "hardware" key at the top level. Use in `AESTopCREECBus` RTL and in verification.
- `KeyScheduleTimeInterleave`: Iterative key schedule generation. Uses a decoupled interface for key input.
    - `key_in`: UInt(128), top input key with a decoupled interface
    - `key_schedule`: Vec(10, UInt(128)), registered key schedule output, ordered 0-9
    - `key_valid`: Bool, states that the key schedule is valid, used to enable cipher and decipher
- Various shared IO bundle definitions are included here.

### AES.scala
- `AES128Combinational`: Purely combinational encryption module. Only used for functional verification. AVOID implementing this.
- `AES128`: Full time iterative encryption module. Uses a submodule that combines rcon generation, key expansion, and cipher.
            Uses a decoupled interface for data input and output.
    - `data_in`: UInt(128), top plain data in with a decoupled interface
    - `data_out`:UInt(128), top encrypted data out with a decoupled interface
    - `key_in`: UInt(128), top input key
- `AES128TimeInterleave`: Encryption module where key expansion is performed combinationally and the cipher is iteratively. This is used to
                accomodate a corresponding decryption module, which cannot easily compute round keys and ciphers simulatenously. Note that the
                key generation is modular and can be substituted with an iterative implementation, as is done below.
                Uses a decoupled interface for data input and output.
    - `data_in`: UInt(128), top plain data in with a decoupled interface
    - `data_out`:UInt(128), top encrypted data out with a decoupled interface
    - `key_in`: UInt(128), top input key

### InvAES.scala
- `InvAES128Combinational`: Purely combinational decryption module. Only used for functional verification. AVOID implementing this.
- `InvAES128`: Decryption module where key expansion is performed combinationally and the cipher is iterative.
        Uses a decoupled interface for data input and output.
    - `data_in`: UInt(128), top encrypted data in with a decoupled interface
    - `data_out`:UInt(128), top decrypted (plain) data out with a decoupled interface
    - `key_in`: UInt(128), top input key

### AESTop.scala
- `AESTopCombinational`: Purely combinational encryption and decryption module.
        Only used for functional verification. AVOID implementing this.
- `AESTopTimeInterleave`: Combinational Key generation, iterative encryption and decryption
        Uses a decoupled interface for data input and output.
    - `encrypt_data_in`: UInt(128), top plain data in with a decoupled interface
    - `encrypt_data_out`:UInt(128), top encrypted data out with a decoupled interface
    - `decrypt_data_in`: UInt(128), top encrypted data in with a decoupled interface
    - `decrypt_data_out`:UInt(128), top decrypted (plain) data out with a decoupled interface
    - `key_in`: UInt(128), top input key
- `AESTopFullTimeInterleave`: Pure iterative system
        Uses a decoupled interface for data input, output, and key input.
    - `encrypt_data_in`: UInt(128), top plain data in with a decoupled interface
    - `encrypt_data_out`:UInt(128), top encrypted data out with a decoupled interface
    - `decrypt_data_in`: UInt(128), top encrypted data in with a decoupled interface
    - `decrypt_data_out`:UInt(128), top decrypted (plain) data out with a decoupled interface
    - `key_in`: UInt(128), top input key with a decoupled interface
- `AESTopCREECBus`: `AESTopFullTimeInterleave` wrapped for CREECBus.
        Uses an auxilary state machine for encrypt and decrypt interfaces to consume
        CREECBus high level transactions.
    - `encrypt_slave`: CREECBus, top encrypt bus input for write path
    - `encrypt_master`:CREECBus, top encrypt bus output for write path
    - `decrypt_slave`: CREECBus, top decrypt bus input for read path
    - `decrypt_master`:CREECBus, top decrypt bus output for read path
    - Above CREECBuses consume `AESBusParams extends BusParams` as a parameter. The key requirement here is `dataWidth=128`.
    - Hardware key is mixed in using `HW_Key`

### AESSWModel.scala
Contains CREECEncryptLowModel, CREECDecryptLowModel, CREECEncryptHighModel, and CREECDecryptHighModel.
These consume CREECBus high or low level transactions and process them
using the Javax implementation of AES. These models consume a set of parameters `AESBusParams`, which is described above.

CREECBus software models only require an overriden `process` function that defines how
high or low level transactions are consumed. The remaining functions for running tests are
inherited from the `SoftwareModel` class.

