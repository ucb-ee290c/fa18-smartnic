# CREECelerator

[![Build Status](https://travis-ci.org/ucberkeley-ee290c/fa18-smartnic.svg?branch=master)](https://travis-ci.org/ucberkeley-ee290c/fa18-smartnic)

CREECelerator is a collection of composable hardware accelerators for compression, AES encryption, and Reed-Solomon codes (ECC)
implemented in Chisel. The accelerators can be arbitrarily composed and can interface with a host to provide both OS-invisible
or filesystem-aware storage acceleration capabilities.

This repo will contain generic implementations of compression (RLE), encryption (AES), and error correction coding (Reed Solomon) blocks.
They are integrated together using a simple custom bus called CREECBus. This pipeline is then integrated with Rocket-chip based on the
ucb-bar [project template](https://github.com/ucb-bar/project-template).


<p align="center">
  <img src="doc/img/creeceleratortop.png" width="460" >
</p>

<p align="center">
  <img src="doc/img/Rocket-top.PNG" width="250" >
</p>

Support for transaction level modeling and verification is a key component in CREECBus.

Synthesis has been tested for the Xilinx VC707 using [Hammer](https://github.com/ucb-bar/hammer).

## Project Background
This project was completed by Bob Zhou, Kyle Kovacs, Vighnesh Iyer, and Tan Nguyen for the Fall 2018 offering of
EE290C at UC Berkeley. The goal of this class was to implement various hardware processors using Chisel generators
for agile system-on-a-chip design. Our project focuses on hardware acceleration of data storage enhancements and modeling those
blocks for design exploration.

# Initial Setup
```
git clone git@github.com:ucberkeley-ee290c/fa18-smartnic
cd fa18-smartnic

git submodule update --init --recursive

# Clean local ivy package cache of chisel/firrtl JARs
cd ~/.ivy2/local/edu.berkeley.cs
rm -rf firrtl*
rm -rf chisel*

cd firrtl
sbt
sbt:firrtl> compile
sbt:firrtl> +publishLocal
sbt:firrtl> exit

cd ../chisel3
sbt
sbt:chisel3> compile
sbt:chisel3> +publishLocal
sbt:chisel3> exit
```

# Building / Testing
## With SBT
At the top level of `fa18-smartnic`, run `sbt compile` and `sbt test`.
You can also use `sbt testOnly` to run specific tests.
Many of our full pipeline tests require large amounts of memory (> 1GB). We recommend setting SBT's memory limit to
2GB or more using`sbt -mem 2048`.


## Integration testing
The CREECelerator pipeline with Rocketchip can be copmiled to Verilator-based executables.
They can be made using the Makefile in the `verisim` directory. Running `make debug` will produce executables that can provide waveform dumps.
To build the write pipeline, run `make MODEL=TestHarnessWrite`. To build the read pipeline, run `make MODEL=TestHarnessRead`.

Basic C tests can be found in the `tests` directory. They can be compiled to `riscv` test stimuli using make from the `tests` directory,
which can then be run using the generated executables. Compilation requires a shell path to [riscv-tools](https://github.com/riscv/riscv-tools),
which is included in the project template described above.
`creec.riscv` is a test for the write pipeline. `creec_decrypt.riscv` is a test for the read pipeline.

## Synthesis using Hammer
[Hammer](https://github.com/ucb-bar/hammer) setup files exist as a submodule in this project.

Setup the path to Vivado installation on your machine in the file `config/vivado_setup.sh`.

To change the clock constraint (or add new constraint), modify the file `config/constraint.xdc`. Note that we do not use `config/clocks.json`.

To select an FPGA, use one of the YAML files in the `config` folder. For example, to use the VC707 FPGA, use "VC707" as `FPGANAME` in the run command.

To synthesize a Verilog file with Hammer (basically Vivado synthesis for now), use the following command:

```
./runhammer.sh {path/to/verilog_file.v} {FPGANAME}
```

The timing and area report will be generated inside the directory `out_{verilog_top_module}_{FPGANAME}`.


# Modules Details

### Compression
Module files are in `src/main/scala/compression`. Tests are in `src/test/scala/compression`.

[Module Details](doc/compression.md)

[Test Details](doc/compression_tests.md)

### AES Encryption
Module files are in `src/main/scala/aes`. Tests are in `src/test/scala/aes`.

[Module Details](doc/AES.md)

[Test Details](doc/AES.md)

### ECC (Reed-Solomon)
Module files are in `src/main/scala/ecc`. Tests are in `src/test/scala/ecc`.

[Module Details](doc/ECC.md)

[Test Details](doc/ECC_tests.md)

### CREECBus Interconnect and Modeling
Module files are in `src/main/scala/interconnect`. Tests are in `src/test/scala/interconnect`.

[CREECBus Interconnect Details](doc/Interconnect_and_Bus_Components.md)

[CREECelerator Details](doc/Top_Level_Integration.md)

[Transaction Modeling Details](doc/Transaction_Level_Modeling.md)



# Supplamental
## Building and Testing With Mill
This project can be compiled and tested with [mill](https://github.com/lihaoyi/mill) instead of sbt with a few caveats.

### Installation
You still need to use sbt to `+publishLocal` the firrtl and chisel3 submodules first. This is only done once.

I'm using mill version 0.3.4. Install with:
```
sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L https://github.com/lihaoyi/mill/releases/download/0.3.4/0.3.4) > /usr/local/bin/mill && chmod +x /usr/local/bin/mill'
```

### Building / Testing
You can ask mill what tasks it can perform with `mill resolve _`, then `mill resolve creec._`. Try running `mill creec.compile`.

Notice that `creec` can be cross-built to scala 2.12.4 or 2.11.12. Building for scala 2.12 is the default. You can make this explicit with `mill creec[2.12.4].compile`.

Mill can watch for file changes for running any task and update if any changes are seen. `mill -w creec[2.12.4].compile`.

Testing is a bit of an issue with mill. Scalatest doesn't support providing a specific test suite or test to run from its test runner main class, and instead relies on some special hooks in sbt.

As a result, it is preferred to use `uTest` to run tests using mill. See `src/test/scala/interconnect/CREECPassthroughTest.scala` for an example.

To run all `uTest` tests use `mill creec[2.12.4].test`, and to run a specific test use `mill creec[2.12.4].test interconnect.CREECPassthroughTest.produceOutput`.

You can use the `-w` watch command for tests as well.