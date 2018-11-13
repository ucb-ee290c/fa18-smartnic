# fa18-smartnic

[![Build Status](https://travis-ci.org/ucberkeley-ee290c/fa18-smartnic.svg?branch=master)](https://travis-ci.org/ucberkeley-ee290c/fa18-smartnic)

This repo will contain generic implementations of compression and encryption (AES2) blocks. These blocks will have generic interfaces to access the interfaces they need (interrupts, system memory, memory-mapped register space).

We will add shims from these implementations to `rocket-chip` and the block device controller in `testchipip` on a branch of `firesim/firechip`.

TODO: `add info about what branches we are using.`

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
At the top level of `fa18-smartnic` run `sbt compile` and `sbt test`.

## With Mill
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
