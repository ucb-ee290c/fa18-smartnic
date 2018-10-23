# fa18-smartnic

This repo will contain generic implementations of compression and encryption (AES2) blocks. These blocks will have generic interfaces to access the interfaces they need (interrupts, system memory, memory-mapped register space).

We will add shims from these implementations to `rocket-chip` and the block device controller in `testchipip` on a branch of `firesim/firechip`.

TODO: `add info about what branches we are using.`

# Building
To get started, clone the latest [`chisel3`](https://github.com/freechipsproject/chisel3), [`chisel-testers`](https://github.com/freechipsproject/chisel-testers) repos and publish their JARs locally, so we can depend on SNAPSHOT versions.

Like this:

```
git clone git@github.com:freechipsproject/chisel3
cd chisel3
sbt compile
sbt publishLocal
```

# Testing
You should be able to run `sbt test`.
