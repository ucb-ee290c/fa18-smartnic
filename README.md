# fa18-smartnic

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
At the top level of `fa18-smartnic` run `sbt compile` and `sbt test`.
