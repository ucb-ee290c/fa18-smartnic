# Transaction Abstraction
In the pursuit of a unified SW testing and RTL stimulus/checking infrastructure, we created a transaction-level modeling framework in Scala for this project. The primary source can be found in `src/main/scala/TransactionModeling.scala`.

The abstract notion of a `Transaction` is just a chunk of data and control that a given block can process or produce.

We defined 2 levels of `Transaction` abstractions specific to the CREECBus.

`CREECHighLevelTransaction`s (HLT) consist of a chunk of `data` represented as a Scala `Seq[Byte]`. The high-level transaction is CREECBus `BusParams` agnostic and can even exist independently of the CREECBus (for instance it could just as easily drive a AXI4-Stream interface). As time went on, we began to add metadata fields to the HLT, and a constraint that the chunk of `data` must be 8B aligned, but this was a poor decision in hindsight as it made the HLT no longer bus agnostic. Most blocks in our CREEC project are modeled using HLTs, since they just operate on a chunk of bytes and spit out another chunk of bytes.

`CREECLowLevelTransaction`s (LLT) can be either a `CREECHeaderBeat` or `CREECDataBeat`, and they represent a single header or data beat respectively. LLTs are `BusParams` specific and they are designed to be driven or monitored on a specific instance of the CREECBus. They expose the inner details of the bus, and therefore closely resemble the `TransactionHeader` and `TransactionData` Bundles which make up the CREECBus Bundle. Only a few blocks are modeled using LLTs, such as the width converter and CREEC passthrough.

# Software Modeling
We defined a formal notion of a software model for our blocks in `TransactionModeling.scala` in the `SoftwareModel` abstract class. A `SoftwareModel` is type parameterized on the types of input and output transactions it can receive and send.

A concrete `SoftwareModel` is created for each block in our project. A concrete model implements the `process` function of `SoftwareModel` which takes one transaction of the `SoftwareModel`'s input type and returns a `Seq` of transactions of the `SoftwareModel`'s output type. This means for a given input transaction, and the current state of the model, none, one, or multiple output transactions may be produced on a call to `process`.

For a simple example, see the `CREECPassthroughModel` defined in `src/main/scala/interconnect/CREECPassthrough.scala`. The AES model is also easy to understand in `src/main/scala/aes/AESSWModel.scala`. It just takes a HLT, passes its data through the Java standard crypto library's AES function, and returns the encrypted data from a copy of the input HLT.

## Bridging from High -> Low or Low -> High
HLTs can be converted into LLTs using the `CREECHighToLowModel` which specializes a HLT for a specific CREECBus with specific BusParams and emits LLTs which can drive the physical bus. This permits the same HLTs to be used for testing the compressor and AES blocks (software and RTL) even though they use different BusParams.

```scala
val high2Low = new CREECHighToLowModel(BusParams.creec)
val out: Seq[CREECLowLevelTransaction] = high2Low.processTransactions(Seq(CREECHighLevelTransaction(Seq(1, 2, 3, 4, 5, 6, 7, 8), 0x0)))
```

LLTs can be converted back into HLTs (for example, from a RTL bus monitor) using the `CREECLowToHighModel`. In doing so, the LLTs are checked for consistency as they are converted, catching FSM mistakes like transmitting a data beat before transmitting its associated header beat. Top-level testing is all done using the HLT abstraction, so LLTs from RTL are converted into HLTs to be compared against the golden software model.

## Testing a Software Model
The software model is intended to be a golden reference for comparing the RTL model against. So the software model itself needs to be checked against hand-verified inputs and outputs. The AES SW model test (in `src/test/scala/aes/CREECBusAESTest.scala` is a great example of how to do this:

```scala
  val data = Seq(CREECHighLevelTransaction(Seq(
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 3, 3, 2,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 3, 3, 2
  ).map(_.asInstanceOf[Byte]), 0x0))

  val encryptedData = Seq(CREECHighLevelTransaction(Seq(
    0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
    0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c,
    0x14, 0x23, 0x6b, 0xd1, 0xce, 0x59, 0x26, 0xe1,
    0x38, 0xad, 0x15, 0x85, 0x82, 0xd4, 0x5c, 0x3c
  ).map(_.asInstanceOf[Byte]), 0x0, encrypted = true))

  "AESSWModel" should "encrypt high" in {
    val aesEncryptModel = new CREECEncryptHighModel
    val out = aesEncryptModel.processTransactions(data)
    assert(out == encryptedData)
  }
```

We construct reference input and output transactions by hand for the software model, then instantiate the model and call `processTransactions` on it with the input transaction sequence. The output transactions are returned and `assert` checks that they match what we expect.

## Composing Software Models
Since we defined a formalism for software models, composing them is easy. A great example is in `/src/test/scala/CREECeleratorSWTest.scala`. For instance, we can compose a compressor and decompressor model and check that they form an identity transform:

```scala
val compressionLoop =
  new CompressorModel(compress = true) ->
  new CompressorModel(compress = false)
val out = compressionLoop.processTransactions(testTx)
assert(out == testTx)
```

The `model1 -> model2` operator is equivalent to `model1.compose(model2)`

# RTL Model Testing

## CREECAgent Testing Infrastructure

# Looking Forward
