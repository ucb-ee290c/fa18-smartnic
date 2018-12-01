# CREECBus Interconnect
For this project, we defined a custom bus interface in `src/main/scala/interconnect/CREECBus.scala`. The bus was designed based on the `BlockDeviceIO` Bundle in `ucb-bar/testchipip/src/main/scala/BlockDevice.scala`, and was intended to be driven directly from the output of the BlockDevice RTL attached to rocket-chip. The bus is also bridge-able to other memory mapped buses like AXI4 and TL-H, and streaming buses like AXI4-Stream.

## Bus Specification (IOs)
The CREECBus is a unidirectional bus (master -> slave communication only, no responses from slave -> master) which has Decoupled header and data channels.

The header bundle contains the length `len` and id `id` of transactions sent on the data channel. The header also contains a bunch of metadata fields which each block in our pipeline can modify and pass to the next block. One Decoupled header transfer is referred to as a header beat.

The data Decoupled bundle contains a `data` field and an `id` field. The width of the `data` field is parameterizable. One Decoupled data transfer is referred to as a data beat.

To send a transaction on the CREECBus, the master must first supply a header beat to the slave with a transaction `id` which isn't already in flight, and with a `len` to indicate the number of data beats that this transaction **contains minus one** (-1 to match the AMBA buses' `len` behavior, and to save 1 bit). The cycle after the header has been accepted by the slave, the master can begin sending data beats. The CREECBus permits the master to send data beats with any ID for which the respective header has already been accepted by the slave. In other words, write interleaving between different transactions is permitted, but the slave may choose to force single-threaded behavior by blocking the header Decoupled channel until all the data associated with the single accepted header beat has been received.

Each block in our CREEC pipeline has a CREECBus input port on which it receives transactions, and a CREECBus output port to which it sends processed transactions. A block may modify the `data`, `len`, or any of the header metadata fields as a transaction passes through it.

## Bus Parameters
`case class BusParams(maxBeats: Int, maxInFlight: Int, dataWidth: Int)`
The CREECBus is parameterizable on `maxBeats`, the maximum data beats one transaction may carry; `maxInFlight`, the number of different transactions that may be in flight from a master to a slave with different IDs; and `dataWidth` which is the width of the `data` field in the data Decoupled channel.

# Passthrough Toy Example
A simple Chisel Module that uses the CREECBus can be found in `src/main/scala/interconnect/CREECPassthrough.scala`. It just passes through CREECBus transactions from input to output with 1 cycle delay, and with a `+1` operation on the `data` field in the data channel.

A golden software model for this passthrough can be found below the Module implementation. The details of software transaction-level modeling can be found in the `Transaction_Level_Modeling` document.

# Bus Components (Padder/Stripper, Width Converter)
The `CREECPadder` and `CREECStripper` are hack bus components which are used to cope with bridging a 64-bit wide CREECBus to the AES block which only works with a 128-bit wide CREECBus (to match the AES block size).

The `CREECPadder` component pads the `data` of CREECBus transactions to multiples of 16 bytes as they pass through it. The number of bytes that were added to the transaction is written to the `encryptionPadBytes` header field when the transaction is output from the padder.

The `CREECStripper` component works in reverse. It looks at the `encryptionPadBytes` header field of an incoming transaction and strips that number of bytes from the `data` as it passes through the stripper.

The `CREECWidthConverter` bridges 2 CREECBus'es of different data widths so long as they divide evenly. The width converter will throw an assertion if it receives an unaligned transaction when expanding the data width. For example, a width converter is converting from a 64-bit to 128-bit data bus, an input transaction with `len = 2` (3 data beats), will cause an assertion to be thrown because 64\*3=192 doesn't evenly divide by 128.

## Parameters
The `CREECPadder` takes `BusParams` which defines the bus parameters of its input and output CREECBus ports, and `padBytes` which tells the block to pad the output data to a multiple of `padBytes` (adding as many data beats to the output transaction as is necessary).

The `CREECStripper` takes the same `BusParams` parameter, and implicitly knows how many bytes to strip from each transaction by looking at the header's `encryptionPadBytes` field (yes, this is a hack).

The `CREECWidthConverter` takes 2 `BusParams` parameters which define the params `(dataWidth, maxBeats, maxInFlight)` of the input and output bus.

## IOs
All blocks have typical IO consisting of an input (`in` or `slave`) CREECBus and an output (`out` or `master`) CREECBus.

## Tests
The padder, stripper, width converter, and passthrough modules all have unit tests in `src/test/scala/interconnect`
