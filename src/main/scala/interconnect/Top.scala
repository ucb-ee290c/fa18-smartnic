package interconnect

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util.DontTouch

class ExampleTop(implicit p: Parameters) extends RocketSubsystem
    with CanHaveMasterAXI4MemPort
    with HasPeripheryBootROM
    with HasSyncExtInterrupts {
  override lazy val module = new ExampleTopModule(this)
}

class ExampleTopModule[+L <: ExampleTop](l: L) extends RocketSubsystemModuleImp(l)
    with HasRTCModuleImp
    with CanHaveMasterAXI4MemPortModuleImp
    with HasPeripheryBootROMModuleImp
    with HasExtInterruptsModuleImp
    with DontTouch

class ExampleTopWithCREECelerator(implicit p: Parameters) extends ExampleTop
    // mix in CREECelerator
    with HasPeripheryCREECelerator {
  override lazy val module = new ExampleTopModule(this)
}

class ExampleTopWithCREECeleratorRead(implicit p: Parameters) extends ExampleTop
    // mix in CREECeleratorRead
    with HasPeripheryCREECeleratorRead {
  override lazy val module = new ExampleTopModule(this)
}
