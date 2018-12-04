package aes

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util.DontTouch

class ExampleTopModule[+L <: ExampleTop](l: L) extends RocketSubsystemModuleImp(l)
  with HasRTCModuleImp
  with CanHaveMasterAXI4MemPortModuleImp
  with HasPeripheryBootROMModuleImp
  with HasExtInterruptsModuleImp
  with DontTouch

class ExampleTop(implicit p: Parameters) extends RocketSubsystem
    with CanHaveMasterAXI4MemPort
    with HasPeripheryBootROM
    with HasSyncExtInterrupts {
  override lazy val module = new ExampleTopModule(this)
}

//Mix in for AES
trait HasPeripheryAESEncrypt extends BaseSubsystem {
  // instantiate cordic chain
  val aesChain = LazyModule(new AESThing(queuedepth=8))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("aesWrite")) {
    aesChain.writeQueue.mem.get
  }
  pbus.toVariableWidthSlave(Some("aesRead")) {
    aesChain.readQueue.mem.get
  }
}

class ExampleTopWithAES(implicit p: Parameters) extends ExampleTop
    // mix in aes
    with HasPeripheryAESEncrypt {
  override lazy val module = new ExampleTopModule(this)
}
