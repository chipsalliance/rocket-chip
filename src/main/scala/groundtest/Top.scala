// See LICENSE.SiFive for license details.

package groundtest

import Chisel._
import config._
import diplomacy._
import coreplex._
import rocketchip._

class GroundTestTop(implicit p: Parameters) extends BaseTop
    with PeripheryMasterAXI4Mem
    with PeripheryTestRAM {
  override lazy val module = new GroundTestTopModule(this, () => new GroundTestTopBundle(this))

  val coreplex = LazyModule(new GroundTestCoreplex)

  socBus.node := coreplex.mmio
  coreplex.mmioInt := intBus.intnode
  mem.foreach { _ := coreplex.mem }
}

class GroundTestTopBundle[+L <: GroundTestTop](_outer: L) extends BaseTopBundle(_outer)
    with PeripheryMasterAXI4MemBundle
    with PeripheryTestRAMBundle {
  val success = Bool(OUTPUT)
}

class GroundTestTopModule[+L <: GroundTestTop, +B <: GroundTestTopBundle[L]](_outer: L, _io: () => B) extends BaseTopModule(_outer, _io)
    with PeripheryMasterAXI4MemModule
    with PeripheryTestRAMModule {
  io.success := outer.coreplex.module.io.success
}
