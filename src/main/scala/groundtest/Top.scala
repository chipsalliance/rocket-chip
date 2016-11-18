package groundtest

import Chisel._
import config._
import diplomacy._
import coreplex._
import rocketchip._

class GroundTestTop[+C <: GroundTestCoreplex](_coreplex: Parameters => C)(implicit p: Parameters) extends BaseTop(_coreplex)
    with DirectConnection
    with PeripheryMasterAXI4Mem
    with PeripheryTestRAM {
  override lazy val module = new GroundTestTopModule(this, () => new GroundTestTopBundle(this))
}

class GroundTestTopBundle[+L <: GroundTestTop[GroundTestCoreplex]](_outer: L) extends BaseTopBundle(_outer)
    with PeripheryMasterAXI4MemBundle
    with PeripheryTestRAMBundle {
  val success = Bool(OUTPUT)
}

class GroundTestTopModule[+L <: GroundTestTop[GroundTestCoreplex], +B <: GroundTestTopBundle[L]](_outer: L, _io: () => B) extends BaseTopModule(_outer, _io)
    with PeripheryMasterAXI4MemModule
    with PeripheryTestRAMModule {
  io.success := outer.coreplex.module.io.success
}
