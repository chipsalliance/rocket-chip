// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package rocket

import Chisel._
import config._
import coreplex._
import diplomacy._
import uncore.converters._
import uncore.devices._
import uncore.tilelink2._
import util._

class RocketTile(val c: RocketConfig)(implicit p: Parameters) extends BaseTile()(p)
    with CanHaveLegacyRoccs  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with CanHaveScratchpad { // implies CanHavePTW with HasHellaCache with HasICacheFrontend

  nDCachePorts += 1 // core TODO dcachePorts += () => module.core.io.dmem ??

  override lazy val module = new RocketTileModule(this)
}

class RocketTileBundle(outer: RocketTile) extends BaseTileBundle(outer)
    with CanHaveScratchpadBundle {
  val hartid = UInt(INPUT, p(XLen))
  val interrupts = new TileInterrupts()(p).asInput
  val resetVector = UInt(INPUT, p(XLen))
}

class RocketTileModule(outer: RocketTile) extends BaseTileModule(outer, () => new RocketTileBundle(outer))
    with CanHaveLegacyRoccsModule
    with CanHaveScratchpadModule {

  val core = Module(new Rocket(outer.c)(outer.p))
  core.io.interrupts := io.interrupts
  core.io.hartid := io.hartid
  outer.frontend.module.io.cpu <> core.io.imem
  outer.frontend.module.io.resetVector := io.resetVector
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  fpuOpt foreach { fpu => core.io.fpu <> fpu.io }
  ptwOpt foreach { ptw => core.io.ptw <> ptw.io.dpath }
  outer.legacyRocc foreach { lr =>
    lr.module.io.core.cmd <> core.io.rocc.cmd
    lr.module.io.core.exception := core.io.rocc.exception
    core.io.rocc.resp <> lr.module.io.core.resp
    core.io.rocc.busy := lr.module.io.core.busy
    core.io.rocc.interrupt := lr.module.io.core.interrupt
  }

  // TODO figure out how to move the below into their respective mix-ins
  require(dcachePorts.size == core.dcacheArbPorts)
  dcacheArb.io.requestor <> dcachePorts
  ptwOpt foreach { ptw => ptw.io.requestor <> ptwPorts }
  fpuOpt foreach { fpu =>
    outer.legacyRocc.orElse {
      fpu.io.cp_req.valid := Bool(false)
      fpu.io.cp_resp.ready := Bool(false)
      None
    } foreach { lr =>
      fpu.io.cp_req <> lr.module.io.fpu.cp_req
      fpu.io.cp_resp <> lr.module.io.fpu.cp_resp
    }
  }
}

// TODO make this into a generic wrapper around CoreConfig => Tile?
class AsyncRocketTile(c: RocketConfig)(implicit p: Parameters) extends LazyModule {
  val rocket = LazyModule(new RocketTile(c))

  val masterNodes = rocket.masterNodes.map(_ => TLAsyncOutputNode())
  val slaveNode = rocket.slaveNode.map(_ => TLAsyncInputNode())

  (rocket.masterNodes zip masterNodes) foreach { case (r,n) => n := TLAsyncCrossingSource()(r) }
  (rocket.slaveNode zip slaveNode) foreach { case (r,n) => r := TLAsyncCrossingSink()(n) }

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val masters = masterNodes.map(_.bundleOut)
      val slave = slaveNode.map(_.bundleIn)
      val hartid = UInt(INPUT, p(XLen))
      val interrupts = new TileInterrupts()(p).asInput
      val resetVector = UInt(INPUT, p(XLen))
    }
    rocket.module.io.interrupts := ShiftRegister(io.interrupts, 3)
    // signals that do not change:
    rocket.module.io.hartid := io.hartid
    rocket.module.io.resetVector := io.resetVector
  }
}
