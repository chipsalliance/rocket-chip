// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

case class RocketTileParams(
    core: RocketCoreParams = RocketCoreParams(),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    rocc: Seq[RoCCParams] = Nil,
    btb: Option[BTBParams] = Some(BTBParams()),
    dataScratchpadBytes: Int = 0,
    trace: Boolean = false,
    hcfOnUncorrectable: Boolean = false,
    name: Option[String] = Some("tile"),
    hartId: Int = 0,
    blockerCtrlAddr: Option[BigInt] = None,
    boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
    ) extends TileParams {
  require(icache.isDefined)
  require(dcache.isDefined)
}

class RocketTile(val rocketParams: RocketTileParams)(implicit p: Parameters) extends BaseTile(rocketParams)(p)
    with HasExternalInterrupts
    with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with CanHaveScratchpad { // implies CanHavePTW with HasHellaCache with HasICacheFrontend

  nDCachePorts += 1 // core TODO dcachePorts += () => module.core.io.dmem ??

  val dtimProperty = scratch.map(d => Map(
    "sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val itimProperty = tileParams.icache.flatMap(_.itimAddr.map(i => Map(
    "sifive,itim" -> frontend.icache.device.asProperty))).getOrElse(Nil)

  val cpuDevice = new Device {
    def describe(resources: ResourceBindings): Description =
      toDescription(resources)("sifive,rocket0", dtimProperty ++ itimProperty)
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceInt(BigInt(hartId)))
  }

  override lazy val module = new RocketTileModule(this)
}

class RocketTileBundle(outer: RocketTile) extends BaseTileBundle(outer)
    with CanHaltAndCatchFire {
  val halt_and_catch_fire = outer.rocketParams.hcfOnUncorrectable.option(Bool(OUTPUT))
}

class RocketTileModule(outer: RocketTile) extends BaseTileModule(outer, () => new RocketTileBundle(outer))
    with HasExternalInterruptsModule
    with HasLazyRoCCModule
    with CanHaveScratchpadModule {

  val core = Module(p(BuildCore)(outer.p))
  val uncorrectable = RegInit(Bool(false))

  decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector
  outer.busErrorUnit.foreach { beu => core.io.interrupts.buserror.get := beu.module.io.interrupt }
  core.io.hartid := io.hartid // Pass through the hartid
  io.trace.foreach { _ := core.io.trace }
  io.halt_and_catch_fire.foreach { _ := uncorrectable }
  outer.frontend.module.io.cpu <> core.io.imem
  outer.frontend.module.io.reset_vector := io.reset_vector
  outer.frontend.module.io.hartid := io.hartid
  outer.dcache.module.io.hartid := io.hartid
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  fpuOpt foreach { fpu => core.io.fpu <> fpu.io }
  core.io.ptw <> ptw.io.dpath
  roccCore.cmd <> core.io.rocc.cmd
  roccCore.exception := core.io.rocc.exception
  core.io.rocc.resp <> roccCore.resp
  core.io.rocc.busy := roccCore.busy
  core.io.rocc.interrupt := roccCore.interrupt

  when(!uncorrectable) { uncorrectable :=
    List(outer.frontend.module.io.errors, outer.dcache.module.io.errors)
      .flatMap { e => e.uncorrectable.map(_.valid) }
      .reduceOption(_||_)
      .getOrElse(false.B)
  }

  // TODO eliminate this redundancy
  val h = dcachePorts.size
  val c = core.dcacheArbPorts
  val o = outer.nDCachePorts
  require(h == c, s"port list size was $h, core expected $c")
  require(h == o, s"port list size was $h, outer counted $o")
  // TODO figure out how to move the below into their respective mix-ins
  dcacheArb.io.requestor <> dcachePorts
  ptw.io.requestor <> ptwPorts
}

class RocketTileWrapperBundle[+L <: RocketTileWrapper](_outer: L) extends BaseTileBundle(_outer)
    with CanHaltAndCatchFire {
  val halt_and_catch_fire = _outer.rocket.module.io.halt_and_catch_fire.map(_.cloneType)
}

class RocketTileWrapper(
    params: RocketTileParams,
    val crossing: CoreplexClockCrossing)
    (implicit p: Parameters) extends BaseTile(params) with HasCrossing {

  val rocket = LazyModule(new RocketTile(params))

  // The buffers needed to cut feed-through paths are microarchitecture specific, so belong here
  val masterBuffer = LazyModule(new TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1)))
  val masterNode = crossing match {
    case _: AsynchronousCrossing => rocket.masterNode
    case SynchronousCrossing(b) =>
      require (!params.boundaryBuffers || (b.depth >= 1 && !b.flow && !b.pipe), "Buffer misconfiguration creates feed-through paths")
      rocket.masterNode
    case RationalCrossing(dir) =>
      require (dir != SlowToFast, "Misconfiguration? Core slower than fabric")
      if (params.boundaryBuffers) {
        masterBuffer.node :=* rocket.masterNode
      } else {
        rocket.masterNode
      }
  }

  val slaveBuffer  = LazyModule(new TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none))
  val slaveNode: TLInwardNode = crossing match {
    case _: SynchronousCrossing  => rocket.slaveNode // requirement already checked
    case _: AsynchronousCrossing => rocket.slaveNode
    case _: RationalCrossing =>
      if (params.boundaryBuffers) {
        DisableMonitors { implicit p => rocket.slaveNode :*= slaveBuffer.node }
      } else {
        rocket.slaveNode
      }
  }

  rocket.intInwardNode := intXbar.intnode
  val intInwardNode = intXbar.intnode
  val intOutwardNode = rocket.intOutwardNode

  override lazy val module = new BaseTileModule(this, () => new RocketTileWrapperBundle(this)) {
    // signals that do not change based on crossing type:
    rocket.module.io.hartid := io.hartid
    rocket.module.io.reset_vector := io.reset_vector
    io.trace.foreach { _ := rocket.module.io.trace.get }
    io.halt_and_catch_fire.foreach { _ := rocket.module.io.halt_and_catch_fire.get }
  }
}
