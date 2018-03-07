// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem.SubsystemClockCrossing
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
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

class RocketTile(
    val rocketParams: RocketTileParams,
    crossing: SubsystemClockCrossing)
  (implicit p: Parameters) extends BaseTile(rocketParams, crossing)(p)
    with HasExternalInterrupts
    with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with HasHellaCache
    with HasICacheFrontend {

  val intOutwardNode = IntIdentityNode()
  val slaveNode = TLIdentityNode()
  val masterNode = TLIdentityNode()

  val dtim_adapter = tileParams.dcache.flatMap { d => d.scratch.map(s =>
    LazyModule(new ScratchpadSlavePort(AddressSet(s, d.dataScratchpadBytes-1), xBytes, tileParams.core.useAtomics && !tileParams.core.useAtomicsOnlyForIO)))
  }
  dtim_adapter.foreach(lm => connectTLSlave(lm.node, xBytes))

  val bus_error_unit = tileParams.core.tileControlAddr map { a =>
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a)))
    intOutwardNode := beu.intNode
    connectTLSlave(beu.node, xBytes)
    beu
  }

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  def findScratchpadFromICache: Option[AddressSet] = dtim_adapter.map { s =>
    val finalNode = frontend.masterNode.edges.out.head.manager.managers.find(_.nodePath.last == s.node)
    require (finalNode.isDefined, "Could not find the scratch pad; not reachable via icache?")
    require (finalNode.get.address.size == 1, "Scratchpad address space was fragmented!")
    finalNode.get.address(0)
  }

  nDCachePorts += 1 /*core */ + (dtim_adapter.isDefined).toInt

  val dtimProperty = dtim_adapter.map(d => Map(
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

  override lazy val module = new RocketTileModuleImp(this)
}

class RocketTileModuleImp(outer: RocketTile) extends BaseTileModuleImp(outer)
    with HasLazyRoCCModule[RocketTile]
    with HasHellaCacheModule
    with HasICacheFrontendModule {

  val core = Module(p(BuildCore)(outer.p))

  val uncorrectable = RegInit(Bool(false))
  val halt_and_catch_fire = outer.rocketParams.hcfOnUncorrectable.option(IO(Bool(OUTPUT)))

  outer.bus_error_unit.foreach { lm =>
    lm.module.io.errors.dcache := outer.dcache.module.io.errors
    lm.module.io.errors.icache := outer.frontend.module.io.errors
  }

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector
  outer.bus_error_unit.foreach { beu => core.io.interrupts.buserror.get := beu.module.io.interrupt }
  core.io.hartid := constants.hartid // Pass through the hartid
  trace.foreach { _ := core.io.trace }
  halt_and_catch_fire.foreach { _ := uncorrectable }
  outer.frontend.module.io.cpu <> core.io.imem
  outer.frontend.module.io.reset_vector := constants.reset_vector
  outer.frontend.module.io.hartid := constants.hartid
  outer.dcache.module.io.hartid := constants.hartid
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  fpuOpt foreach { fpu => core.io.fpu <> fpu.io }
  core.io.ptw <> ptw.io.dpath
  roccCore.cmd <> core.io.rocc.cmd
  roccCore.exception := core.io.rocc.exception
  core.io.rocc.resp <> roccCore.resp
  core.io.rocc.busy := roccCore.busy
  core.io.rocc.interrupt := roccCore.interrupt

  // Rocket has higher priority to DTIM than other TileLink clients
  outer.dtim_adapter.foreach { lm => dcachePorts += lm.module.io.dmem }

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
