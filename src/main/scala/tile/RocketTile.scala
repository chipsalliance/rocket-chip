// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

case class RocketTileParams(
    core: RocketCoreParams = RocketCoreParams(),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
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
    crossing: ClockCrossingType)
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

  nDCachePorts += 1 /*core */ + (dtim_adapter.isDefined).toInt

  val dtimProperty = dtim_adapter.map(d => Map(
    "sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val itimProperty = tileParams.icache.flatMap(_.itimAddr.map(i => Map(
    "sifive,itim" -> frontend.icache.device.asProperty))).getOrElse(Nil)

  val cpuDevice = new SimpleDevice("cpu", Seq("sifive,rocket0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty ++ tileProperties ++ dtimProperty ++ itimProperty)
    }

    override def getOMComponents(resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
      val cores = getOMRocketCores(resourceBindingsMap)
      val icache = getOMICacheFromBindings(resourceBindingsMap)
      cores ++: icache
    }

    def getOMICacheFromBindings(resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
      frontend.icache.device.getOMComponents(resourceBindingsMap)
    }

    def getOMRocketCores(resourceBindingsMap: ResourceBindingsMap): Seq[OMRocketCore] = { // TODO use resourceBindingsMap: ResourceBindingsMap?
      val coreParams = rocketParams.core

      val perfMon = if (coreParams.haveBasicCounters || coreParams.nPerfCounters > 0) {
        Some(OMPerformanceMonitor(
          specifications = List[OMSpecification](PrivilegedArchitectureExtensions.specVersion(MachineLevelISA, "1.10")),
          hasBasicCounters = coreParams.haveBasicCounters,
          nAdditionalCounters = coreParams.nPerfCounters
        ))
      }
      else { None }

      val pmp = if (coreParams.pmpGranularity > 0 || coreParams.nPMPs > 0) {
        Some(OMPMP(
          specifications = List[OMSpecification](PrivilegedArchitectureExtensions.specVersion(MachineLevelISA, "1.10")),
          nRegions = coreParams.nPMPs,
          granularity = coreParams.pmpGranularity
        ))
      }
      else { None }

      // TODO val mulDiv = coreParams.mulDiv.map{ md => MulDiv.makeOMI(md, xLen)}

      val baseInstructionSet = xLen match {
        case 32 => if (XLen == 32) RV32E else RV32I // TODO coreParams.useRVE
        case 64 => RV64I
        case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen: $xLen")
      }

      val isaExtSpec = ISAExtensions.specVersion _
      val baseSpec = BaseExtensions.specVersion _

      val baseISAVersion = "" // TODO This func is in the om-scala-rocket branch ISAExtensions.baseISASpecification(baseInstructionSet)

      val d = coreParams.fpu.filter(_.fLen > 32).map(x => isaExtSpec(D, "2.0"))

      val omIsa = OMISA(
        xLen = xLen,
        baseSpecification = baseSpec(baseInstructionSet, baseISAVersion),
        base = baseInstructionSet,
        m = coreParams.mulDiv.map { case x => isaExtSpec(M, "2.0") },
        a = coreParams.useAtomics.option(isaExtSpec(A, "2.0")),
        f = coreParams.fpu.map { case x => isaExtSpec(F, "2.0") },
        d = d,
        c = coreParams.useCompressed.option(isaExtSpec(C," 2.0")),
        u = coreParams.useUser.option(isaExtSpec(U,"1.10")),
        s = None,
        addressTranslationModes = Nil
      )

      Seq(OMRocketCore(
        isa = omIsa,
        mulDiv = None, // TODO mulDiv,
        performanceMonitor = perfMon,
        pmp = pmp,
        documentationName = "TODO",
        hartIds = Seq(hartId),
        hasTrace = false, // TODO in the imp below
        hasVectoredInterrupts = true,
        interruptLatency = 2, // TODO
        nLocalInterrupts = coreParams.nLocalInterrupts,
        nBreakpoints = coreParams.nBreakpoints,
        branchPredictor = None, // TODO  Option[OMRocketBranchPredictor],
        dcache = None, // TODO Option[OMDCache],
        icache = None // Option[OMICache]
      ))
    }

  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(hartId))
  }

  override lazy val module = new RocketTileModuleImp(this)

  override def makeMasterBoundaryBuffers(implicit p: Parameters) = {
    if (!rocketParams.boundaryBuffers) super.makeMasterBoundaryBuffers
    else TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
  }

  override def makeSlaveBoundaryBuffers(implicit p: Parameters) = {
    if (!rocketParams.boundaryBuffers) super.makeSlaveBoundaryBuffers
    else TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
  }
}

class RocketTileModuleImp(outer: RocketTile) extends BaseTileModuleImp(outer)
    with HasFpuOpt
    with HasLazyRoCCModule
    with HasICacheFrontendModule {
  Annotated.params(this, outer.rocketParams)

  val core = Module(new Rocket(outer)(outer.p))

  val uncorrectable = RegInit(Bool(false))
  val halt_and_catch_fire = outer.rocketParams.hcfOnUncorrectable.option(IO(Bool(OUTPUT)))

  override val cease = outer.rocketParams.core.clockGate.option(IO(Bool(OUTPUT)))
  cease.foreach(_ := RegNext(
    !outer.dcache.module.io.cpu.clock_enabled &&
    !outer.frontend.module.io.cpu.clock_enabled &&
    !ptw.io.dpath.clock_enabled &&
    core.io.cease
  ))

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

  if (outer.roccs.size > 0) {
    cmdRouter.get.io.in <> core.io.rocc.cmd
    outer.roccs.foreach(_.module.io.exception := core.io.rocc.exception)
    core.io.rocc.resp <> respArb.get.io.out
    core.io.rocc.busy <> (cmdRouter.get.io.busy || outer.roccs.map(_.module.io.busy).reduce(_ || _))
    core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_ || _)
  }

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

trait HasFpuOpt { this: RocketTileModuleImp =>
  val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new FPU(params)(outer.p)))
}
