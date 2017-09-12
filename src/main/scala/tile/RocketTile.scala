// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class RocketTileParams(
    core: RocketCoreParams = RocketCoreParams(),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    rocc: Seq[RoCCParams] = Nil,
    btb: Option[BTBParams] = Some(BTBParams()),
    dataScratchpadBytes: Int = 0,
    boundaryBuffers: Boolean = false,
    trace: Boolean = false,
    hcfOnUncorrectable: Boolean = false,
    name: Option[String] = Some("tile"),
    externalMasterBuffers: Int = 0,
    externalSlaveBuffers: Int = 0) extends TileParams {
  require(icache.isDefined)
  require(dcache.isDefined)
}
  
class RocketTile(val rocketParams: RocketTileParams, val hartid: Int)(implicit p: Parameters) extends BaseTile(rocketParams)(p)
    with HasExternalInterrupts
    with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with CanHaveScratchpad { // implies CanHavePTW with HasHellaCache with HasICacheFrontend

  nDCachePorts += 1 // core TODO dcachePorts += () => module.core.io.dmem ??

  private def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))
  private def ofStr(x: String) = Seq(ResourceString(x))
  private def ofRef(x: Device) = Seq(ResourceReference(x.label))

  val cpuDevice = new Device {
    def describe(resources: ResourceBindings): Description = {
      val block =  p(CacheBlockBytes)
      val m = if (rocketParams.core.mulDiv.nonEmpty) "m" else ""
      val a = if (rocketParams.core.useAtomics) "a" else ""
      val f = if (rocketParams.core.fpu.nonEmpty) "f" else ""
      val d = if (rocketParams.core.fpu.nonEmpty && p(XLen) > 32) "d" else ""
      val c = if (rocketParams.core.useCompressed) "c" else ""
      val isa = s"rv${p(XLen)}i$m$a$f$d$c"

      val dcache = rocketParams.dcache.filter(!_.scratch.isDefined).map(d => Map(
        "d-cache-block-size"   -> ofInt(block),
        "d-cache-sets"         -> ofInt(d.nSets),
        "d-cache-size"         -> ofInt(d.nSets * d.nWays * block))).getOrElse(Map())

      val dtim = scratch.map(d => Map(
        "sifive,dtim"          -> ofRef(d.device))).getOrElse(Map())

      val itim = if (!frontend.icache.slaveNode.isDefined) Map() else Map(
        "sifive,itim"          -> ofRef(frontend.icache.device))

      val icache = rocketParams.icache.map(i => Map(
        "i-cache-block-size"   -> ofInt(block),
        "i-cache-sets"         -> ofInt(i.nSets),
        "i-cache-size"         -> ofInt(i.nSets * i.nWays * block))).getOrElse(Map())

      val dtlb = rocketParams.dcache.filter(_ => rocketParams.core.useVM).map(d => Map(
        "d-tlb-size"           -> ofInt(d.nTLBEntries),
        "d-tlb-sets"           -> ofInt(1))).getOrElse(Map())

      val itlb = rocketParams.icache.filter(_ => rocketParams.core.useVM).map(i => Map(
        "i-tlb-size"           -> ofInt(i.nTLBEntries),
        "i-tlb-sets"           -> ofInt(1))).getOrElse(Map())

      val mmu = if (!rocketParams.core.useVM) Map() else Map(
        "tlb-split" -> Nil,
        "mmu-type"  -> ofStr(p(PgLevels) match {
          case 2 => "riscv,sv32"
          case 3 => "riscv,sv39"
          case 4 => "riscv,sv48"
      }))

      // Find all the caches
      val outer = masterNode.edgesOut
        .flatMap(_.manager.managers)
        .filter(_.supportsAcquireB)
        .flatMap(_.resources.headOption)
        .map(_.owner.label)
        .distinct
      val nextlevel: Option[(String, Seq[ResourceValue])] =
        if (outer.isEmpty) None else
        Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)

      Description(s"cpus/cpu@${hartid}", Map(
        "reg"                  -> resources("reg").map(_.value),
        "device_type"          -> ofStr("cpu"),
        "compatible"           -> Seq(ResourceString("sifive,rocket0"), ResourceString("riscv")),
        "status"               -> ofStr("okay"),
        "clock-frequency"      -> Seq(ResourceInt(rocketParams.core.bootFreqHz)),
        "riscv,isa"            -> ofStr(isa))
        ++ dcache ++ icache ++ nextlevel ++ mmu ++ itlb ++ dtlb ++ dtim ++itim)
    }
  }
  val intcDevice = new Device {
    def describe(resources: ResourceBindings): Description = {
      Description(s"cpus/cpu@${hartid}/interrupt-controller", Map(
        "compatible"           -> ofStr("riscv,cpu-intc"),
        "interrupt-controller" -> Nil,
        "#interrupt-cells"     -> ofInt(1)))
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceInt(BigInt(hartid)))
    Resource(intcDevice, "reg").bind(ResourceInt(BigInt(hartid)))

    intNode.edgesIn.flatMap(_.source.sources).map { case s =>
      for (i <- s.range.start until s.range.end) {
       csrIntMap.lift(i).foreach { j =>
          s.resources.foreach { r =>
            r.bind(intcDevice, ResourceInt(j))
          }
        }
      }
    }
  }

  override lazy val module = new RocketTileModule(this)
}

class RocketTileBundle(outer: RocketTile) extends BaseTileBundle(outer)
    with HasExternalInterruptsBundle
    with CanHaveScratchpadBundle
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

abstract class RocketTileWrapper(rtp: RocketTileParams, hartid: Int)(implicit p: Parameters) extends LazyModule {
  val rocket = LazyModule(new RocketTile(rtp, hartid))
  val masterNode: OutputNode[_,_,_,_,_]
  val slaveNode: InputNode[_,_,_,_,_]
  val intOutputNode = rocket.intOutputNode.map(dummy => IntOutputNode())
  val asyncIntNode   = IntInputNode()
  val periphIntNode  = IntInputNode()
  val coreIntNode    = IntInputNode()
  val intXbar = LazyModule(new IntXbar)

  rocket.intNode := intXbar.intnode

  def optionalMasterBuffer(in: TLOutwardNode): TLOutwardNode = {
    if (rtp.boundaryBuffers) {
      val mbuf = LazyModule(new TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1)))
      mbuf.node :=* in
      mbuf.node
    } else {
      in
    }
  }

  def optionalSlaveBuffer(in: TLOutwardNode): TLOutwardNode = {
    if (rtp.boundaryBuffers) {
      val sbuf = LazyModule(new TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none))
      DisableMonitors { implicit p => sbuf.node :*= in }
      sbuf.node
    } else {
      in
    }
  }

  def outputInterruptXingLatency: Int

  rocket.intOutputNode.foreach { rocketIntOutputNode =>
    val outXing = LazyModule(new IntXing(outputInterruptXingLatency))
    intOutputNode.get := outXing.intnode
    outXing.intnode := rocketIntOutputNode
  }

  lazy val module = new LazyModuleImp(this) {
    val io = new CoreBundle
        with HasExternallyDrivenTileConstants
        with CanHaveInstructionTracePort
        with CanHaltAndCatchFire {
      val master = masterNode.bundleOut
      val slave = slaveNode.bundleIn
      val outputInterrupts = intOutputNode.map(_.bundleOut)
      val asyncInterrupts  = asyncIntNode.bundleIn
      val periphInterrupts = periphIntNode.bundleIn
      val coreInterrupts   = coreIntNode.bundleIn
      val halt_and_catch_fire = rocket.module.io.halt_and_catch_fire.map(_.cloneType)
    }
    // signals that do not change based on crossing type:
    rocket.module.io.hartid := io.hartid
    rocket.module.io.reset_vector := io.reset_vector
    io.trace.foreach { _ := rocket.module.io.trace.get }
    io.halt_and_catch_fire.foreach { _ := rocket.module.io.halt_and_catch_fire.get }
  }
}

class SyncRocketTile(rtp: RocketTileParams, hartid: Int)(implicit p: Parameters) extends RocketTileWrapper(rtp, hartid) {
  val masterNode = TLOutputNode()
  masterNode :=* optionalMasterBuffer(rocket.masterNode)

  val slaveNode = new TLInputNode()(ValName("slave")) { override def reverse = true }
  DisableMonitors { implicit p => rocket.slaveNode :*= optionalSlaveBuffer(slaveNode) }

  // Fully async interrupts need synchronizers.
  // Others need no synchronization.
  val xing = LazyModule(new IntXing(3))
  xing.intnode := asyncIntNode

  intXbar.intnode  := xing.intnode
  intXbar.intnode  := periphIntNode
  intXbar.intnode  := coreIntNode

  def outputInterruptXingLatency = 0
}

class AsyncRocketTile(rtp: RocketTileParams, hartid: Int)(implicit p: Parameters) extends RocketTileWrapper(rtp, hartid) {
  val masterNode = TLAsyncOutputNode()
  val source = LazyModule(new TLAsyncCrossingSource)
  source.node :=* rocket.masterNode
  masterNode :=* source.node

  val slaveNode = new TLAsyncInputNode()(ValName("slave")) { override def reverse = true }
  val sink = LazyModule(new TLAsyncCrossingSink)

  DisableMonitors { implicit p =>
    rocket.slaveNode :*= sink.node
    sink.node :*= slaveNode
  }

  // Fully async interrupts need synchronizers,
  // as do those coming from the periphery clock.
  // Others need no synchronization.
  val asyncXing = LazyModule(new IntXing(3))
  val periphXing = LazyModule(new IntXing(3))
  asyncXing.intnode := asyncIntNode
  periphXing.intnode := periphIntNode

  intXbar.intnode  := asyncXing.intnode
  intXbar.intnode  := periphXing.intnode
  intXbar.intnode  := coreIntNode

  def outputInterruptXingLatency = 3
}

class RationalRocketTile(rtp: RocketTileParams, hartid: Int)(implicit p: Parameters) extends RocketTileWrapper(rtp, hartid) {
  val masterNode = TLRationalOutputNode()
  val source = LazyModule(new TLRationalCrossingSource)
  source.node :=* optionalMasterBuffer(rocket.masterNode)
  masterNode :=* source.node

  val slaveNode = new TLRationalInputNode()(ValName("slave")) { override def reverse = true }
  val sink = LazyModule(new TLRationalCrossingSink(SlowToFast))

  DisableMonitors { implicit p =>
    sink.node :*= slaveNode
    rocket.slaveNode :*= optionalSlaveBuffer(sink.node)
  }

  // Fully async interrupts need synchronizers.
  // Those coming from periphery clock need a
  // rational synchronizer.
  // Others need no synchronization.
  val asyncXing    = LazyModule(new IntXing(3))
  val periphXing = LazyModule(new IntXing(1))
  asyncXing.intnode := asyncIntNode
  periphXing.intnode := periphIntNode

  intXbar.intnode  := asyncXing.intnode
  intXbar.intnode  := periphXing.intnode
  intXbar.intnode  := coreIntNode

  def outputInterruptXingLatency = 1
}
