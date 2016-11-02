// See LICENSE for license details.

package rocket

import Chisel._
import uncore.tilelink._
import uncore.agents._
import uncore.converters._
import uncore.devices._
import util._
import cde.{Parameters, Field}

case object BuildRoCC extends Field[Seq[RoccParameters]]
case object DecoupledRoCC extends Field[Bool]
case object RoCCQueueDepth extends Field[Int]
case object NCachedTileLinkPorts extends Field[Int]
case object NUncachedTileLinkPorts extends Field[Int]
case object TileId extends Field[Int]

case class RoccParameters(
  opcodes: OpcodeSet,
  generator: (Parameters) => RoCC,
  nMemChannels: Int = 0,
  nPTWPorts : Int = 0,
  useFPU: Boolean = false)

case class TileBundleConfig(
  nCachedTileLinkPorts: Int,
  nUncachedTileLinkPorts: Int,
  xLen: Int,
  hasSlavePort: Boolean)

class TileIO(c: TileBundleConfig)(implicit p: Parameters) extends Bundle {
  val cached = Vec(c.nCachedTileLinkPorts, new ClientTileLinkIO)
  val uncached = Vec(c.nUncachedTileLinkPorts, new ClientUncachedTileLinkIO)
  val hartid = UInt(INPUT, c.xLen)
  val interrupts = new TileInterrupts().asInput
  val slave = c.hasSlavePort.option(new ClientUncachedTileLinkIO().flip)
  val resetVector = UInt(INPUT, c.xLen)
  val roccClock = Clock(INPUT)
  val roccReset = Bool(INPUT)

  override def cloneType = new TileIO(c).asInstanceOf[this.type]
}

abstract class Tile(clockSignal: Clock = null, resetSignal: Bool = null)
    (implicit p: Parameters) extends Module(Option(clockSignal), Option(resetSignal)) {
  val nCachedTileLinkPorts = p(NCachedTileLinkPorts)
  val nUncachedTileLinkPorts = p(NUncachedTileLinkPorts)
  val dcacheParams = p.alterPartial({ case CacheName => "L1D" })
  val bc = TileBundleConfig(
    nCachedTileLinkPorts = nCachedTileLinkPorts,
    nUncachedTileLinkPorts = nUncachedTileLinkPorts,
    xLen = p(XLen),
    hasSlavePort = p(DataScratchpadSize) > 0)

  val io = new TileIO(bc)
}

class RocketTile(clockSignal: Clock = null, resetSignal: Bool = null)
    (implicit p: Parameters) extends Tile(clockSignal, resetSignal)(p) {
  val buildRocc = p(BuildRoCC)
  val usingRocc = !buildRocc.isEmpty
  val nRocc = buildRocc.size
  val nFPUPorts = buildRocc.filter(_.useFPU).size

  val core = Module(new Rocket)
  val icache = Module(new Frontend()(p.alterPartial({ case CacheName => "L1I" })))
  val dcache = HellaCache(p(DCacheKey))(dcacheParams)

  val ptwPorts = collection.mutable.ArrayBuffer(icache.io.ptw, dcache.ptw)
  val dcPorts = collection.mutable.ArrayBuffer(core.io.dmem)
  val uncachedArbPorts = collection.mutable.ArrayBuffer(icache.io.mem)
  val uncachedPorts = collection.mutable.ArrayBuffer[ClientUncachedTileLinkIO]()
  val cachedPorts = collection.mutable.ArrayBuffer(dcache.mem)
  core.io.interrupts := io.interrupts
  core.io.hartid := io.hartid
  icache.io.cpu <> core.io.imem
  icache.io.resetVector := io.resetVector

  // Send counter signals into the core for use in the CSR file
  core.io.counters.imem := icache.io.mem.acquire.valid & icache.io.mem.acquire.ready
  core.io.counters.dmem := dcache.counters

  val fpuOpt = p(FPUKey).map(cfg => Module(new FPU(cfg)))
  fpuOpt.foreach(fpu => core.io.fpu <> fpu.io)

  if (usingRocc) {
    val respArb = Module(new RRArbiter(new RoCCResponse, nRocc))
    core.io.rocc.resp <> respArb.io.out

    val roccOpcodes = buildRocc.map(_.opcodes)
    val cmdRouter = Module(new RoccCommandRouter(roccOpcodes))
    cmdRouter.io.in <> core.io.rocc.cmd

    val roccs = buildRocc.zipWithIndex.map { case (accelParams, i) =>
      val rocc = accelParams.generator(p.alterPartial({
        case RoccNMemChannels => accelParams.nMemChannels
        case RoccNPTWPorts => accelParams.nPTWPorts
      }))
      rocc.clock := io.roccClock
      rocc.reset := io.roccReset
      val interface = if(p(DecoupledRoCC)) {
        val decoupler = Module(new RoccBusyDecoupler(
          accelParams.opcodes, 10)(p.alterPartial({
            case RoccNMemChannels => accelParams.nMemChannels
            case RoccNPTWPorts => accelParams.nPTWPorts
        })))

        AsyncQueueify(decoupler.clock, decoupler.reset,
          (rocc.io.elements - "utl").values,
          (decoupler.io.roccOut.elements - "utl").values,
          rocc.clock, rocc.reset,
          p(RoCCQueueDepth), 2)
        // UTL port is crossed in the coreplex for now
        decoupler.io.roccOut.utl <> rocc.io.utl

        val twoPhaseRocc = Wire(Bool())
        twoPhaseRocc := LevelSyncTo(rocc.clock, decoupler.io.twoPhase, 2)
        // Most RoCC impls takes a cycle of decode to become busy
        // so we add another sync reg to account for this
        decoupler.io.delayTwoPhase := LevelSyncFrom(rocc.clock, twoPhaseRocc, 2+1)
        decoupler
      } else rocc

      val dcIF = Module(new SimpleHellaCacheIF()(dcacheParams))
      interface.io.cmd <> cmdRouter.io.out(i)
      interface.io.exception := core.io.rocc.exception
      dcIF.io.requestor <> interface.io.mem
      dcPorts += dcIF.io.cache
      uncachedArbPorts += interface.io.autl
      interface
    }

    if (nFPUPorts > 0) {
      fpuOpt.foreach { fpu =>
        val fpArb = Module(new InOrderArbiter(new FPInput, new FPResult, nFPUPorts))
        val fp_roccs = roccs.zip(buildRocc)
          .filter { case (_, params) => params.useFPU }
          .map { case (rocc, _) => rocc.io }
        fpArb.io.in_req <> fp_roccs.map(_.fpu_req)
        fp_roccs.zip(fpArb.io.in_resp).foreach {
          case (rocc, fpu_resp) => rocc.fpu_resp <> fpu_resp
        }
        fpu.io.cp_req <> fpArb.io.out_req
        fpArb.io.out_resp <> fpu.io.cp_resp
      }
    }

    core.io.rocc.busy := cmdRouter.io.busy || roccs.map(_.io.busy).reduce(_ || _)
    core.io.rocc.interrupt := roccs.map(_.io.interrupt).reduce(_ || _)
    respArb.io.in <> roccs.map(rocc => Queue(rocc.io.resp))

    ptwPorts ++= roccs.flatMap(_.io.ptw)
    uncachedPorts ++= roccs.flatMap(_.io.utl)
  }

  val uncachedArb = Module(new ClientUncachedTileLinkIOArbiter(uncachedArbPorts.size))
  uncachedArb.io.in <> uncachedArbPorts
  uncachedArb.io.out +=: uncachedPorts

  // Connect the caches and RoCC to the outer memory system
  io.uncached <> uncachedPorts
  io.cached <> cachedPorts
  // TODO remove nCached/nUncachedTileLinkPorts parameters and these assertions
  require(uncachedPorts.size == nUncachedTileLinkPorts)
  require(cachedPorts.size == nCachedTileLinkPorts)

  if (p(UseVM)) {
    val ptw = Module(new PTW(ptwPorts.size)(dcacheParams))
    ptw.io.requestor <> ptwPorts
    ptw.io.mem +=: dcPorts
    core.io.ptw <> ptw.io.dpath
  }

  io.slave foreach { case slavePort =>
    val adapter = Module(new ScratchpadSlavePort()(dcacheParams))
    val fragmenter = Module(new TileLinkFragmenter()(adapter.io.tl.p))
    TileLinkWidthAdapter(fragmenter.io.in, slavePort)
    adapter.io.tl <> fragmenter.io.out
    adapter.io.dmem +=: dcPorts
  }

  require(dcPorts.size == core.dcacheArbPorts)
  val dcArb = Module(new HellaCacheArbiter(dcPorts.size)(dcacheParams))
  dcArb.io.requestor <> dcPorts
  dcache.cpu <> dcArb.io.mem

  if (nFPUPorts == 0) {
    fpuOpt.foreach { fpu =>
      fpu.io.cp_req.valid := Bool(false)
      fpu.io.cp_resp.ready := Bool(false)
    }
  }
}
