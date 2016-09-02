// See LICENSE for license details.

package rocket

import Chisel._
import uncore.tilelink._
import uncore.agents._
import uncore.devices._
import Util._
import cde.{Parameters, Field}

case object BuildRoCC extends Field[Seq[RoccParameters]]
case object NCachedTileLinkPorts extends Field[Int]
case object NUncachedTileLinkPorts extends Field[Int]
case object TileId extends Field[Int]

case class RoccParameters(
  opcodes: OpcodeSet,
  generator: Parameters => RoCC,
  nMemChannels: Int = 0,
  nPTWPorts : Int = 0,
  useFPU: Boolean = false)

abstract class Tile(clockSignal: Clock = null, resetSignal: Bool = null)
    (implicit p: Parameters) extends Module(Option(clockSignal), Option(resetSignal)) {
  val nCachedTileLinkPorts = p(NCachedTileLinkPorts)
  val nUncachedTileLinkPorts = p(NUncachedTileLinkPorts)
  val dcacheParams = p.alterPartial({ case CacheName => "L1D" })

  class TileIO extends Bundle {
    val cached = Vec(nCachedTileLinkPorts, new ClientTileLinkIO)
    val uncached = Vec(nUncachedTileLinkPorts, new ClientUncachedTileLinkIO)
    val prci = new PRCITileIO().flip
  }

  val io = new TileIO
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
  core.io.prci <> io.prci
  icache.io.cpu <> core.io.imem

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
      val dcIF = Module(new SimpleHellaCacheIF()(dcacheParams))
      rocc.io.cmd <> cmdRouter.io.out(i)
      rocc.io.exception := core.io.rocc.exception
      dcIF.io.requestor <> rocc.io.mem
      dcPorts += dcIF.io.cache
      uncachedArbPorts += rocc.io.autl
      rocc
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
