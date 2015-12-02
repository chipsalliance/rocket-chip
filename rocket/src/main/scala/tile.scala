// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._
import cde.{Parameters, Field}

case object CoreName extends Field[String]
case object BuildRoCC extends Field[Seq[Parameters => RoCC]]
case object RoccOpcodes extends Field[Seq[OpcodeSet]]
case object RoccAcceleratorMemChannels extends Field[Seq[Int]]
case object RoccUseFPU extends Field[Seq[Boolean]]

abstract class Tile(resetSignal: Bool = null)
                   (implicit p: Parameters) extends Module(_reset = resetSignal) {
  val buildRocc = p(BuildRoCC)
  val roccOpcodes = p(RoccOpcodes)
  val roccMemChannels = p(RoccAcceleratorMemChannels)
  val usingRocc = !buildRocc.isEmpty
  val nRocc = buildRocc.size
  val roccUseFPU = p(RoccUseFPU)
  val nFPUPorts = roccUseFPU.filter(useFPU => useFPU).size
  val nDCachePorts = 2 + nRocc
  val nPTWPorts = 2 + 3 * nRocc
  val nCachedTileLinkPorts = 1
  val nUncachedTileLinkPorts = 1 + p(RoccNMemChannels)
  val dcacheParams = p.alterPartial({ case CacheName => "L1D" })
  val io = new Bundle {
    val cached = Vec(nCachedTileLinkPorts, new ClientTileLinkIO)
    val uncached = Vec(nUncachedTileLinkPorts, new ClientUncachedTileLinkIO)
    val host = new HtifIO
  }
}

class RocketTile(resetSignal: Bool = null)(implicit p: Parameters) extends Tile(resetSignal)(p) {
  val core = Module(new Rocket()(p.alterPartial({ case CoreName => "Rocket" })))
  val icache = Module(new Frontend()(p.alterPartial({
    case CacheName => "L1I"
    case CoreName => "Rocket" })))
  val dcache = Module(new HellaCache()(dcacheParams))
  val ptw = Module(new PTW(nPTWPorts)(dcacheParams))

  dcache.io.cpu.invalidate_lr := core.io.dmem.invalidate_lr // Bypass signal to dcache
  val dcArb = Module(new HellaCacheArbiter(nDCachePorts)(dcacheParams))
  dcArb.io.requestor(0) <> ptw.io.mem
  dcArb.io.requestor(1) <> core.io.dmem
  dcache.io.cpu <> dcArb.io.mem

  ptw.io.requestor(0) <> icache.io.ptw
  ptw.io.requestor(1) <> dcache.io.ptw

  io.host <> core.io.host
  icache.io.cpu <> core.io.imem
  core.io.ptw <> ptw.io.dpath

  val fpuOpt = if (p(UseFPU)) Some(Module(new FPU)) else None
  fpuOpt.foreach(fpu => core.io.fpu <> fpu.io)

   // Connect the caches and ROCC to the outer memory system
  io.cached.head <> dcache.io.mem
  // If so specified, build an RoCC module and wire it to core + TileLink ports,
  // otherwise just hookup the icache
  io.uncached <> (if (usingRocc) {
    val iMemArb = Module(new ClientTileLinkIOArbiter(1 + nRocc))
    iMemArb.io.in(0) <> icache.io.mem

    val respArb = Module(new RRArbiter(new RoCCResponse, nRocc))
    core.io.rocc.resp <> respArb.io.out

    val cmdRouter = Module(new RoccCommandRouter(roccOpcodes))
    cmdRouter.io.in <> core.io.rocc.cmd

    val roccs = buildRocc.zip(roccMemChannels).zipWithIndex.map {
      case ((buildItHere, nchannels), i) =>
        val accelParams = p.alterPartial({ case RoccNMemChannels => nchannels})
        val rocc = buildItHere(accelParams)
        val dcIF = Module(new SimpleHellaCacheIF()(dcacheParams))
        rocc.io.cmd <> cmdRouter.io.out(i)
        rocc.io.s := core.io.rocc.s
        rocc.io.exception := core.io.rocc.exception
        dcIF.io.requestor <> rocc.io.mem
        dcArb.io.requestor(2 + i) <> dcIF.io.cache
        iMemArb.io.in(1 + i) <> rocc.io.imem
        ptw.io.requestor(2 + 3 * i) <> rocc.io.iptw
        ptw.io.requestor(3 + 3 * i) <> rocc.io.dptw
        ptw.io.requestor(4 + 3 * i) <> rocc.io.pptw
        rocc
    }

    if (nFPUPorts > 0) {
      fpuOpt.foreach { fpu =>
        val fpArb = Module(new InOrderArbiter(new FPInput, new FPResult, nFPUPorts))
        val fp_roccs = roccs.zip(roccUseFPU)
          .filter { case (_, useFPU) => useFPU }
          .map { case (rocc, _) => rocc }
        fpArb.io.in_req <> fp_roccs.map(_.io.fpu_req)
        fp_roccs.zip(fpArb.io.in_resp).foreach {
          case (rocc, fpu_resp) => rocc.io.fpu_resp <> fpu_resp
        }
        fpu.io.cp_req <> fpArb.io.out_req
        fpArb.io.out_resp <> fpu.io.cp_resp
      }
    }

    core.io.rocc.busy := cmdRouter.io.busy || roccs.map(_.io.busy).reduce(_ || _)
    core.io.rocc.interrupt := roccs.map(_.io.interrupt).reduce(_ || _)
    respArb.io.in <> roccs.map(rocc => Queue(rocc.io.resp))

    roccs.flatMap(_.io.dmem) :+ iMemArb.io.out
  } else { Seq(icache.io.mem) })
}
