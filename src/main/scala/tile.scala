// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._
import cde.{Parameters, Field}

case object CoreName extends Field[String]
case object BuildRoCC extends Field[Option[Parameters => RoCC]]

abstract class Tile(resetSignal: Bool = null)
                   (implicit p: Parameters) extends Module(_reset = resetSignal) {
  val usingRocc = !p(BuildRoCC).isEmpty
  val nDCachePorts = 2 + (if(!usingRocc) 0 else 1)
  val nPTWPorts = 2 + (if(!usingRocc) 0 else 3)
  val nCachedTileLinkPorts = 1
  val nUncachedTileLinkPorts = 1 + (if(!usingRocc) 0 else p(RoccNMemChannels))
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

  //If so specified, build an FPU module and wire it in
  if (p(UseFPU)) core.io.fpu <> Module(new FPU()(p)).io

   // Connect the caches and ROCC to the outer memory system
  io.cached.head <> dcache.io.mem
  // If so specified, build an RoCC module and wire it to core + TileLink ports,
  // otherwise just hookup the icache
  io.uncached <> p(BuildRoCC).map { buildItHere =>
    val rocc = buildItHere(p)
    val iMemArb = Module(new ClientTileLinkIOArbiter(2))
    val dcIF = Module(new SimpleHellaCacheIF()(dcacheParams))
    core.io.rocc <> rocc.io
    dcIF.io.requestor <> rocc.io.mem
    dcArb.io.requestor(2) <> dcIF.io.cache
    iMemArb.io.in(0) <> icache.io.mem
    iMemArb.io.in(1) <> rocc.io.imem
    ptw.io.requestor(2) <> rocc.io.iptw
    ptw.io.requestor(3) <> rocc.io.dptw
    ptw.io.requestor(4) <> rocc.io.pptw
    rocc.io.dmem :+ iMemArb.io.out
  }.getOrElse(List(icache.io.mem))
}
