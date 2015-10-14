// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._

case object CoreName extends Field[String]
case object NDCachePorts extends Field[Int]
case object NPTWPorts extends Field[Int]
case object BuildRoCC extends Field[Option[Parameters => RoCC]]

abstract class Tile(resetSignal: Bool = null)
                   (implicit p: Parameters) extends Module(_reset = resetSignal) {
  val io = new Bundle {
    val cached = new ClientTileLinkIO
    val uncached = new ClientUncachedTileLinkIO
    val host = new HtifIO
  }
}

class RocketTile(resetSignal: Bool = null)(implicit p: Parameters) extends Tile(resetSignal)(p) {
  //TODO
  val dcacheParams = p.alterPartial({ case CacheName => "L1D" })
  val icache = Module(new Frontend()(p.alterPartial({
                 case CacheName => "L1I"
                 case CoreName => "Rocket" })))
  val dcache = Module(new HellaCache()(dcacheParams))
  val ptw = Module(new PTW(p(NPTWPorts))(dcacheParams))
  val core = Module(new Rocket()(p.alterPartial({ case CoreName => "Rocket" })))

  dcache.io.cpu.invalidate_lr := core.io.dmem.invalidate_lr // Bypass signal to dcache
  val dcArb = Module(new HellaCacheArbiter(p(NDCachePorts))(dcacheParams))
  dcArb.io.requestor(0) <> ptw.io.mem
  dcArb.io.requestor(1) <> core.io.dmem
  dcache.io.cpu <> dcArb.io.mem

  ptw.io.requestor(0) <> icache.io.ptw
  ptw.io.requestor(1) <> dcache.io.ptw

  io.host <> core.io.host
  icache.io.cpu <> core.io.imem
  core.io.ptw <> ptw.io.dpath

  //If so specified, build an FPU module and wire it in
  p(BuildFPU) foreach { fpu => core.io.fpu <> fpu(p).io }

  // Connect the caches and ROCC to the outer memory system
  io.cached <> dcache.io.mem
  // If so specified, build an RoCC module and wire it in
  // otherwise, just hookup the icache
  io.uncached <> p(BuildRoCC).map { buildItHere =>
    val rocc = buildItHere(p)
    val memArb = Module(new ClientTileLinkIOArbiter(3))
    val dcIF = Module(new SimpleHellaCacheIF()(dcacheParams))
    core.io.rocc <> rocc.io
    dcIF.io.requestor <> rocc.io.mem
    dcArb.io.requestor(2) <> dcIF.io.cache
    memArb.io.in(0) <> icache.io.mem
    memArb.io.in(1) <> rocc.io.imem
    memArb.io.in(2) <> rocc.io.dmem
    ptw.io.requestor(2) <> rocc.io.iptw
    ptw.io.requestor(3) <> rocc.io.dptw
    ptw.io.requestor(4) <> rocc.io.pptw
    memArb.io.out
  }.getOrElse(icache.io.mem)
}
