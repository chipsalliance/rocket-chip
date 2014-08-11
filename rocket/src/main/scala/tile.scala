package rocket

import Chisel._
import uncore._
import Util._

case object NDCachePorts extends Field[Int]
case object NTilePorts extends Field[Int]
case object BuildRoCC extends Field[Option[() => RoCC]]
case object RetireWidth extends Field[Int]
case object UseVM extends Field[Boolean]
case object FastLoadWord extends Field[Boolean]
case object FastLoadByte extends Field[Boolean]
case object FastMulDiv extends Field[Boolean]
case object DcacheReqTagBits extends Field[Int]
case object XprLen extends Field[Int]
case object NXpr extends Field[Int]
case object NXprBits extends Field[Int]
case object RocketDCacheParams extends Field[PF]
case object RocketFrontendParams extends Field[PF]

class Tile(resetSignal: Bool = null) extends Module(_reset = resetSignal) {

  if(params(FastLoadByte)) require(params(FastLoadWord))
  require(params(RetireWidth) == 1) // for now...

  val io = new Bundle {
    val tilelink = new TileLinkIO
    val host = new HTIFIO
  }
  // Mimic client id extension done by UncachedTileLinkIOArbiter for Acquires from either client)

  val optionalRoCC = params(BuildRoCC)

  val p = params.alter(params(CoreBTBParams)).alter(params(RocketFrontendParams)) // Used in icache, Core
  val icache = Module(new Frontend)(p) //TODO PARAMS: best way to alter both?
  params.alter(params(RocketDCacheParams)) // Used in dcache, PTW, RoCCm Core
  val dcache = Module(new HellaCache)
  val ptw = Module(new PTW(if(optionalRoCC.isEmpty) 2 else 5))
    // 2 ports, 1 from I$, 1 from D$, maybe 3 from RoCC
  val core = Module(new Core)

  val dcArb = Module(new HellaCacheArbiter(params(NDCachePorts)))
  dcArb.io.requestor(0) <> ptw.io.mem
  dcArb.io.requestor(1) <> core.io.dmem
  dcArb.io.mem <> dcache.io.cpu

  ptw.io.requestor(0) <> icache.io.cpu.ptw
  ptw.io.requestor(1) <> dcache.io.cpu.ptw

  core.io.host <> io.host
  core.io.imem <> icache.io.cpu
  core.io.ptw <> ptw.io.dpath

  val memArb = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(params(NTilePorts)))
  val dcPortId = 0
  memArb.io.in(dcPortId) <> dcache.io.mem
  memArb.io.in(1) <> icache.io.mem

  if(!optionalRoCC.isEmpty) {
    val rocc = Module(optionalRoCC.get())
    val dcIF = Module(new SimpleHellaCacheIF)
    dcIF.io.requestor <> rocc.io.mem
    core.io.rocc <> rocc.io
    dcArb.io.requestor(2) <> dcIF.io.cache
    memArb.io.in(2) <> rocc.io.imem
    ptw.io.requestor(2) <> rocc.io.iptw
    ptw.io.requestor(3) <> rocc.io.dptw
    ptw.io.requestor(4) <> rocc.io.pptw
  }
 
  io.tilelink.acquire <> memArb.io.out.acquire
  io.tilelink.grant <> memArb.io.out.grant
  io.tilelink.finish <> memArb.io.out.finish
  // Probes and releases routed directly to coherent dcache
  io.tilelink.probe <> dcache.io.mem.probe
  // Mimic client id extension done by UncachedTileLinkIOArbiter for Acquires from either client)
  io.tilelink.release.valid   := dcache.io.mem.release.valid
  dcache.io.mem.release.ready := io.tilelink.release.ready
  io.tilelink.release.bits := dcache.io.mem.release.bits
  io.tilelink.release.bits.payload.client_xact_id :=  Cat(dcache.io.mem.release.bits.payload.client_xact_id, UInt(dcPortId, log2Up(params(NTilePorts))))
}
