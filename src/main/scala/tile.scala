package rocket

import Chisel._
import uncore._
import Util._

case class RocketConfiguration(tl: TileLinkConfiguration,
                               icache: ICacheConfig, dcache: DCacheConfig,
                               fpu: Boolean, rocc: Option[RocketConfiguration => RoCC] = None,
                               fastLoadWord: Boolean = true,
                               fastLoadByte: Boolean = false,
                               fastMulDiv: Boolean = true)
{
  val dcacheReqTagBits = 9 // enforce compliance with require()
  val xprlen = 64
  val nxpr = 32
  val nxprbits = log2Up(nxpr)
  if (fastLoadByte) require(fastLoadWord)
}

class Tile(resetSignal: Bool = null)(confIn: RocketConfiguration) extends Module(_reset = resetSignal) with ClientCoherenceAgent
{
  val memPorts = 2
  val dcachePortId = 0
  val icachePortId = 1
  implicit val tlConf = confIn.tl
  implicit val lnConf = confIn.tl.ln
  implicit val icConf = confIn.icache
  implicit val dcConf = confIn.dcache.copy(reqtagbits = confIn.dcacheReqTagBits + log2Up(memPorts), databits = confIn.xprlen)
  implicit val conf = confIn.copy(dcache = dcConf)

  val io = new Bundle {
    val tilelink = new TileLinkIO
    val host = new HTIFIO(lnConf.nClients)
  }

  val core = Module(new Core)
  val icache = Module(new Frontend)
  val dcache = Module(new HellaCache)
  val ptw = Module(new PTW(2))

  val dcacheArb = Module(new HellaCacheArbiter(2 + !conf.rocc.isEmpty))
  dcacheArb.io.requestor(0) <> ptw.io.mem
  dcacheArb.io.requestor(1) <> core.io.dmem
  dcache.io.cpu <> dcacheArb.io.mem

  ptw.io.requestor(0) <> icache.io.cpu.ptw
  ptw.io.requestor(1) <> dcache.io.cpu.ptw

  if (!conf.rocc.isEmpty) {
    val dcIF = Module(new SimpleHellaCacheIF)
    val rocc = Module((conf.rocc.get)(conf))
    dcIF.io.requestor <> rocc.io.mem
    core.io.rocc <> rocc.io
    dcacheArb.io.requestor(2) <> dcIF.io.cache
  }

  core.io.host <> io.host
  core.io.imem <> icache.io.cpu
  core.io.ptw <> ptw.io.dpath

  val memArb = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(memPorts))
  memArb.io.in(dcachePortId) <> dcache.io.mem
  memArb.io.in(icachePortId) <> icache.io.mem

  io.tilelink.acquire <> memArb.io.out.acquire
  memArb.io.out.grant <> io.tilelink.grant
  io.tilelink.grant_ack <> memArb.io.out.grant_ack
  dcache.io.mem.probe <> io.tilelink.probe
  io.tilelink.release.data <> dcache.io.mem.release.data
  io.tilelink.release.meta.valid   := dcache.io.mem.release.meta.valid
  dcache.io.mem.release.meta.ready := io.tilelink.release.meta.ready
  io.tilelink.release.meta.bits := dcache.io.mem.release.meta.bits
  io.tilelink.release.meta.bits.payload.client_xact_id :=  Cat(dcache.io.mem.release.meta.bits.payload.client_xact_id, UInt(dcachePortId, log2Up(memPorts))) // Mimic client id extension done by UncachedTileLinkIOArbiter for Acquires from either client)
}
