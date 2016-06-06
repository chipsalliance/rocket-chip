package groundtest

import Chisel._
import rocket._
import uncore._
import junctions._
import scala.util.Random
import cde.{Parameters, Field}

case object BuildGroundTest extends Field[(Int, Parameters) => GroundTest]
case object GroundTestMaxXacts extends Field[Int]
case object GroundTestCSRs extends Field[Seq[Int]]
case object TohostAddr extends Field[BigInt]

/** A "cache" that responds to probe requests with a release indicating
 *  the block is not present */
class DummyCache(implicit val p: Parameters) extends Module {
  val io = new ClientTileLinkIO

  val req = Reg(new Probe)
  val coh = ClientMetadata.onReset
  val (s_probe :: s_release :: Nil) = Enum(Bits(), 2)
  val state = Reg(init = s_probe)

  io.acquire.valid := Bool(false)
  io.probe.ready := (state === s_probe)
  io.grant.ready := Bool(true)
  io.release.valid := (state === s_release)
  io.release.bits := coh.makeRelease(req)

  when (io.probe.fire()) {
    req := io.probe.bits
    state := s_release
  }

  when (io.release.fire()) {
    state := s_probe
  }
}

class DummyPTW(n: Int)(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val requestors = Vec(n, new TLBPTWIO).flip
  }

  val req_arb = Module(new RRArbiter(new PTWReq, n))
  req_arb.io.in <> io.requestors.map(_.req)
  req_arb.io.out.ready := Bool(true)

  def vpn_to_ppn(vpn: UInt): UInt = vpn(ppnBits - 1, 0)

  class QueueChannel extends ParameterizedBundle()(p) {
    val ppn = UInt(width = ppnBits)
    val chosen = UInt(width = log2Up(n))
  }

  val s1_ppn = vpn_to_ppn(req_arb.io.out.bits.addr)
  val s2_ppn = RegEnable(s1_ppn, req_arb.io.out.valid)
  val s2_chosen = RegEnable(req_arb.io.chosen, req_arb.io.out.valid)
  val s2_valid = Reg(next = req_arb.io.out.valid)

  val s2_resp = Wire(new PTWResp)
  s2_resp.pte.ppn := s2_ppn
  s2_resp.pte.reserved_for_software := UInt(0)
  s2_resp.pte.d := Bool(true)
  s2_resp.pte.r := Bool(false)
  s2_resp.pte.typ := UInt("b0101")
  s2_resp.pte.v := Bool(true)

  io.requestors.zipWithIndex.foreach { case (requestor, i) =>
    requestor.resp.valid := s2_valid && s2_chosen === UInt(i)
    requestor.resp.bits := s2_resp
    requestor.status.vm := UInt("b01000")
    requestor.status.prv := UInt(PRV.S)
    requestor.invalidate := Bool(false)
  }
}

class GroundTestIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val cache = new HellaCacheIO
  val mem = new ClientUncachedTileLinkIO
  val dma = new DmaIO
  val ptw = new TLBPTWIO
  val finished = Bool(OUTPUT)
}

abstract class GroundTest(implicit val p: Parameters) extends Module
    with HasAddrMapParameters {
  val io = new GroundTestIO
  val memStart = addrMap("mem").start
  val memStartBlock = memStart >> p(CacheBlockOffsetBits)

  def disablePorts(mem: Boolean = true,
                   cache: Boolean = true,
                   ptw: Boolean = true) {
    if (mem) {
      io.mem.acquire.valid := Bool(false)
      io.mem.grant.ready := Bool(false)
    }
    if (cache) {
      io.cache.req.valid := Bool(false)
    }
    if (ptw) {
      io.ptw.req.valid := Bool(false)
    }
  }
}

class GroundTestFinisher(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val finished = Bool(INPUT)
    val mem = new ClientUncachedTileLinkIO
  }

  val addrBits = p(PAddrBits)
  val offsetBits = tlBeatAddrBits + tlByteAddrBits
  val tohostAddr = UInt(p(TohostAddr), addrBits)

  val s_idle :: s_write :: s_wait :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  when (state === s_idle && io.finished) { state := s_write }
  when (io.mem.acquire.fire()) { state := s_wait }
  when (io.mem.grant.fire()) { state := s_done }

  io.mem.acquire.valid := (state === s_write)
  io.mem.acquire.bits := Put(
    client_xact_id = UInt(0),
    addr_block = tohostAddr(addrBits - 1, offsetBits),
    addr_beat = tohostAddr(offsetBits - 1, tlByteAddrBits),
    data = UInt(1),
    wmask = SInt(-1, 8).asUInt)
  io.mem.grant.ready := (state === s_wait)
}

class GroundTestTile(id: Int, resetSignal: Bool)
                    (implicit val p: Parameters) extends Tile(resetSignal = resetSignal)(p) {

  val test = p(BuildGroundTest)(id, dcacheParams)

  val dcache = Module(new HellaCache()(dcacheParams))
  val dcacheIF = Module(new SimpleHellaCacheIF()(dcacheParams))
  dcacheIF.io.requestor <> test.io.cache
  dcache.io.cpu <> dcacheIF.io.cache
  io.cached.head <> dcache.io.mem

  // SimpleHellaCacheIF leaves invalidate_lr dangling, so we wire it to false
  dcache.io.cpu.invalidate_lr := Bool(false)

  val ptw = Module(new DummyPTW(2))
  ptw.io.requestors(0) <> test.io.ptw
  ptw.io.requestors(1) <> dcache.io.ptw

  // Only Tile 0 needs to write tohost
  if (id == 0) {
    val finisher = Module(new GroundTestFinisher)
    finisher.io.finished := test.io.finished

    val memArb = Module(new ClientUncachedTileLinkIOArbiter(2))
    memArb.io.in(0) <> test.io.mem
    memArb.io.in(1) <> finisher.io.mem
    io.uncached.head <> memArb.io.out
  } else { io.uncached.head <> test.io.mem }
}
