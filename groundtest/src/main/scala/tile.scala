package groundtest

import Chisel._
import rocket._
import uncore._
import junctions.{SMIIO, ParameterizedBundle}
import scala.util.Random
import cde.{Parameters, Field}

case object BuildGroundTest extends Field[(Int, Parameters) => GroundTest]
case object GroundTestMaxXacts extends Field[Int]

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
  s2_resp.error := Bool(false)
  s2_resp.pte.ppn := s2_ppn
  s2_resp.pte.reserved_for_software := UInt(0)
  s2_resp.pte.d := Bool(false)
  s2_resp.pte.r := Bool(false)
  s2_resp.pte.typ := UInt(2)
  s2_resp.pte.v := Bool(true)

  io.requestors.zipWithIndex.foreach { case (requestor, i) =>
    requestor.resp.valid := s2_valid && s2_chosen === UInt(i)
    requestor.resp.bits := s2_resp
    requestor.status.mprv := Bool(false)
    requestor.status.vm := UInt("b01000")
    requestor.status.prv := UInt(PRV_S)
  }
}

class CSRHandler(implicit val p: Parameters) extends Module {
  private val csrDataBits = 64
  private val csrAddrBits = 12

  val io = new Bundle {
    val finished = Bool(INPUT)
    val csr = new SMIIO(csrDataBits, csrAddrBits).flip
  }

  val csr_resp_valid = Reg(Bool()) // Don't reset
  val csr_resp_data = Reg(UInt(width = csrDataBits))

  io.csr.req.ready := Bool(true)
  io.csr.resp.valid := csr_resp_valid
  io.csr.resp.bits := csr_resp_data

  when (io.csr.req.fire()) {
    val req = io.csr.req.bits
    csr_resp_valid := Bool(true)
    csr_resp_data := Mux(req.addr === UInt(CSRs.mtohost), io.finished, req.data)
  }

  when (io.csr.resp.fire()) {
    csr_resp_valid := Bool(false)
  }
}

class GroundTestIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val cache = new HellaCacheIO
  val mem = new ClientUncachedTileLinkIO
  val dma = new DmaIO
  val ptw = new TLBPTWIO
  val finished = Bool(OUTPUT)
}

abstract class GroundTest(implicit val p: Parameters) extends Module {
  val io = new GroundTestIO

  def disablePorts(mem: Boolean = true,
                   cache: Boolean = true,
                   dma: Boolean = true,
                   ptw: Boolean = true) {
    if (mem) {
      io.mem.acquire.valid := Bool(false)
      io.mem.grant.ready := Bool(false)
    }
    if (cache) {
      io.cache.req.valid := Bool(false)
    }
    if (dma) {
      io.dma.req.valid := Bool(false)
      io.dma.resp.ready := Bool(false)
    }
    if (ptw) {
      io.ptw.req.valid := Bool(false)
    }
  }
}

class GroundTestTile(id: Int, resetSignal: Bool)
                   (implicit val p: Parameters) extends Tile(resetSignal)(p) {

  val test = p(BuildGroundTest)(id, dcacheParams)
  io.uncached.head <> test.io.mem
  io.dma <> test.io.dma

  val dcache = Module(new HellaCache()(dcacheParams))
  val dcacheIF = Module(new SimpleHellaCacheIF()(dcacheParams))
  dcacheIF.io.requestor <> test.io.cache
  dcache.io.cpu <> dcacheIF.io.cache
  io.cached.head <> dcache.io.mem

  val csr = Module(new CSRHandler)
  csr.io.finished := test.io.finished
  csr.io.csr <> io.host.csr

  val ptw = Module(new DummyPTW(2))
  ptw.io.requestors(0) <> test.io.ptw
  ptw.io.requestors(1) <> dcache.io.ptw
}
