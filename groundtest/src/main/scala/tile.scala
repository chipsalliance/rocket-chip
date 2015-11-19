package groundtest

import Chisel._
import rocket._
import uncore._
import junctions.SMIIO
import scala.util.Random
import cde.Parameters

/** A "cache" that responds to probe requests with a release indicating
 *  the block is not present */
class DummyCache(implicit val p: Parameters) extends Module
    with HasGeneratorParams {
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

class GeneratorTile(id: Int, resetSignal: Bool)
                   (implicit val p: Parameters) extends Tile(resetSignal)(p)
                   with HasGeneratorParams {

  val gen_finished = Wire(Vec(2 * nGensPerTile, Bool()))

  if (genUncached) {
    val uncacheArb = Module(new ClientUncachedTileLinkIOArbiter(nGensPerTile))

    for (i <- 0 until nGensPerTile) {
      val genid = id * nGensPerTile + i
      val uncacheGen = Module(new UncachedTileLinkGenerator(genid))
      uncacheArb.io.in(i) <> uncacheGen.io.mem
      gen_finished(2 * i) := uncacheGen.io.finished
    }

    io.uncached(0) <> uncacheArb.io.out
  } else {
    io.uncached(0).acquire.valid := Bool(false)
    io.uncached(0).grant.ready := Bool(false)
    for (i <- 0 until nGensPerTile) { gen_finished(2 * i) := Bool(true) }
  }

  if (genCached) {
    val cacheArb = Module(new HellaCacheArbiter(nGensPerTile)(dcacheParams))
    val cache = Module(new HellaCache()(dcacheParams))

    for (i <- 0 until nGensPerTile) {
      val genid = id * nGensPerTile + i
      val cacheGen = Module(new HellaCacheGenerator(genid)(dcacheParams))
      val cacheIF = Module(new SimpleHellaCacheIF()(dcacheParams))
      cacheIF.io.requestor <> cacheGen.io.mem
      cacheArb.io.requestor(i) <> cacheIF.io.cache
      gen_finished(2 * i + 1) := cacheGen.io.finished
    }

    cache.io.ptw.req.ready := Bool(false)
    cache.io.ptw.resp.valid := Bool(false)
    cache.io.cpu <> cacheArb.io.mem

    assert(!cache.io.ptw.req.valid, 
      "Cache should not be using virtual addressing")

    io.cached(0) <> cache.io.mem
  } else {
    io.cached(0) <> Module(new DummyCache).io
    for (i <- 0 until nGensPerTile) { gen_finished(2 * i + 1) := Bool(true) }
  }

  val all_done = gen_finished.reduce(_ && _)
  val csr = Module(new CSRHandler)
  csr.io.finished := all_done
  csr.io.csr <> io.host.csr
}

