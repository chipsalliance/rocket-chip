package groundtest

import Chisel._
import rocket._
import uncore._
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

class GeneratorTile(id: Int, resetSignal: Bool)
                   (implicit val p: Parameters) extends Tile(resetSignal)(p)
                   with HasGeneratorParams {
  val gen_finished = Wire(Vec(nGensPerTile, Bool()))

  val arb = Module(new ClientUncachedTileLinkIOArbiter(nGensPerTile))

  for (i <- 0 until nGensPerTile) {
    val genid = id * nGensPerTile + i
    val generator = Module(new UncachedTileLinkGenerator(genid))
    arb.io.in(i) <> generator.io.mem
    gen_finished(i) := generator.io.finished
  }

  io.uncached(0) <> arb.io.out
  io.cached(0) <> Module(new DummyCache).io

  val all_done = gen_finished.reduce(_ && _)

  val csr_resp_valid = Reg(Bool()) // Don't reset
  val csr_resp_data = Reg(io.host.csr.resp.bits)

  io.host.csr.req.ready := Bool(true)
  io.host.csr.resp.valid := csr_resp_valid
  io.host.csr.resp.bits := csr_resp_data

  when (io.host.csr.req.fire()) {
    val req = io.host.csr.req.bits
    csr_resp_valid := Bool(true)
    csr_resp_data := Mux(req.addr === UInt(CSRs.mtohost), all_done, req.data)
  }

  when (io.host.csr.resp.fire()) {
    csr_resp_valid := Bool(false)
  }
}
