package groundtest

import Chisel._
import rocket._
import uncore._
import scala.util.Random
import cde.Parameters

class GeneratorTile(id: Int, resetSignal: Bool)
                   (implicit val p: Parameters) extends Tile(resetSignal)(p)
                   with HasGeneratorParams {
  val gen_finished = Wire(Vec(nGensPerTile, Bool()))

  val arb = Module(new ClientUncachedTileLinkIOArbiter(nGensPerTile))

  for (i <- 0 until nGensPerTile) {
    val genid = id * nGensPerTile + i
    val generator = Module(new UncachedTileLinkGenerator(genid))
    arb.io.in(i) <> generator.io.tl
    gen_finished(i) := generator.io.finished
  }

  io.uncached(0) <> arb.io.out
  io.cached(0).acquire.valid := Bool(false)
  io.cached(0).grant.ready := Bool(false)
  io.cached(0).probe.ready := Bool(false)
  io.cached(0).release.valid := Bool(false)

  assert(!io.cached(0).probe.valid, "Shouldn't be receiving probes")

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
