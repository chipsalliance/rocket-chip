package groundtest

import Chisel._
import rocket._
import uncore._
import scala.util.Random
import cde.Parameters

class GeneratorTile(id: Int, rnd: Random, resetSignal: Bool)
                   (implicit val p: Parameters) extends Tile(resetSignal)(p)
                   with HasGeneratorParams {
  val gen_finished = Wire(Vec(nGensPerTile, Bool()))

  val arb = Module(new ClientTileLinkIOArbiter(nGensPerTile))

  for (i <- 0 until nGensPerTile) {
    val genid = id * nGensPerTile + i
    val generator = p(BuildGenerator)(genid, rnd, p)
    arb.io.in(i) <> generator.io.tl
    gen_finished(i) := generator.io.finished
  }

  io.cached(0) <> arb.io.out
  io.uncached(0).acquire.valid := Bool(false)
  io.uncached(0).grant.ready := Bool(false)

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
