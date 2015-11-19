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

class GroundTestIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val cache = new HellaCacheIO
  val mem = new ClientUncachedTileLinkIO
  val finished = Bool(OUTPUT)
}

abstract class GroundTest(implicit p: Parameters) extends Module {
  val io = new GroundTestIO
}

class GroundTestTile(id: Int, resetSignal: Bool)
                   (implicit val p: Parameters) extends Tile(resetSignal)(p) {

  val dcache = Module(new HellaCache()(dcacheParams))
  val dcacheIF = Module(new SimpleHellaCacheIF()(dcacheParams))
  val test = p(BuildGroundTest)(id, dcacheParams)
  io.uncached.head <> test.io.mem
  dcacheIF.io.requestor <> test.io.cache
  dcache.io.cpu <> dcacheIF.io.cache
  io.cached.head <> dcache.io.mem

  val csr = Module(new CSRHandler)
  csr.io.finished := test.io.finished
  csr.io.csr <> io.host.csr
}
