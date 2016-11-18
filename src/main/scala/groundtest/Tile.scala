package groundtest

import Chisel._
import rocket._
import uncore.tilelink._
import uncore.agents.CacheName
import uncore.tilelink2._
import rocketchip.ExtMem
import diplomacy._
import scala.util.Random
import scala.collection.mutable.ListBuffer
import junctions.HasAddrMapParameters
import util.ParameterizedBundle
import cde.{Parameters, Field}

case object BuildGroundTest extends Field[Parameters => GroundTest]

case class GroundTestTileSettings(
  uncached: Int = 0, cached: Int = 0, ptw: Int = 0, maxXacts: Int = 1)
case object GroundTestKey extends Field[Seq[GroundTestTileSettings]]

trait HasGroundTestConstants {
  val timeoutCodeBits = 4
  val errorCodeBits = 4
}

trait HasGroundTestParameters {
  implicit val p: Parameters
  val tileSettings = p(GroundTestKey)(p(TileId))
  val nUncached = tileSettings.uncached
  val nCached = tileSettings.cached
  val nPTW = tileSettings.ptw
  val memStart = p(ExtMem).base
  val memStartBlock = memStart >> p(CacheBlockOffsetBits)
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
  s2_resp.pte.a := Bool(false)
  s2_resp.pte.g := Bool(false)
  s2_resp.pte.u := Bool(true)
  s2_resp.pte.r := Bool(true)
  s2_resp.pte.w := Bool(true)
  s2_resp.pte.x := Bool(false)
  s2_resp.pte.v := Bool(true)

  io.requestors.zipWithIndex.foreach { case (requestor, i) =>
    requestor.resp.valid := s2_valid && s2_chosen === UInt(i)
    requestor.resp.bits := s2_resp
    requestor.status.vm := UInt("b01000")
    requestor.status.prv := UInt(PRV.S)
    requestor.status.debug := Bool(false)
    requestor.status.mprv  := Bool(true)
    requestor.status.mpp := UInt(0)
    requestor.ptbr.asid := UInt(0)
    requestor.ptbr.ppn := UInt(0)
    requestor.invalidate := Bool(false)
  }
}

class GroundTestStatus extends Bundle with HasGroundTestConstants {
  val finished = Bool(OUTPUT)
  val timeout = Valid(UInt(width = timeoutCodeBits))
  val error = Valid(UInt(width = errorCodeBits))
}

class GroundTestIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
    with HasGroundTestParameters {
  val cache = Vec(nCached, new HellaCacheIO)
  val mem = Vec(nUncached, new ClientUncachedTileLinkIO)
  val ptw = Vec(nPTW, new TLBPTWIO)
  val status = new GroundTestStatus
}

abstract class GroundTest(implicit val p: Parameters) extends Module
    with HasGroundTestParameters {
  val io = new GroundTestIO
}

class GroundTestTile(implicit val p: Parameters) extends LazyModule with HasGroundTestParameters {
  val dcacheParams = p.alterPartial({ case CacheName => CacheName("L1D") })
  val slave = None
  val dcache = HellaCache(p(DCacheKey))(dcacheParams)
  val ucLegacy = LazyModule(new TLLegacy()(p))

   val cachedOut = TLOutputNode()
   val uncachedOut = TLOutputNode()
   cachedOut := dcache.node
   uncachedOut := TLHintHandler()(ucLegacy.node)
   val masterNodes = List(cachedOut, uncachedOut)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val cached = cachedOut.bundleOut
      val uncached = uncachedOut.bundleOut
      val success = Bool(OUTPUT)
    }

    val test = p(BuildGroundTest)(dcacheParams)

    val ptwPorts = ListBuffer.empty ++= test.io.ptw
    val uncachedArbPorts = ListBuffer.empty ++= test.io.mem

    if (nCached > 0) {
      val dcacheArb = Module(new HellaCacheArbiter(nCached)(dcacheParams))

      dcacheArb.io.requestor.zip(test.io.cache).foreach {
        case (requestor, cache) =>
          val dcacheIF = Module(new SimpleHellaCacheIF()(dcacheParams))
          dcacheIF.io.requestor <> cache
          requestor <> dcacheIF.io.cache
      }
      dcache.module.io.cpu <> dcacheArb.io.mem

      // SimpleHellaCacheIF leaves invalidate_lr dangling, so we wire it to false
      dcache.module.io.cpu.invalidate_lr := Bool(false)

      ptwPorts += dcache.module.io.ptw
    }

    if (ptwPorts.size > 0) {
      val ptw = Module(new DummyPTW(ptwPorts.size))
      ptw.io.requestors <> ptwPorts
    }

    val uncachedArb = Module(new ClientUncachedTileLinkIOArbiter(uncachedArbPorts.size))
    uncachedArb.io.in <> uncachedArbPorts
    ucLegacy.module.io.legacy <> uncachedArb.io.out

    io.success := test.io.status.finished
  }
}
