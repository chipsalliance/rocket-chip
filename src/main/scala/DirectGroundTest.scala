package rocketchip

import Chisel._
import cde.{Parameters, Field}
import groundtest._
import uncore.tilelink._
import uncore.agents._

class DirectGroundTestTop(topParams: Parameters) extends Module
    with HasTopLevelParameters {
  implicit val p = topParams
  val io = new TopIO

  // Not using the debug 
  io.debug.req.ready := Bool(false)
  io.debug.resp.valid := Bool(false)

  require(io.mmio_axi.isEmpty && io.mmio_ahb.isEmpty && io.mmio_tl.isEmpty)
  require(io.mem_ahb.isEmpty && io.mem_tl.isEmpty)
  require(nMemChannels == 1)
  require(nTiles == 1)

  val test = p(BuildGroundTest)(outermostParams.alterPartial({
    case GroundTestId => 0
    case CacheName => "L1D"
  }))
  require(test.io.cache.size == 0)
  require(test.io.mem.size == nBanksPerMemChannel)
  require(test.io.ptw.size == 0)

  when (test.io.finished) { stop() }

  val mem_ic = Module(new TileLinkMemoryInterconnect(
    nBanksPerMemChannel, nMemChannels)(outermostParams))

  mem_ic.io.in <> test.io.mem
  io.mem_axi.zip(mem_ic.io.out).foreach { case (nasti, tl) =>
    TopUtils.connectTilelinkNasti(nasti, tl)(outermostParams)
  }
}
