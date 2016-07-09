package rocketchip

import Chisel._
import cde.{Parameters, Field}
import groundtest._
import uncore.tilelink._

trait HasDirectMemtestParameters {
  implicit val p: Parameters
  lazy val tileSettings = p(GroundTestKey)(0)
  lazy val nGens = tileSettings.uncached
}

class MemtestGenerators(implicit val p: Parameters) extends Module
    with HasDirectMemtestParameters {
  val io = new Bundle {
    val mem = Vec(nGens, new ClientUncachedTileLinkIO)
    val finished = Bool(OUTPUT)
  }

  val generators =
    (0 until nGens).map(id => Module(new UncachedTileLinkGenerator(id)))

  io.mem <> generators.map(_.io.mem)
  io.finished := generators.map(_.io.finished).reduce(_ && _)
}

class DirectMemtestTop(topParams: Parameters) extends Module
    with HasTopLevelParameters
    with HasDirectMemtestParameters {
  implicit val p = topParams
  val io = new TopIO

  // Not using the debug 
  io.debug.req.ready := Bool(false)
  io.debug.resp.valid := Bool(false)

  require(io.mmio_axi.isEmpty && io.mmio_ahb.isEmpty && io.mmio_tl.isEmpty)
  require(io.mem_ahb.isEmpty && io.mem_tl.isEmpty)
  require(nBanksPerMemChannel == nGens)
  require(nMemChannels == 1)
  require(nTiles == 1)

  val memtest = Module(new MemtestGenerators()(outermostParams))
  val mem_ic = Module(new TileLinkMemoryInterconnect(nGens, 1)(outermostParams))

  mem_ic.io.in <> memtest.io.mem
  io.mem_axi.zip(mem_ic.io.out).foreach { case (nasti, tl) =>
    TopUtils.connectTilelinkNasti(nasti, tl)(outermostParams)
  }
  when (memtest.io.finished) { stop() }
}
