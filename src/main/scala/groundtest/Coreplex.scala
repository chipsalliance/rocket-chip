// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._

import scala.math.max

case object TileId extends Field[Int]

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex {
  val tileParams = p(GroundTestTilesKey)
  val tiles = tileParams.zipWithIndex.map { case(c, i) => LazyModule(
    c.build(i, p.alterPartial {
      case TileKey => c
      case SharedMemoryTLEdge => tile_splitter.node.edgesIn(0)
    })
  )}

  val fixer = LazyModule(new TLFIFOFixer)
  tile_splitter.node :=* fixer.node
  tiles.foreach { fixer.node :=* _.masterNode }

  val pbusRAM = LazyModule(new TLRAM(AddressSet(testRamAddr, 0xffff), false, pbusBeatBytes))
  pbusRAM.node := TLFragmenter(pbusBeatBytes, pbusBlockBytes)(pbus.node)

  override lazy val module = new GroundTestCoreplexModule(this, () => new GroundTestCoreplexBundle(this))
}

class GroundTestCoreplexBundle[+L <: GroundTestCoreplex](_outer: L) extends BaseCoreplexBundle(_outer) {
  val success = Bool(OUTPUT)
}

class GroundTestCoreplexModule[+L <: GroundTestCoreplex, +B <: GroundTestCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io) {

  outer.tiles.zipWithIndex.map { case(t, i) => t.module.io.hartid := UInt(i) }

  val status = DebugCombiner(outer.tiles.map(_.module.io.status))
  io.success := status.finished
}
