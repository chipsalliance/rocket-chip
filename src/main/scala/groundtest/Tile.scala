// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.rocket.{HellaCache, RocketCoreParams}
import freechips.rocketchip.tile._
import scala.collection.mutable.ListBuffer

trait GroundTestTileParams extends TileParams {
  val memStart: BigInt
  val maxRequests: Int
  val numGens: Int

  def build(i: Int, p: Parameters): GroundTestTile
  
  val icache = None
  val btb = None
  val rocc = Nil
  val core = RocketCoreParams(nPMPs = 0) //TODO remove this
  val cached = if(dcache.isDefined) 1 else 0
  val dataScratchpadBytes = 0
}

case object GroundTestTilesKey extends Field[Seq[GroundTestTileParams]]

abstract class GroundTestTile(params: GroundTestTileParams)(implicit p: Parameters) extends BaseTile(params)(p) {
  val slave = None
  val dcacheOpt = params.dcache.map { dc => LazyModule(HellaCache(0, dc.nMSHRs == 0)) }
  dcacheOpt.foreach { tileBus.node := _.node }

  override lazy val module = new GroundTestTileModule(this, () => new GroundTestTileBundle(this))
}

class GroundTestTileBundle[+L <: GroundTestTile](_outer: L) extends BaseTileBundle(_outer) {
  val status = new GroundTestStatus
}

class GroundTestTileModule[+L <: GroundTestTile, +B <: GroundTestTileBundle[L]](_outer: L, _io: () => B) extends BaseTileModule(_outer, _io) {

  outer.dcacheOpt foreach { dcache =>
    val ptw = Module(new DummyPTW(1))
    ptw.io.requestors.head <> dcache.module.io.ptw
  }
}
