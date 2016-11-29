// See LICENSE.SiFive for license details.

package groundtest

import Chisel._
import config._
import diplomacy._
import coreplex._
import uncore.devices.NTiles
import uncore.tilelink2._
import uncore.tilelink.TLId

case object TileId extends Field[Int]

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex {
  val tiles = List.tabulate(p(NTiles)) { i =>
    LazyModule(new GroundTestTile()(p.alterPartial({
      case TLId => "L1toL2"
      case TileId => i
    })))
  }
  tiles.foreach { lm =>
    l1tol2.node := lm.cachedOut
    l1tol2.node := lm.uncachedOut
  }

  val cbusRAM = LazyModule(new TLRAM(AddressSet(testRamAddr, 0xffff), false, cbus_beatBytes))
  cbusRAM.node := TLFragmenter(cbus_beatBytes, cbus_lineBytes)(cbus.node)

  override lazy val module = new GroundTestCoreplexModule(this, () => new GroundTestCoreplexBundle(this))
}

class GroundTestCoreplexBundle[+L <: GroundTestCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)
{
  val success = Bool(OUTPUT)
}

class GroundTestCoreplexModule[+L <: GroundTestCoreplex, +B <: GroundTestCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io) {
  io.success := outer.tiles.map(_.module.io.success).reduce(_&&_)
}
