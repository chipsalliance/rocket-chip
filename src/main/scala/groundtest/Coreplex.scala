package groundtest

import Chisel._
import cde.Parameters
import diplomacy._
import coreplex._
import uncore.devices.NTiles
import rocket.TileId
import uncore.tilelink.TLId

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex
    with BroadcastL2
    with DirectConnection {
  val tiles = (0 until p(NTiles)).map { i =>
    LazyModule(new GroundTestTile()(p.alterPartial({
      case TLId => "L1toL2"
      case TileId => i
    })))
  }
  override lazy val module = new GroundTestCoreplexModule(this, () => new GroundTestCoreplexBundle(this))
}

class GroundTestCoreplexBundle[+L <: GroundTestCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)

class GroundTestCoreplexModule[+L <: GroundTestCoreplex, +B <: GroundTestCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with DirectConnectionModule {
  io.success := outer.tiles.flatMap(_.module.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
