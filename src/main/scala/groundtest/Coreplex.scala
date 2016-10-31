package groundtest

import Chisel._
import cde.{Parameters}
import coreplex._

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex
    with DirectConnection {
  override lazy val module = new GroundTestCoreplexModule(this, () => new GroundTestCoreplexBundle(this))
}

class GroundTestCoreplexBundle[+L <: GroundTestCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)

class GroundTestCoreplexModule[+L <: GroundTestCoreplex, +B <: GroundTestCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with DirectConnectionModule {
  io.success := tiles.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
