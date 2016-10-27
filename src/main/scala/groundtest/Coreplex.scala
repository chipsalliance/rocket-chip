package groundtest

import Chisel._
import cde.{Parameters}
import coreplex._

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex {
  override lazy val module = new GroundTestCoreplexModule(this, new GroundTestCoreplexBundle(this))
}

class GroundTestCoreplexBundle[+L <: GroundTestCoreplex](outer: L) extends BaseCoreplexBundle(outer)

class GroundTestCoreplexModule[+L <: GroundTestCoreplex, +B <: GroundTestCoreplexBundle[L]](outer: L, io: B) extends BaseCoreplexModule(outer, io)
    with DirectConnection {
  io.success := tiles.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
