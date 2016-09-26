package groundtest

import Chisel._
import cde.{Parameters}
import coreplex._

class GroundTestCoreplex(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplex(c)(p) {
  override lazy val module = Module(new GroundTestCoreplexModule(c, this, new GroundTestCoreplexBundle(c)(p))(p))
}

class GroundTestCoreplexBundle(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplexBundle(c)(p)

class GroundTestCoreplexModule[+L <: GroundTestCoreplex, +B <: GroundTestCoreplexBundle]
    (c: CoreplexConfig, l: L, b: => B)(implicit p: Parameters)
    extends BaseCoreplexModule(c, l, b)(p) with DirectConnection {
  io.success := tiles.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
