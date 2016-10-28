package groundtest

import Chisel._
import cde.{Parameters}
import coreplex._

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex
    with DirectConnection {
  override lazy val module = new GroundTestCoreplexModule(new GroundTestCoreplexBundle(this))
}

class GroundTestCoreplexBundle[+L <: GroundTestCoreplex](outer: L) extends BaseCoreplexBundle(outer)

class GroundTestCoreplexModule[+B <: GroundTestCoreplexBundle[GroundTestCoreplex]](io: B) extends BaseCoreplexModule(io)
    with DirectConnectionModule {
  io.success := tiles.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
