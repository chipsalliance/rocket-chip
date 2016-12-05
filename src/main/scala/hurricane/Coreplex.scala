package hurricane

import coreplex._
import cde.Parameters
import Chisel._

class MultiClockGroundTestCoreplex(c: CoreplexConfig)
    (implicit p: Parameters) extends MultiClockCoreplex(c)(p) {
  override lazy val module = Module(new MultiClockGroundTestCoreplexModule(c, this, new MultiClockGroundTestCoreplexBundle(c)(p))(p))
}

class MultiClockGroundTestCoreplexBundle(c: CoreplexConfig)
  (implicit p: Parameters) extends MultiClockCoreplexBundle(c)(p)

class MultiClockGroundTestCoreplexModule
    [+L <: MultiClockGroundTestCoreplex, +B <: MultiClockGroundTestCoreplexBundle]
    (c: CoreplexConfig, l: L, b: => B)(implicit p: Parameters)
    extends MultiClockCoreplexModule(c, l, b)(p) {
  io.success := tiles.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
