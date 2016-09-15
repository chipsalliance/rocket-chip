package groundtest

import Chisel._
import cde.{Parameters}
import coreplex.{CoreplexConfig, DefaultCoreplex}

class GroundTestCoreplex(tp: Parameters, tc: CoreplexConfig) extends DefaultCoreplex(tp, tc) {
  io.success := tileList.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
