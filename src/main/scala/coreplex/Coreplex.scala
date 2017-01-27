// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import util._

/////

class DefaultCoreplex(implicit p: Parameters) extends BaseCoreplex
    with CoreplexRISCVPlatform
    with HasL2MasterPort
    with HasRocketTiles {
  override lazy val module = new DefaultCoreplexModule(this, () => new DefaultCoreplexBundle(this))
}

class DefaultCoreplexBundle[+L <: DefaultCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)
    with CoreplexRISCVPlatformBundle
    with HasL2MasterPortBundle
    with HasRocketTilesBundle

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with CoreplexRISCVPlatformModule
    with HasL2MasterPortModule
    with HasRocketTilesModule
