// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config.Parameters

class RocketPlex(implicit p: Parameters) extends BaseCoreplex
    with CoreplexRISCVPlatform
    with HasRocketTiles {
  override lazy val module = new RocketPlexModule(this, () => new RocketPlexBundle(this))
}

class RocketPlexBundle[+L <: RocketPlex](_outer: L) extends BaseCoreplexBundle(_outer)
    with CoreplexRISCVPlatformBundle
    with HasRocketTilesBundle

class RocketPlexModule[+L <: RocketPlex, +B <: RocketPlexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with CoreplexRISCVPlatformModule
    with HasRocketTilesModule
