package coreplex

import Chisel._
import config._
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.util._
import util._
import rocket._

/////

class DefaultCoreplex(implicit p: Parameters) extends BaseCoreplex
    with CoreplexRISCVPlatform
    with RocketPlex {
  override lazy val module = new DefaultCoreplexModule(this, () => new DefaultCoreplexBundle(this))
}

class DefaultCoreplexBundle[+L <: DefaultCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)
    with CoreplexRISCVPlatformBundle
    with RocketPlexBundle

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with CoreplexRISCVPlatformModule
    with RocketPlexModule

/////

class MultiClockCoreplex(implicit p: Parameters) extends BaseCoreplex
    with CoreplexRISCVPlatform
    with AsyncRocketPlex {
  override lazy val module = new MultiClockCoreplexModule(this, () => new MultiClockCoreplexBundle(this))
}

class MultiClockCoreplexBundle[+L <: MultiClockCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)
    with CoreplexRISCVPlatformBundle
    with AsyncRocketPlexBundle

class MultiClockCoreplexModule[+L <: MultiClockCoreplex, +B <: MultiClockCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with CoreplexRISCVPlatformModule
    with AsyncRocketPlexModule
