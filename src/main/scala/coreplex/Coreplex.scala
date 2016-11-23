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

trait L2MasterPort extends CoreplexNetwork
{
  val module: L2MasterPortModule
  val l2in = TLInputNode()
  l1tol2.node := l2in
}

trait L2MasterPortBundle extends CoreplexNetworkBundle
{
  val outer: L2MasterPort
  val l2in = outer.l2in.bundleIn
}

trait L2MasterPortModule extends CoreplexNetworkModule
{
  val outer: L2MasterPort
  val io: L2MasterPortBundle
}

/////

class DefaultCoreplex(implicit p: Parameters) extends BaseCoreplex
    with CoreplexRISCVPlatform
    with L2MasterPort
    with RocketTiles {
  override lazy val module = new DefaultCoreplexModule(this, () => new DefaultCoreplexBundle(this))
}

class DefaultCoreplexBundle[+L <: DefaultCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)
    with CoreplexRISCVPlatformBundle
    with L2MasterPortBundle
    with RocketTilesBundle

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with CoreplexRISCVPlatformModule
    with L2MasterPortModule
    with RocketTilesModule

/////

class MultiClockCoreplex(implicit p: Parameters) extends BaseCoreplex
    with CoreplexRISCVPlatform
    with L2MasterPort
    with AsyncRocketTiles {
  override lazy val module = new MultiClockCoreplexModule(this, () => new MultiClockCoreplexBundle(this))
}

class MultiClockCoreplexBundle[+L <: MultiClockCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)
    with CoreplexRISCVPlatformBundle
    with L2MasterPortBundle
    with AsyncRocketTilesBundle

class MultiClockCoreplexModule[+L <: MultiClockCoreplex, +B <: MultiClockCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with CoreplexRISCVPlatformModule
    with L2MasterPortModule
    with AsyncRocketTilesModule
