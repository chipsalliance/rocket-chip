// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import tile.XLen
import tile.TileInterrupts
import uncore.tilelink2._
import util._

/** Widths of various points in the SoC */
case class TLBusConfig(beatBytes: Int)
case object CBusConfig extends Field[TLBusConfig]
case object L1toL2Config extends Field[TLBusConfig]

// These parameters apply to all caches, for now
case object CacheBlockBytes extends Field[Int]

/** L2 Broadcast Hub configuration */
case class BroadcastConfig(
  nTrackers:  Int     = 4,
  bufferless: Boolean = false)
case object BroadcastConfig extends Field[BroadcastConfig]

/** L2 memory subsystem configuration */
case class BankedL2Config(
  nMemoryChannels:  Int = 1,
  nBanksPerChannel: Int = 1,
  coherenceManager: (Parameters, CoreplexNetwork) => (TLInwardNode, TLOutwardNode) = { case (q, _) =>
    implicit val p = q
    val BroadcastConfig(nTrackers, bufferless) = p(BroadcastConfig)
    val bh = LazyModule(new TLBroadcast(p(CacheBlockBytes), nTrackers, bufferless))
    val ww = LazyModule(new TLWidthWidget(p(L1toL2Config).beatBytes))
    ww.node :*= bh.node
    (bh.node, ww.node)
  }) {
  val nBanks = nMemoryChannels*nBanksPerChannel
}
case object BankedL2Config extends Field[BankedL2Config]

/** The file to read the BootROM contents from */
case object BootROMFile extends Field[String]

trait HasCoreplexParameters {
  implicit val p: Parameters
  lazy val tilesParams = p(RocketTilesKey)
  lazy val cbusConfig = p(CBusConfig)
  lazy val l1tol2Config = p(L1toL2Config)
  lazy val nTiles = tilesParams.size
  lazy val l2Config = p(BankedL2Config)
}

case class CoreplexParameters(implicit val p: Parameters) extends HasCoreplexParameters

abstract class BareCoreplex(implicit p: Parameters) extends LazyModule with BindingScope

abstract class BareCoreplexBundle[+L <: BareCoreplex](_outer: L) extends GenericParameterizedBundle(_outer) {
  val outer = _outer
  implicit val p = outer.p
}

abstract class BareCoreplexModule[+L <: BareCoreplex, +B <: BareCoreplexBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = _io ()
}

abstract class BaseCoreplex(implicit p: Parameters) extends BareCoreplex
    with CoreplexNetwork
    with BankedL2CoherenceManagers {
  override lazy val module = new BaseCoreplexModule(this, () => new BaseCoreplexBundle(this))
}

class BaseCoreplexBundle[+L <: BaseCoreplex](_outer: L) extends BareCoreplexBundle(_outer)
    with CoreplexNetworkBundle
    with BankedL2CoherenceManagersBundle

class BaseCoreplexModule[+L <: BaseCoreplex, +B <: BaseCoreplexBundle[L]](_outer: L, _io: () => B) extends BareCoreplexModule(_outer, _io)
    with CoreplexNetworkModule
    with BankedL2CoherenceManagersModule
