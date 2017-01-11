// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket.{TileInterrupts, XLen}
import uncore.tilelink2._
import uncore.util._
import util._

/** Widths of various points in the SoC */
case class TLBusConfig(beatBytes: Int)
case object CBusConfig extends Field[TLBusConfig]
case object L1toL2Config extends Field[TLBusConfig]

/** L2 Broadcast Hub configuration */
case class BroadcastConfig(
  nTrackers:  Int     = 4,
  bufferless: Boolean = false)
case object BroadcastConfig extends Field[BroadcastConfig]

/** L2 memory subsystem configuration */
case class BankedL2Config(
  nMemoryChannels:  Int = 1,
  nBanksPerChannel: Int = 1,
  coherenceManager: Parameters => (TLInwardNode, TLOutwardNode) = { case q =>
    implicit val p = q
    val BroadcastConfig(nTrackers, bufferless) = p(BroadcastConfig)
    val bh = LazyModule(new TLBroadcast(p(CacheBlockBytes), nTrackers, bufferless))
    (bh.node, TLWidthWidget(p(L1toL2Config).beatBytes)(bh.node))
  }) {
  val nBanks = nMemoryChannels*nBanksPerChannel
}
case object BankedL2Config extends Field[BankedL2Config]

/** The file to read the BootROM contents from */
case object BootROMFile extends Field[String]

trait HasCoreplexParameters {
  implicit val p: Parameters
  lazy val cbusConfig = p(CBusConfig)
  lazy val l1tol2Config = p(L1toL2Config)
  lazy val nTiles = p(uncore.devices.NTiles)
  lazy val hasSupervisor = p(rocket.UseVM)
  lazy val l2Config = p(BankedL2Config)
}

case class CoreplexParameters(implicit val p: Parameters) extends HasCoreplexParameters

abstract class BareCoreplex(implicit p: Parameters) extends LazyModule

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
