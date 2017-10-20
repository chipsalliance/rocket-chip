// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

// TODO: how specific are these to RocketTiles?
case class TileMasterPortParams(
    addBuffers: Int = 0,
    blockerCtrlAddr: Option[BigInt] = None,
    cork: Option[Boolean] = None) {
  def adapt(coreplex: HasPeripheryBus)
           (masterNode: TLOutwardNode)
           (implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val tile_master_cork = cork.map(u => (LazyModule(new TLCacheCork(unsafe = u))))
    val tile_master_blocker =
      blockerCtrlAddr
        .map(BasicBusBlockerParams(_, coreplex.pbus.beatBytes, coreplex.sbus.beatBytes, deadlock = true))
        .map(bp => LazyModule(new BasicBusBlocker(bp)))
    val tile_master_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.allUncacheable))
    val tile_master_buffer = LazyModule(new TLBufferChain(addBuffers))

    tile_master_blocker.foreach { _.controlNode := coreplex.pbus.toVariableWidthSlaves }

    val node: Option[TLNode] = SourceCardinality { implicit p => 
      TLNodeChain(List(
        Some(tile_master_buffer.node),
        Some(tile_master_fixer.node),
        tile_master_blocker.map(_.node),
        tile_master_cork.map(_.node)).flatten)
    }
    node.foreach { _ :=* masterNode }
    node.getOrElse(masterNode)
  }
}

case class TileSlavePortParams(
    addBuffers: Int = 0,
    blockerCtrlAddr: Option[BigInt] = None) {
  def adapt(coreplex: HasPeripheryBus)
           (masterNode: TLOutwardNode)
           (implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val tile_slave_blocker =
      blockerCtrlAddr
        .map(BasicBusBlockerParams(_, coreplex.pbus.beatBytes, coreplex.sbus.beatBytes))
        .map(bp => LazyModule(new BasicBusBlocker(bp)))
    val tile_slave_buffer = LazyModule(new TLBufferChain(addBuffers))

    tile_slave_blocker.foreach { _.controlNode := coreplex.pbus.toVariableWidthSlaves }

    val node: Option[TLNode] = SinkCardinality { implicit p => 
      TLNodeChain(List(
        Some(tile_slave_buffer.node),
        tile_slave_blocker.map(_.node)).flatten)
    }
    node.foreach { _ :*= masterNode }
    node.getOrElse(masterNode)
  }
}

case class RocketCrossingParams(
    crossingType: CoreplexClockCrossing = SynchronousCrossing(),
    master: TileMasterPortParams = TileMasterPortParams(),
    slave: TileSlavePortParams = TileSlavePortParams()) {
  def knownRatio: Option[Int] = crossingType match {
    case RationalCrossing(_) => Some(2)
    case _ => None
  }
}

case object RocketTilesKey extends Field[Seq[RocketTileParams]](Nil)
case object RocketCrossingKey extends Field[Seq[RocketCrossingParams]](List(RocketCrossingParams()))

trait HasRocketTiles extends HasTiles
    with HasPeripheryBus
    with HasPeripheryPLIC
    with HasPeripheryClint
    with HasPeripheryDebug {
  val module: HasRocketTilesModuleImp

  protected val tileParams = p(RocketTilesKey)
  private val NumRocketTiles = tileParams.size
  private val crossingParams = p(RocketCrossingKey)
  private val crossings = crossingParams.size match {
    case 1 => List.fill(NumRocketTiles) { crossingParams.head }
    case NumRocketTiles => crossingParams
    case _ => throw new Exception("RocketCrossingKey.size must == 1 or == RocketTilesKey.size")
  }

  // Make a wrapper for each tile that will wire it to coreplex devices and crossbars,
  // according to the specified type of clock crossing.
  private val crossingTuples = localIntNodes.zip(tileParams).zip(crossings)
  val tiles: Seq[BaseTile] = crossingTuples.map { case ((lip, tp), crossing) =>
    val pWithExtra = p.alterPartial {
      case TileKey => tp
      case BuildRoCC => tp.rocc
      case SharedMemoryTLEdge => sharedMemoryTLEdge
      case RocketCrossingKey => List(crossing)
    }

    val wrapper = crossing.crossingType match {
      case SynchronousCrossing(params) => {
        val wrapper = LazyModule(new SyncRocketTile(tp)(pWithExtra))
        sbus.fromSyncTiles(params, crossing.master.adapt(this) _, tp.name) :=* wrapper.masterNode
        FlipRendering { implicit p => wrapper.slaveNode:*= pbus.toSyncSlaves(crossing.slave.adapt(this) _, tp.name) }
        wrapper
      }
      case AsynchronousCrossing(depth, sync) => {
        val wrapper = LazyModule(new AsyncRocketTile(tp)(pWithExtra))
        sbus.fromAsyncTiles(depth, sync, crossing.master.adapt(this) _, tp.name) :=* wrapper.masterNode
        FlipRendering { implicit p => wrapper.slaveNode :*= pbus.toAsyncSlaves(sync, crossing.slave.adapt(this) _, tp.name) }
        wrapper
      }
      case RationalCrossing(direction) => {
        val wrapper = LazyModule(new RationalRocketTile(tp)(pWithExtra))
        sbus.fromRationalTiles(direction, crossing.master.adapt(this) _, tp.name) :=* wrapper.masterNode
        FlipRendering { implicit p => wrapper.slaveNode :*= pbus.toRationalSlaves(crossing.slave.adapt(this) _, tp.name) }
        wrapper
      }
    }
    tp.name.foreach(wrapper.suggestName) // Try to stabilize this name for downstream tools

    // Local Interrupts must be synchronized to the core clock
    // before being passed into this module.
    // This allows faster latency for interrupts which are already synchronized.
    // The CLINT and PLIC outputs interrupts that are synchronous to the periphery clock,
    // so may or may not need to be synchronized depending on the Tile's crossing type.
    // Debug interrupt is definitely asynchronous in all cases.
    val asyncIntXbar  = LazyModule(new IntXbar)
    asyncIntXbar.suggestName("asyncIntXbar")
    asyncIntXbar.intnode  := debug.intnode                  // debug
    wrapper.asyncIntNode  := asyncIntXbar.intnode

    val periphIntXbar = LazyModule(new IntXbar)
    periphIntXbar.suggestName("periphIntXbar")
    periphIntXbar.intnode := clint.intnode                  // msip+mtip
    periphIntXbar.intnode := plic.intnode                   // meip
    if (tp.core.useVM) periphIntXbar.intnode := plic.intnode // seip
    wrapper.periphIntNode := periphIntXbar.intnode

    val coreIntXbar = LazyModule(new IntXbar)
    coreIntXbar.suggestName("coreIntXbar")
    lip.foreach { coreIntXbar.intnode := _ }                // lip
    wrapper.coreIntNode   := coreIntXbar.intnode

    wrapper.intOutputNode.foreach { case int =>
      val rocketIntXing = LazyModule(new IntXing(wrapper.outputInterruptXingLatency))
      FlipRendering { implicit p => rocketIntXing.intnode := int }
      plic.intnode := rocketIntXing.intnode
    }

    wrapper
  }
}

trait HasRocketTilesModuleImp extends HasTilesModuleImp
    with HasPeripheryDebugModuleImp {
  val outer: HasRocketTiles
}

class RocketCoreplex(implicit p: Parameters) extends BaseCoreplex
    with HasRocketTiles {
  override lazy val module = new RocketCoreplexModule(this)
}

class RocketCoreplexModule[+L <: RocketCoreplex](_outer: L) extends BaseCoreplexModule(_outer)
    with HasRocketTilesModuleImp
