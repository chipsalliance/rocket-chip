// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket._
import tile._
import uncore.tilelink2._
import util._

sealed trait ClockCrossing
case class SynchronousCrossing(params: BufferParams = BufferParams.default) extends ClockCrossing
case class RationalCrossing(direction: RationalDirection = FastToSlow) extends ClockCrossing
case class AsynchronousCrossing(depth: Int, sync: Int = 2) extends ClockCrossing

case object RocketTilesKey extends Field[Seq[RocketTileParams]]
case object RocketCrossing extends Field[ClockCrossing]

trait HasRocketTiles extends CoreplexRISCVPlatform {
  val module: HasRocketTilesModule

  private val crossing = p(RocketCrossing)
  val tileParams = p(RocketTilesKey)

  // Handle interrupts to be routed directly into each tile
  val localIntNodes = tileParams map { t =>
    (t.core.nLocalInterrupts > 0).option(IntInputNode())
  }

  // Make a function for each tile that will wire it to coreplex devices and crossbars,
  // according to the specified type of clock crossing.
  val wiringTuple = localIntNodes.zip(tileParams).zipWithIndex
  val rocketWires: Seq[HasRocketTilesBundle => Unit] = wiringTuple.map { case ((lip, c), i) =>
    val pWithExtra = p.alterPartial {
      case TileKey => c
      case BuildRoCC => c.rocc
      case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
    }

    val asyncIntXbar  = LazyModule(new IntXbar)
    val periphIntXbar = LazyModule(new IntXbar)
    val coreIntXbar   = LazyModule(new IntXbar)

    // Local Interrupts must be synchronized to the core clock
    // before being passed into this module.
    // This allows faster latency for interrupts which are already synchronized.
    // The CLINT and PLIC outputs interrupts that are synchronous to the periphery clock,
    // so may or may not need to be synchronized depending on the Tile's
    // synchronization type.
    // Debug interrupt is definitely asynchronous in all cases.

    asyncIntXbar.intnode  := debug.intnode                  // debug
    periphIntXbar.intnode := clint.intnode                  // msip+mtip
    periphIntXbar.intnode := plic.intnode                   // meip
    if (c.core.useVM) periphIntXbar.intnode := plic.intnode // seip
    lip.foreach { coreIntXbar.intnode := _ }                // lip

    crossing match {
      case SynchronousCrossing(params) => {
        val wrapper = LazyModule(new SyncRocketTile(c, i)(pWithExtra))
        val buffer = LazyModule(new TLBuffer(params))
        val fixer = LazyModule(new TLFIFOFixer)
        buffer.node :=* wrapper.masterNode
        fixer.node :=* buffer.node
        l1tol2.node :=* fixer.node
        wrapper.slaveNode :*= cbus.node
        wrapper.asyncIntNode  := asyncIntXbar.intnode
        wrapper.periphIntNode := periphIntXbar.intnode
        wrapper.coreIntNode   := coreIntXbar.intnode
        (io: HasRocketTilesBundle) => {
          // leave clock as default (simpler for hierarchical PnR)
          wrapper.module.io.hartid := UInt(i)
          wrapper.module.io.resetVector := io.resetVector
        }
      }
      case AsynchronousCrossing(depth, sync) => {
        val wrapper = LazyModule(new AsyncRocketTile(c, i)(pWithExtra))
        val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
        val source = LazyModule(new TLAsyncCrossingSource(sync))
        val fixer = LazyModule(new TLFIFOFixer)
        sink.node :=* wrapper.masterNode
        fixer.node :=* sink.node
        l1tol2.node :=* fixer.node
        wrapper.slaveNode :*= source.node
        wrapper.asyncIntNode  := asyncIntXbar.intnode
        wrapper.periphIntNode := periphIntXbar.intnode
        wrapper.coreIntNode   := coreIntXbar.intnode
        source.node :*= cbus.node
        (io: HasRocketTilesBundle) => {
          wrapper.module.clock := io.tcrs(i).clock
          wrapper.module.reset := io.tcrs(i).reset
          wrapper.module.io.hartid := UInt(i)
          wrapper.module.io.resetVector := io.resetVector
        }
      }
      case RationalCrossing(direction) => {
        val wrapper = LazyModule(new RationalRocketTile(c, i)(pWithExtra))
        val sink = LazyModule(new TLRationalCrossingSink(direction))
        val source = LazyModule(new TLRationalCrossingSource)
        val fixer = LazyModule(new TLFIFOFixer)
        sink.node :=* wrapper.masterNode
        fixer.node :=* sink.node
        l1tol2.node :=* fixer.node
        wrapper.slaveNode :*= source.node
        wrapper.asyncIntNode := asyncIntXbar.intnode
        wrapper.periphIntNode := periphIntXbar.intnode
        wrapper.coreIntNode   := coreIntXbar.intnode
        source.node :*= cbus.node
        (io: HasRocketTilesBundle) => {
          wrapper.module.clock := io.tcrs(i).clock
          wrapper.module.reset := io.tcrs(i).reset
          wrapper.module.io.hartid := UInt(i)
          wrapper.module.io.resetVector := io.resetVector
        }
      }
    }
  }
}

trait HasRocketTilesBundle extends CoreplexRISCVPlatformBundle {
  val outer: HasRocketTiles
  val local_interrupts = HeterogeneousBag(outer.localIntNodes.flatten.map(_.bundleIn))
  val tcrs = Vec(p(RocketTilesKey).size, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
}

trait HasRocketTilesModule extends CoreplexRISCVPlatformModule {
  val outer: HasRocketTiles
  val io: HasRocketTilesBundle
  outer.rocketWires.foreach { _(io) }
}
