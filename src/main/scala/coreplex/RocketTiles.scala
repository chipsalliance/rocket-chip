// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket._
import tile._
import uncore.tilelink2._

sealed trait ClockCrossing
case object Synchronous extends ClockCrossing
case object Rational extends ClockCrossing
case class Asynchronous(depth: Int, sync: Int = 2) extends ClockCrossing

case object RocketTilesKey extends Field[Seq[RocketTileParams]]
case object RocketCrossing extends Field[ClockCrossing]

trait HasRocketTiles extends CoreplexRISCVPlatform {
  val module: HasRocketTilesModule

  private val crossing = p(RocketCrossing)
  private val configs = p(RocketTilesKey)

  // TODO: hack to fix deduplication; see PR https://github.com/ucb-bar/berkeley-hardfloat/pull/14
  hardfloat.consts

  val rocketWires: Seq[HasRocketTilesBundle => Unit] = configs.zipWithIndex.map { case (c, i) =>
    val pWithExtra = p.alterPartial {
      case TileKey => c
      case BuildRoCC => c.rocc
      case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
    }

    val intBar = LazyModule(new IntXbar)
    intBar.intnode := debug.intnode // Debug Interrupt
    intBar.intnode := clint.intnode // msip+mtip
    intBar.intnode := plic.intnode // meip
    if (c.core.useVM) intBar.intnode := plic.intnode // seip

    crossing match {
      case Synchronous => {
        val wrapper = LazyModule(new SyncRocketTile(c, i)(pWithExtra))
        val buffer = LazyModule(new TLBuffer)
        val fixer = LazyModule(new TLFIFOFixer)
        buffer.node :=* wrapper.masterNode
        fixer.node :=* buffer.node
        l1tol2.node :=* fixer.node
        wrapper.slaveNode :*= cbus.node
        wrapper.intNode := intBar.intnode
        (io: HasRocketTilesBundle) => {
          // leave clock as default (simpler for hierarchical PnR)
          wrapper.module.io.hartid := UInt(i)
          wrapper.module.io.resetVector := io.resetVector
        }
      }
      case Asynchronous(depth, sync) => {
        val wrapper = LazyModule(new AsyncRocketTile(c, i)(pWithExtra))
        val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
        val source = LazyModule(new TLAsyncCrossingSource(sync))
        val fixer = LazyModule(new TLFIFOFixer)
        sink.node :=* wrapper.masterNode
        fixer.node :=* sink.node
        l1tol2.node :=* fixer.node
        wrapper.slaveNode :*= source.node
        wrapper.intNode := intBar.intnode
        source.node :*= cbus.node
        (io: HasRocketTilesBundle) => {
          wrapper.module.clock := io.tcrs(i).clock
          wrapper.module.reset := io.tcrs(i).reset
          wrapper.module.io.hartid := UInt(i)
          wrapper.module.io.resetVector := io.resetVector
        }
      }
      case Rational => {
        val wrapper = LazyModule(new RationalRocketTile(c, i)(pWithExtra))
        val sink = LazyModule(new TLRationalCrossingSink(util.FastToSlow))
        val source = LazyModule(new TLRationalCrossingSource)
        val fixer = LazyModule(new TLFIFOFixer)
        sink.node :=* wrapper.masterNode
        fixer.node :=* sink.node
        l1tol2.node :=* fixer.node
        wrapper.slaveNode :*= source.node
        wrapper.intNode := intBar.intnode
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
