// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket._
import uncore.tilelink2._

sealed trait ClockCrossing
case object Synchronous extends ClockCrossing
case object Rational extends ClockCrossing
case class Asynchronous(depth: Int, sync: Int = 2) extends ClockCrossing

case object RocketConfigs extends Field[Seq[RocketConfig]]
case object RocketCrossing extends Field[ClockCrossing]

trait HasRocketTiles extends CoreplexRISCVPlatform {
  val module: HasRocketTilesModule

  private val crossing = p(RocketCrossing)
  private val configs = p(RocketConfigs)
  private val pWithExtra = p.alterPartial {
    case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
    case PAddrBits => l1tol2.node.edgesIn(0).bundle.addressBits
  }

  private val rocketTileIntNodes = configs.map { _ => IntInternalOutputNode() }
  rocketTileIntNodes.foreach { _ := plic.intnode }

  private def wireInterrupts(x: TileInterrupts, i: Int) {
    x := clint.module.io.tiles(i)
    x.debug := debug.module.io.debugInterrupts(i)
    x.meip := rocketTileIntNodes(i).bundleOut(0)(0)
    x.seip.foreach { _ := rocketTileIntNodes(i).bundleOut(0)(1) }
  }

  val rocketWires: Seq[HasRocketTilesBundle => Unit] = configs.zipWithIndex.map { case (c, i) =>
    crossing match {
      case Synchronous => {
        val tile = LazyModule(new RocketTile(c)(pWithExtra))
        tile.masterNodes.foreach { l1tol2.node := TLBuffer()(_) }
        tile.slaveNode.foreach { _ := cbus.node }
        (io: HasRocketTilesBundle) => {
          // leave clock as default (simpler for hierarchical PnR)
          tile.module.io.hartid := UInt(i)
          tile.module.io.resetVector := io.resetVector
          wireInterrupts(tile.module.io.interrupts, i)
        }
      }
      case Asynchronous(depth, sync) => {
        val wrapper = LazyModule(new AsyncRocketTile(c)(pWithExtra))
        wrapper.masterNodes.foreach { l1tol2.node := TLAsyncCrossingSink(depth, sync)(_) }
        wrapper.slaveNode.foreach { _ := TLAsyncCrossingSource(sync)(cbus.node) }
        (io: HasRocketTilesBundle) => {
          wrapper.module.clock := io.tcrs(i).clock
          wrapper.module.reset := io.tcrs(i).reset
          wrapper.module.io.hartid := UInt(i)
          wrapper.module.io.resetVector := io.resetVector
          wireInterrupts(wrapper.module.io.interrupts, i)
        }
      }
      case Rational => {
        val wrapper = LazyModule(new RationalRocketTile(c)(pWithExtra))
        wrapper.masterNodes.foreach { l1tol2.node := TLRationalCrossingSink()(_) }
        wrapper.slaveNode.foreach { _ := TLRationalCrossingSource()(cbus.node) }
        (io: HasRocketTilesBundle) => {
          wrapper.module.clock := io.tcrs(i).clock
          wrapper.module.reset := io.tcrs(i).reset
          wrapper.module.io.hartid := UInt(i)
          wrapper.module.io.resetVector := io.resetVector
          wireInterrupts(wrapper.module.io.interrupts, i)
        }
      }
    }
  }
}

trait HasRocketTilesBundle extends CoreplexRISCVPlatformBundle {
  val outer: HasRocketTiles
  val tcrs = Vec(p(RocketConfigs).size, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
}

trait HasRocketTilesModule extends CoreplexRISCVPlatformModule {
  val outer: HasRocketTiles
  val io: HasRocketTilesBundle
  outer.rocketWires.foreach { _(io) }
}
