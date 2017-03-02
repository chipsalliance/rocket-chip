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

  private val rocketTileIntNodes = configs.map { _ => IntInternalOutputNode(IntSinkPortSimple(ports = 2)) }
  rocketTileIntNodes.foreach { n =>
    n := plic.intnode
    n := clint.intnode
  }

  private def wireInterrupts(x: TileInterrupts, i: Int) {
    x.debug := debug.module.io.debugInterrupts(i)
    x.meip := rocketTileIntNodes(i).bundleOut(0)(0)
    x.seip.foreach { _ := rocketTileIntNodes(i).bundleOut(0)(1) } // optional
    x.msip := rocketTileIntNodes(i).bundleOut(1)(0)
    x.mtip := rocketTileIntNodes(i).bundleOut(1)(1)
  }

  val rocketWires: Seq[HasRocketTilesBundle => Unit] = configs.zipWithIndex.map { case (c, i) =>
    val pWithExtra = p.alterPartial {
      case TileKey => c
      case BuildRoCC => c.rocc
      case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
      case PAddrBits => l1tol2.node.edgesIn(0).bundle.addressBits
    }

    crossing match {
      case Synchronous => {
        val tile = LazyModule(new RocketTile(c, i)(pWithExtra))
        val buffer = LazyModule(new TLBuffer)
        buffer.node :=* tile.masterNode
        l1tol2.node :=* buffer.node
        tile.slaveNode :*= cbus.node
        ResourceBinding {
          rocketTileIntNodes(i).edgesIn(0).source.sources.flatMap(_.resources).foreach { r =>
            r.bind(tile.device, ResourceInt(11)) // meip
            if (c.core.useVM) r.bind(tile.device, ResourceInt(9)) // seip
          }
          rocketTileIntNodes(i).edgesIn(1).source.sources.flatMap(_.resources).foreach { r =>
            r.bind(tile.device, ResourceInt(3)) // msip
            r.bind(tile.device, ResourceInt(7)) // mtip
          }
        }
        (io: HasRocketTilesBundle) => {
          // leave clock as default (simpler for hierarchical PnR)
          tile.module.io.hartid := UInt(i)
          tile.module.io.resetVector := io.resetVector
          wireInterrupts(tile.module.io.interrupts, i)
        }
      }
      case Asynchronous(depth, sync) => {
        val wrapper = LazyModule(new AsyncRocketTile(c, i)(pWithExtra))
        val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
        val source = LazyModule(new TLAsyncCrossingSource(sync))
        sink.node :=* wrapper.masterNode
        l1tol2.node :=* sink.node
        wrapper.slaveNode :*= source.node
        source.node :*= cbus.node
        ResourceBinding {
          rocketTileIntNodes(i).edgesIn(0).source.sources.flatMap(_.resources).foreach { r =>
            r.bind(wrapper.rocket.device, ResourceInt(11)) // meip
            if (c.core.useVM) r.bind(wrapper.rocket.device, ResourceInt(9)) // seip
          }
          rocketTileIntNodes(i).edgesIn(1).source.sources.flatMap(_.resources).foreach { r =>
            r.bind(wrapper.rocket.device, ResourceInt(3)) // msip
            r.bind(wrapper.rocket.device, ResourceInt(7)) // mtip
          }
        }
        (io: HasRocketTilesBundle) => {
          wrapper.module.clock := io.tcrs(i).clock
          wrapper.module.reset := io.tcrs(i).reset
          wrapper.module.io.hartid := UInt(i)
          wrapper.module.io.resetVector := io.resetVector
          wireInterrupts(wrapper.module.io.interrupts, i)
        }
      }
      case Rational => {
        val wrapper = LazyModule(new RationalRocketTile(c, i)(pWithExtra))
        val sink = LazyModule(new TLRationalCrossingSink(util.FastToSlow))
        val source = LazyModule(new TLRationalCrossingSource)
        sink.node :=* wrapper.masterNode
        l1tol2.node :=* sink.node
        wrapper.slaveNode :*= source.node
        source.node :*= cbus.node
        ResourceBinding {
          rocketTileIntNodes(i).edgesIn(0).source.sources.flatMap(_.resources).foreach { r =>
            r.bind(wrapper.rocket.device, ResourceInt(11)) // meip
            if (c.core.useVM) r.bind(wrapper.rocket.device, ResourceInt(9)) // seip
          }
          rocketTileIntNodes(i).edgesIn(1).source.sources.flatMap(_.resources).foreach { r =>
            r.bind(wrapper.rocket.device, ResourceInt(3)) // msip
            r.bind(wrapper.rocket.device, ResourceInt(7)) // mtip
          }
        }
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
