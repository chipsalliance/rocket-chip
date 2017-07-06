// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case object RocketTilesKey extends Field[Seq[RocketTileParams]]
case object RocketCrossing extends Field[CoreplexClockCrossing]

trait HasRocketTiles extends HasSystemBus
    with HasPeripheryBus
    with HasPeripheryPLIC
    with HasPeripheryClint
    with HasPeripheryDebug {
  val module: HasRocketTilesModuleImp

  private val crossing = p(RocketCrossing)
  private val tileParams = p(RocketTilesKey)
  val nRocketTiles = tileParams.size

  // Handle interrupts to be routed directly into each tile
  val localIntNodes = tileParams map { t =>
    (t.core.nLocalInterrupts > 0).option(IntInputNode())
  }

  // Make a wrapper for each tile that will wire it to coreplex devices and crossbars,
  // according to the specified type of clock crossing.
  val wiringTuple = localIntNodes.zip(tileParams).zipWithIndex
  val wrappers: Seq[RocketTileWrapper] = wiringTuple.map { case ((lip, c), i) =>
    val pWithExtra = p.alterPartial {
      case TileKey => c
      case BuildRoCC => c.rocc
      case SharedMemoryTLEdge => sharedMemoryTLEdge
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
        sbus.inwardBufNode :=* wrapper.masterNode 
        wrapper.slaveNode :*= pbus.outwardBufNode
        wrapper.asyncIntNode  := asyncIntXbar.intnode
        wrapper.periphIntNode := periphIntXbar.intnode
        wrapper.coreIntNode   := coreIntXbar.intnode
        wrapper
      }
      case AsynchronousCrossing(depth, sync) => {
        val wrapper = LazyModule(new AsyncRocketTile(c, i)(pWithExtra))
        val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
        val source = LazyModule(new TLAsyncCrossingSource(sync))
        sink.node :=* wrapper.masterNode
        sbus.inwardFIFONode :=* sink.node
        source.node :*= pbus.outwardBufNode
        wrapper.slaveNode :*= source.node
        wrapper.asyncIntNode  := asyncIntXbar.intnode
        wrapper.periphIntNode := periphIntXbar.intnode
        wrapper.coreIntNode   := coreIntXbar.intnode
        wrapper
      }
      case RationalCrossing(direction) => {
        val wrapper = LazyModule(new RationalRocketTile(c, i)(pWithExtra))
        val sink = LazyModule(new TLRationalCrossingSink(direction))
        val source = LazyModule(new TLRationalCrossingSource)
        sink.node :=* wrapper.masterNode
        sbus.inwardFIFONode :=* sink.node
        source.node :*= pbus.outwardBufNode
        wrapper.slaveNode :*= source.node
        wrapper.asyncIntNode  := asyncIntXbar.intnode
        wrapper.periphIntNode := periphIntXbar.intnode
        wrapper.coreIntNode   := coreIntXbar.intnode
        wrapper
      }
    }
  }
}

class ClockedRocketTileInputs(implicit val p: Parameters) extends ParameterizedBundle
    with HasExternallyDrivenTileConstants
    with Clocked

trait HasRocketTilesBundle {
  val rocket_tile_inputs: Vec[ClockedRocketTileInputs]
}

trait HasRocketTilesModuleImp extends LazyMultiIOModuleImp
    with HasRocketTilesBundle
    with HasGlobalResetVectorWire
    with HasPeripheryDebugModuleImp {
  val outer: HasRocketTiles
  val rocket_tile_inputs = Wire(Vec(outer.nRocketTiles, new ClockedRocketTileInputs))

  // Unconditionally wire up the non-diplomatic tile inputs
  outer.wrappers.map(_.module).zip(rocket_tile_inputs).foreach { case(tile, wire) =>
    tile.clock := wire.clock
    tile.reset := wire.reset
    tile.io.hartid := wire.hartid
    tile.io.resetVector := wire.resetVector
  }

  // Default values for tile inputs; may be overriden in other traits
  rocket_tile_inputs.zipWithIndex.foreach { case(wire, i) =>
    wire.clock := clock
    wire.reset := reset
    wire.hartid := UInt(i)
    wire.resetVector := global_reset_vector
  }
}

class RocketCoreplex(implicit p: Parameters) extends BaseCoreplex
    with HasRTC
    with HasPeripheryErrorSlave
    with HasRocketTiles {
  override lazy val module = new RocketCoreplexModule(this)
}

class RocketCoreplexModule[+L <: RocketCoreplex](_outer: L) extends BaseCoreplexModule(_outer)
    with HasGlobalResetVectorWire
    with HasRocketTilesModuleImp
