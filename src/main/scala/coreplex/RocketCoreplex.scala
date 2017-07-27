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
  // TODO: figure out how to merge the localIntNodes and coreIntXbar below
  val localIntCounts = tileParams.map(_.core.nLocalInterrupts)
  val localIntNodes = tileParams map { t =>
    (t.core.nLocalInterrupts > 0).option(LazyModule(new IntXbar).intnode)
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

    val wrapper = crossing match {
      case SynchronousCrossing(params) => {
        val wrapper = LazyModule(new SyncRocketTile(c, i)(pWithExtra))
        sbus.fromSyncTiles(params) :=* wrapper.masterNode 
        wrapper.slaveNode :*= pbus.bufferToSlaves
        wrapper
      }
      case AsynchronousCrossing(depth, sync) => {
        val wrapper = LazyModule(new AsyncRocketTile(c, i)(pWithExtra))
        sbus.fromAsyncTiles(depth, sync) :=* wrapper.masterNode
        wrapper.slaveNode :*= pbus.toAsyncSlaves(sync)
        wrapper
      }
      case RationalCrossing(direction) => {
        val wrapper = LazyModule(new RationalRocketTile(c, i)(pWithExtra))
        sbus.fromRationalTiles(direction) :=* wrapper.masterNode
        wrapper.slaveNode :*= pbus.toRationalSlaves
        wrapper
      }
    }
    wrapper.suggestName("tile") // Try to stabilize this name for downstream tools

    // Local Interrupts must be synchronized to the core clock
    // before being passed into this module.
    // This allows faster latency for interrupts which are already synchronized.
    // The CLINT and PLIC outputs interrupts that are synchronous to the periphery clock,
    // so may or may not need to be synchronized depending on the Tile's crossing type.
    // Debug interrupt is definitely asynchronous in all cases.
    val asyncIntXbar  = LazyModule(new IntXbar)
    asyncIntXbar.intnode  := debug.intnode                  // debug
    wrapper.asyncIntNode  := asyncIntXbar.intnode

    val periphIntXbar = LazyModule(new IntXbar)
    periphIntXbar.intnode := clint.intnode                  // msip+mtip
    periphIntXbar.intnode := plic.intnode                   // meip
    if (c.core.useVM) periphIntXbar.intnode := plic.intnode // seip
    wrapper.periphIntNode := periphIntXbar.intnode

    val coreIntXbar = LazyModule(new IntXbar)
    lip.foreach { coreIntXbar.intnode := _ }                // lip
    wrapper.coreIntNode   := coreIntXbar.intnode

    wrapper
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
    with HasResetVectorWire
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
    with HasRocketTiles {
  override lazy val module = new RocketCoreplexModule(this)
}

class RocketCoreplexModule[+L <: RocketCoreplex](_outer: L) extends BaseCoreplexModule(_outer)
    with HasRocketTilesModuleImp
