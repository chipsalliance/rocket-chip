// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

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
case class TileMasterPortParams(buffers: Int = 0, cork: Option[Boolean] = None)
case class TileSlavePortParams(buffers: Int = 0, blockerCtrlAddr: Option[BigInt] = None)

case class RocketCrossingParams(
    crossingType: SubsystemClockCrossing = SynchronousCrossing(),
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
    with HasPeripheryPLIC
    with HasPeripheryCLINT
    with HasPeripheryDebug { this: BaseSubsystem =>
  val module: HasRocketTilesModuleImp

  protected val rocketTileParams = p(RocketTilesKey)
  private val NumRocketTiles = rocketTileParams.size
  private val crossingParams = p(RocketCrossingKey)
  private val crossings = crossingParams.size match {
    case 1 => List.fill(NumRocketTiles) { crossingParams.head }
    case NumRocketTiles => crossingParams
    case _ => throw new Exception("RocketCrossingKey.size must == 1 or == RocketTilesKey.size")
  }
  private val crossingTuples = rocketTileParams.zip(crossings)

  // Make a tile and wire its nodes into the system,
  // according to the specified type of clock crossing.
  // Note that we also inject new nodes into the tile itself,
  // also based on the crossing type.
  val rocketTiles = crossingTuples.map { case (tp, crossing) =>
    // For legacy reasons, it is convenient to store some state
    // in the global Parameters about the specific tile being built now
    val rocket = LazyModule(new RocketTile(tp, crossing.crossingType)(p.alterPartial {
        case TileKey => tp
        case BuildRoCC => tp.rocc
        case SharedMemoryTLEdge => sharedMemoryTLEdge
      })
    ).suggestName(tp.name)

    // Connect the master ports of the tile to the system bus

    def tileMasterBuffering: TLOutwardNode = rocket {
      // The buffers needed to cut feed-through paths are microarchitecture specific, so belong here
      val masterBufferNode = TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
      crossing.crossingType match {
        case _: AsynchronousCrossing => rocket.masterNode
        case SynchronousCrossing(b) =>
          require (!tp.boundaryBuffers || (b.depth >= 1 && !b.flow && !b.pipe), "Buffer misconfiguration creates feed-through paths")
          rocket.masterNode
        case RationalCrossing(dir) =>
          require (dir != SlowToFast, "Misconfiguration? Core slower than fabric")
          if (tp.boundaryBuffers) {
            masterBufferNode :=* rocket.masterNode
          } else {
            rocket.masterNode
          }
      }
    }

    sbus.fromTile(tp.name, crossing.master.buffers) {
        crossing.master.cork
          .map { u => TLCacheCork(unsafe = u) }
          .map { _ :=* rocket.crossTLOut }
          .getOrElse { rocket.crossTLOut }
    } :=* tileMasterBuffering

    // Connect the slave ports of the tile to the periphery bus

    def tileSlaveBuffering: TLInwardNode = rocket {
      val slaveBufferNode = TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
      crossing.crossingType match {
        case RationalCrossing(_) if (tp.boundaryBuffers) => rocket.slaveNode :*= slaveBufferNode
        case _ => rocket.slaveNode
      }
    }

    DisableMonitors { implicit p =>
      tileSlaveBuffering :*= pbus.toTile(tp.name) {
        crossing.slave.blockerCtrlAddr
          .map { BasicBusBlockerParams(_, pbus.beatBytes, sbus.beatBytes) }
          .map { bbbp => LazyModule(new BasicBusBlocker(bbbp)) }
          .map { bbb =>
            pbus.toVariableWidthSlave(Some("bus_blocker")) { bbb.controlNode }
            rocket.crossTLIn :*= bbb.node
          } .getOrElse { rocket.crossTLIn }
      }
    }

    // Handle all the different types of interrupts crossing to or from the tile:
    // 1. Debug interrupt is definitely asynchronous in all cases.
    // 2. The CLINT and PLIC output interrupts are synchronous to the periphery clock,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // 3. Local Interrupts are required to already be synchronous to the tile clock.
    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from rocket.intNode inside the tile.

    // 1. always async crossing for debug
    rocket.intInwardNode := rocket { IntSyncCrossingSink(3) } := debug.intnode

    // 2. clint+plic conditionally crossing
    val periphIntNode = rocket.intInwardNode :=* rocket.crossIntIn
    periphIntNode := clint.intnode                   // msip+mtip
    periphIntNode := plic.intnode                    // meip
    if (tp.core.useVM) periphIntNode := plic.intnode // seip

    // 3. local interrupts  never cross 
    // rocket.intInwardNode is wired up externally     // lip

    // 4. conditional crossing from core to PLIC
    FlipRendering { implicit p =>
      plic.intnode :=* rocket.crossIntOut :=* rocket.intOutwardNode
    }

    rocket
  }
}

trait HasRocketTilesModuleImp extends HasTilesModuleImp
    with HasPeripheryDebugModuleImp {
  val outer: HasRocketTiles
}

class RocketSubsystem(implicit p: Parameters) extends BaseSubsystem
    with HasRocketTiles {
  val tiles = rocketTiles
  override lazy val module = new RocketSubsystemModuleImp(this)
}

class RocketSubsystemModuleImp[+L <: RocketSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasRocketTilesModuleImp {
  tile_inputs.zip(outer.hartIdList).foreach { case(wire, i) =>
    wire.clock := clock
    wire.reset := reset
    wire.hartid := UInt(i)
    wire.reset_vector := global_reset_vector
  }
}
