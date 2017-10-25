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

    tile_master_blocker.foreach { _.controlNode := coreplex.pbus.toVariableWidthSlaves }
    (Seq(tile_master_fixer.node) ++ TLBuffer.chain(addBuffers)
     ++ tile_master_blocker.map(_.node) ++ tile_master_cork.map(_.node))
     .foldRight(masterNode)(_ :=* _)
  }
}

case class TileSlavePortParams(
    addBuffers: Int = 0,
    blockerCtrlAddr: Option[BigInt] = None) {

  def adapt(coreplex: HasPeripheryBus)
           (slaveNode: TLInwardNode)
           (implicit p: Parameters, sourceInfo: SourceInfo): TLInwardNode = {
    val tile_slave_blocker =
      blockerCtrlAddr
        .map(BasicBusBlockerParams(_, coreplex.pbus.beatBytes, coreplex.sbus.beatBytes))
        .map(bp => LazyModule(new BasicBusBlocker(bp)))

    tile_slave_blocker.foreach { _.controlNode := coreplex.pbus.toVariableWidthSlaves }
    (TLBuffer.chain(addBuffers) ++ tile_slave_blocker.map(_.node))
    .foldLeft(slaveNode)(_ :*= _)
  }
}

case class RocketCrossingParams(
    crossingType: CoreplexClockCrossing = SynchronousCrossing(),
    master: TileMasterPortParams = TileMasterPortParams(),
    slave: TileSlavePortParams = TileSlavePortParams(),
    boundaryBuffers: Boolean = false) {
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
  private val crossingTuples = localIntNodes.zip(tileParams).zip(crossings)

  // Make a wrapper for each tile that will wire it to coreplex devices and crossbars,
  // according to the specified type of clock crossing.
  val tiles: Seq[BaseTile] = crossingTuples.map { case ((lip, tp), crossing) =>
    // For legacy reasons, it is convenient to store some state
    // in the global Parameters about the specific tile being built now
    val wrapper = LazyModule(new RocketTileWrapper(
      params = tp,
      crossing = crossing.crossingType,
      boundaryBuffers = crossing.boundaryBuffers
      )(p.alterPartial {
        case TileKey => tp
        case BuildRoCC => tp.rocc
        case SharedMemoryTLEdge => sharedMemoryTLEdge
        case RocketCrossingKey => List(crossing)
      })
    ).suggestName(tp.name)

    // Connect the master ports of the tile to the system bus
    sbus.fromTile(tp.name) { implicit p => crossing.master.adapt(this)(wrapper.cross(wrapper.masterNode)) }

    // Connect the slave ports of the tile to the periphery bus
    pbus.toTile(tp.name) { implicit p => crossing.slave.adapt(this)(wrapper.slaveNode) } // !!! wrapper.cross

    // Handle all the different types of interrupts crossing to or from the tile:
    // 1. Debug interrupt is definitely asynchronous in all cases.
    // 2. The CLINT and PLIC output interrupts are synchronous to the periphery clock,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // 3. Local Interrupts are required to already be synchronous to the tile clock.
    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from rocket.intNode inside the tile.

    val asyncIntXbar  = LazyModule(new IntXbar).suggestName(tp.name.map(_ + "AsyncIntXbar"))
    asyncIntXbar.intnode  := debug.intnode                   // debug
    wrapper.intXbar.intnode := wrapper.cross(                // 1. always crosses
      name = tp.name.map(_ + "AsyncIntXbar"),
      overrideCrossing = Some(AsynchronousCrossing(8,3))
    )(x = asyncIntXbar.intnode)

    val periphIntXbar = LazyModule(new IntXbar).suggestName(tp.name.map(_ + "PeriphIntXbar"))
    periphIntXbar.intnode := clint.intnode                   // msip+mtip
    periphIntXbar.intnode := plic.intnode                    // meip
    if (tp.core.useVM) periphIntXbar.intnode := plic.intnode // seip
    wrapper.intXbar.intnode := wrapper.cross(                // 2. conditionally crosses
      name = tp.name.map(_ + "PeriphIntXbar")
    )(x = periphIntXbar.intnode)

    val coreIntXbar = LazyModule(new IntXbar).suggestName(tp.name.map(_ + "CoreIntXbar"))
    lip.foreach { coreIntXbar.intnode := _ }                 // lip
    wrapper.intXbar.intnode := coreIntXbar.intnode           // 3. never crosses

    wrapper.rocket.intOutputNode.foreach { i =>              // 4. conditionally crosses
      plic.intnode := FlipRendering { implicit p =>
        wrapper.cross(name = tp.name.map(_ + "PeriphIntOutput"))(x = i)
      }
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
