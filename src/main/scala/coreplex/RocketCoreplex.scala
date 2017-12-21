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
    cork: Option[Boolean] = None) {

  def adapt(coreplex: HasPeripheryBus)
           (masterNode: TLOutwardNode)
           (implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val tile_master_cork = cork.map(u => (LazyModule(new TLCacheCork(unsafe = u))))
    val tile_master_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.allUncacheable))

    (Seq(tile_master_fixer.node) ++ TLBuffer.chain(addBuffers) ++ tile_master_cork.map(_.node))
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
    (Seq() ++ tile_slave_blocker.map(_.node) ++ TLBuffer.chain(addBuffers))
    .foldLeft(slaveNode)(_ :*= _)
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

  protected val rocketTileParams = p(RocketTilesKey)
  private val NumRocketTiles = rocketTileParams.size
  private val crossingParams = p(RocketCrossingKey)
  private val crossings = crossingParams.size match {
    case 1 => List.fill(NumRocketTiles) { crossingParams.head }
    case NumRocketTiles => crossingParams
    case _ => throw new Exception("RocketCrossingKey.size must == 1 or == RocketTilesKey.size")
  }
  private val crossingTuples = rocketTileParams.zip(crossings)

  // Make a wrapper for each tile that will wire it to coreplex devices and crossbars,
  // according to the specified type of clock crossing.
  val rocketTiles = crossingTuples.map { case (tp, crossing) =>
    // For legacy reasons, it is convenient to store some state
    // in the global Parameters about the specific tile being built now
    val wrapper = LazyModule(new RocketTileWrapper(
      params = tp,
      crossing = crossing.crossingType
      )(p.alterPartial {
        case TileKey => tp
        case BuildRoCC => tp.rocc
        case SharedMemoryTLEdge => sharedMemoryTLEdge
        case RocketCrossingKey => List(crossing)
      })
    ).suggestName(tp.name)

    // Connect the master ports of the tile to the system bus
    sbus.fromTile(tp.name) { implicit p => crossing.master.adapt(this)(wrapper.crossTLOut :=* wrapper.masterNode) }

    // Connect the slave ports of the tile to the periphery bus
    pbus.toTile(tp.name) { implicit p => crossing.slave.adapt(this)(wrapper.slaveNode :*= wrapper.crossTLIn) }

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
    wrapper.intInwardNode := wrapper { IntSyncCrossingSink(3) } := debug.intnode

    // 2. clint+plic conditionally crossing
    val periphIntNode = wrapper.intInwardNode :=* wrapper.crossIntIn
    periphIntNode := clint.intnode                   // msip+mtip
    periphIntNode := plic.intnode                    // meip
    if (tp.core.useVM) periphIntNode := plic.intnode // seip

    // 3. local interrupts  never cross 
    // this.intInwardNode is wired up externally     // lip

    // 4. conditional crossing from core to PLIC
    FlipRendering { implicit p =>
      plic.intnode :=* wrapper.crossIntOut :=* wrapper.intOutwardNode
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
  val tiles = rocketTiles
  override lazy val module = new RocketCoreplexModule(this)
}

class RocketCoreplexModule[+L <: RocketCoreplex](_outer: L) extends BaseCoreplexModule(_outer)
    with HasRocketTilesModuleImp {
  tile_inputs.zip(outer.hartIdList).foreach { case(wire, i) =>
    wire.clock := clock
    wire.reset := reset
    wire.hartid := UInt(i)
    wire.reset_vector := global_reset_vector
  }
}
