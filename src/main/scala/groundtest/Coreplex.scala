// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._

import scala.math.max

case object TileId extends Field[Int]

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex
    with HasMasterAXI4MemPort
    with HasPeripheryTestRAMSlave
    with HasInterruptBus {
  val tileParams = p(GroundTestTilesKey)
  val tiles = tileParams.zipWithIndex.map { case(c, i) => LazyModule(
    c.build(i, p.alterPartial {
      case TileKey => c
      case SharedMemoryTLEdge => sbus.busView
    })
  )}

  tiles.flatMap(_.dcacheOpt).foreach { dc =>
    sbus.fromTile(None) { implicit p => TileMasterPortParams(addBuffers = 1).adapt(this)(dc.node) }
  }

  // No PLIC in ground test; so just sink the interrupts to nowhere
  IntSinkNode(IntSinkPortSimple()) := ibus.toPLIC

  val pbusRAM = LazyModule(new TLRAM(AddressSet(testRamAddr, 0xffff), true, false, pbus.beatBytes))
  pbusRAM.node := pbus.toVariableWidthSlaves

  override lazy val module = new GroundTestCoreplexModule(this)
}

class GroundTestCoreplexModule[+L <: GroundTestCoreplex](_outer: L) extends BaseCoreplexModule(_outer)
    with HasMasterAXI4MemPortModuleImp {
  val success = IO(Bool(OUTPUT))

  outer.tiles.zipWithIndex.map { case(t, i) => t.module.constants.hartid := UInt(i) }

  val status = DebugCombiner(outer.tiles.map(_.module.status))
  success := status.finished
}

/** Adds a SRAM to the system for testing purposes. */
trait HasPeripheryTestRAMSlave extends HasPeripheryBus {
  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), true, true, pbus.beatBytes))
  testram.node := pbus.toVariableWidthSlaves
}

/** Adds a fuzzing master to the system for testing purposes. */
trait HasPeripheryTestFuzzMaster extends HasPeripheryBus {
  val fuzzer = LazyModule(new TLFuzzer(5000))
  pbus.bufferFromMasters := fuzzer.node
}
