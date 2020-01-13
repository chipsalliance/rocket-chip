// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.model.OMInterrupt
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._

import scala.math.max

case object TileId extends Field[Int]

class GroundTestSubsystem(implicit p: Parameters) extends BaseSubsystem
    with HasHierarchicalBusTopology
    with CanHaveMasterAXI4MemPort {
  val tileParams = p(GroundTestTilesKey)
  val tiles = tileParams.zipWithIndex.map { case(c, i) => LazyModule(c.build(i, p)) }

  tiles.map(_.masterNode).foreach { m =>
    sbus.fromTile(None, buffer = BufferParams.default){ m }
  }

  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), beatBytes=pbus.beatBytes))
  pbus.coupleTo("TestRAM") { testram.node := TLFragmenter(pbus) := _ }

  // No PLIC in ground test; so just sink the interrupts to nowhere
  IntSinkNode(IntSinkPortSimple()) :=* ibus.toPLIC

  val dummyClockGroupSourceNode = SimpleClockGroupSource()
  clockGroupNode :*= dummyClockGroupSourceNode

  override lazy val module = new GroundTestSubsystemModuleImp(this)
}

class GroundTestSubsystemModuleImp[+L <: GroundTestSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer) {
  val success = IO(Bool(OUTPUT))

  outer.tiles.zipWithIndex.map { case(t, i) => t.module.constants.hartid := UInt(i) }

  val status = DebugCombiner(outer.tiles.map(_.module.status))
  success := status.finished
}
