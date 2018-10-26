// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._

import scala.math.max

case object TileId extends Field[Int]

class GroundTestSubsystem(implicit p: Parameters) extends BaseSubsystem
    with CanHaveMasterAXI4MemPort {
  val tileParams = p(GroundTestTilesKey)
  val tiles = tileParams.zipWithIndex.map { case(c, i) => LazyModule(
    c.build(i, p.alterPartial {
      case TileKey => c
      case SharedMemoryTLEdge => sbus.busView
    })
  )}

  tiles.flatMap(_.dcacheOpt).foreach { dc =>
    sbus.fromTile(None, buffer = BufferParams.default){ dc.node }
  }

  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), true, true, pbus.beatBytes))
  pbus.coupleTo("TestRAM") { testram.node := TLFragmenter(pbus) := _ }

  // No PLIC in ground test; so just sink the interrupts to nowhere
  IntSinkNode(IntSinkPortSimple()) := ibus.toPLIC

  override lazy val module = new GroundTestSubsystemModuleImp(this)
}

class GroundTestSubsystemModuleImp[+L <: GroundTestSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with CanHaveMasterAXI4MemPortModuleImp {
  val success = IO(Bool(OUTPUT))

  outer.tiles.zipWithIndex.map { case(t, i) => t.module.constants.hartid := UInt(i) }

  val status = DebugCombiner(outer.tiles.map(_.module.status))
  success := status.finished
}
