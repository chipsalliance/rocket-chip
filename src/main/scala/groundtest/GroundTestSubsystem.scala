// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import chisel3._
import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.subsystem.{BaseSubsystem, BaseSubsystemModuleImp, HasTiles, CanHaveMasterAXI4MemPort}
import freechips.rocketchip.tilelink.{TLRAM, TLFragmenter}
import freechips.rocketchip.interrupts.{NullIntSyncSource}

class GroundTestSubsystem(implicit p: Parameters)
  extends BaseSubsystem
  with HasTiles
  with CanHaveMasterAXI4MemPort
{
  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), beatBytes=pbus.beatBytes))
  pbus.coupleTo("TestRAM") { testram.node := TLFragmenter(pbus) := _ }

  // No cores to monitor
  def coreMonitorBundles = Nil

  // No PLIC in ground test; so just sink the interrupts to nowhere
  IntSinkNode(IntSinkPortSimple()) :=* ibus.toPLIC

  val tileStatusNodes = tiles.collect { case t: GroundTestTile => t.statusNode.makeSink() }

  // no debug module
  val debugNode = NullIntSyncSource()

  override lazy val module = new GroundTestSubsystemModuleImp(this)
}

class GroundTestSubsystemModuleImp[+L <: GroundTestSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer) {
  val success = IO(Output(Bool()))
  val status = dontTouch(DebugCombiner(outer.tileStatusNodes.map(_.bundle)))
  success := outer.tileCeaseSinkNode.in.head._1.asUInt.andR
}
