// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import chisel3._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.subsystem.{BaseSubsystem, BaseSubsystemModuleImp, CanHaveMasterAXI4MemPort, HasTiles}
import freechips.rocketchip.tilelink.{TLFragmenter, TLRAM}

class GroundTestSubsystem(implicit p: Parameters)
  extends BaseSubsystem
  with HasTiles
  with CanHaveMasterAXI4MemPort
  with HasPeripheryDebug
{
  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), beatBytes=pbus.beatBytes))
  pbus.coupleTo("TestRAM") { testram.node := TLFragmenter(pbus) := _ }

  // No cores to monitor
  def coreMonitorBundles = Nil

  // No PLIC in ground test; so just sink the interrupts to nowhere
  IntSinkNode(IntSinkPortSimple()) :=* ibus.toPLIC

  val tileStatusNodes = tiles.collect { case t: GroundTestTile => t.statusNode.makeSink() }

  override lazy val module = new GroundTestSubsystemModuleImp(this)
}

class GroundTestSubsystemModuleImp[+L <: GroundTestSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
  with HasPeripheryDebugModuleImp {
  val success = IO(Output(Bool()))
  val status = dontTouch(DebugCombiner(outer.tileStatusNodes.map(_.bundle)))
  success := outer.tileCeaseSinkNode.in.head._1.asUInt.andR
}
