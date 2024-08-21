// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import chisel3._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.AddressSet

import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{NMI}
import freechips.rocketchip.devices.tilelink.{CLINTConsts}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink.{TLRAM, TLFragmenter}
import freechips.rocketchip.interrupts.{NullIntSyncSource}

class GroundTestSubsystem(implicit p: Parameters)
  extends BaseSubsystem
  with InstantiatesHierarchicalElements
  with HasHierarchicalElementsRootContext
  with HasHierarchicalElements
  with HasTileNotificationSinks
  with HasTileInputConstants
  with CanHaveMasterAXI4MemPort
{
  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), beatBytes=tlBusWrapperLocationMap.get(PBUS).getOrElse(tlBusWrapperLocationMap(p(TLManagerViewpointLocated(location)))).beatBytes))
  tlBusWrapperLocationMap.lift(PBUS).getOrElse(tlBusWrapperLocationMap(p(TLManagerViewpointLocated(location)))).coupleTo("TestRAM") { testram.node := TLFragmenter(tlBusWrapperLocationMap.lift(PBUS).getOrElse(tlBusWrapperLocationMap(p(TLManagerViewpointLocated(location))))) := _ }

  // No cores to monitor
  def coreMonitorBundles = Nil

  // No PLIC in ground test; so just sink the interrupts to nowhere
  IntSinkNode(IntSinkPortSimple()) :=* ibus.toPLIC

  val tileStatusNodes = totalTiles.values.collect { case t: GroundTestTile => t.statusNode.makeSink() }
  val clintOpt = None
  val clintDomainOpt = None
  val debugOpt = None
  val plicOpt = None
  val plicDomainOpt = None

  override lazy val module = new GroundTestSubsystemModuleImp(this)
}

class GroundTestSubsystemModuleImp[+L <: GroundTestSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer) {
  val success = IO(Output(Bool()))
  val status = dontTouch(DebugCombiner(_outer.tileStatusNodes.map(_.bundle).toSeq))
  success := _outer.tileCeaseSinkNode.in.head._1.asUInt.andR
}
