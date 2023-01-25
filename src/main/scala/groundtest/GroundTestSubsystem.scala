// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import chisel3._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{NMI}
import freechips.rocketchip.devices.tilelink.{CLINTConsts}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink.{TLRAM, TLFragmenter}

class GroundTestSubsystem(implicit p: Parameters)
  extends BaseSubsystem
  with InstantiatesElements
  with HasElements
  with HasTileNotificationSinks
  with HasTileInputConstants
  with CanHaveMasterAXI4MemPort
{
  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), beatBytes=pbus.beatBytes))
  pbus.coupleTo("TestRAM") { testram.node := TLFragmenter(pbus) := _ }

  // No cores to monitor
  def coreMonitorBundles = Nil

  // No PLIC in ground test; so just sink the interrupts to nowhere
  IntSinkNode(IntSinkPortSimple()) :=* ibus.toPLIC

  val tileStatusNodes = totalTiles.collect { case t: GroundTestTile => t.statusNode.makeSink() }

  lazy val msipNodes = Map[Int, IntOutwardNode]().withDefaultValue(NullIntSource(sources = CLINTConsts.ints))
  lazy val meipNodes = Map[Int, IntOutwardNode]().withDefaultValue(NullIntSource())
  lazy val seipNodes = Map[Int, IntOutwardNode]().withDefaultValue(NullIntSource())
  lazy val plicNodes = Map[Int, IntInwardNode]()
  lazy val debugNodes = Map[Int, IntSyncOutwardNode]().withDefaultValue(IntSyncCrossingSource() := NullIntSource())
  lazy val nmiNodes = Map[Int, BundleBridgeOutwardNode[NMI]]().withDefaultValue(BundleBridgeSource[NMI])

  override lazy val module = new GroundTestSubsystemModuleImp(this)
}

class GroundTestSubsystemModuleImp[+L <: GroundTestSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer) {
  val success = IO(Output(Bool()))
  val status = dontTouch(DebugCombiner(outer.tileStatusNodes.map(_.bundle)))
  success := outer.tileCeaseSinkNode.in.head._1.asUInt.andR
}
