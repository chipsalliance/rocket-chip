// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import rocket.Util._
import util._
import testchipip._
import coreplex._
import uncore.tilelink2._
import uncore.tilelink._
import uncore.agents._
import junctions._
import hbwif._

case object BuildHTop extends Field[Parameters => HUpTop]

/* Hurricane Chisel Top */
class HUpTop(q: Parameters) extends BaseTop(q)
    with PeripheryBootROM
    with PeripheryDebug
    with PeripheryCoreplexLocalInterrupter
    with HurricaneIF
    with HurricaneExtraTopLevel
    with Hbwif
    with PeripheryMasterMMIO
    with PeripherySlave { //TODOHurricane: Do we need this?/What is it for?
  override lazy val module = Module(new HUpTopModule(p, this, new HUpTopBundle(p)))
}

class HUpTopBundle(p: Parameters) extends BaseTopBundle(p)
    with PeripheryBootROMBundle
    with PeripheryDebugBundle
    with PeripheryCoreplexLocalInterrupterBundle
    with HurricaneIFBundle
    with HbwifBundle
    with PeripheryMasterMMIOBundle
    with PeripherySlaveBundle
//TODOHurricane: add DRAM I/Os here

class HUpTopModule[+L <: HUpTop, +B <: HUpTopBundle]
    (p: Parameters, l: L, b: => B) extends BaseTopModule(p, l, b)
    with PeripheryBootROMModule
    with PeripheryDebugModule
    with PeripheryCoreplexLocalInterrupterModule
    with HurricaneIFModule
    with HurricaneExtraTopLevelModule
    with HbwifModule
    with PeripheryMasterMMIOModule
    with PeripherySlaveModule
    with HardwiredResetVector {
  val multiClockCoreplexIO = coreplexIO.asInstanceOf[MultiClockCoreplexBundle]

  coreplex.clock := clock
  coreplex.reset := ResetSync(topLevelSCRBuilder.control("coreplex_reset", UInt(1))(0).toBool, coreplex.clock)
  multiClockCoreplexIO.tcrs.dropRight(1).zipWithIndex foreach { case (tcr, i) =>
    tcr.clock := clock
    tcr.reset := ResetSync(topLevelSCRBuilder.control(s"core_${i}_reset", UInt(1))(0).toBool, tcr.clock)
  }
  multiClockCoreplexIO.tcrs.last.reset := topLevelSCRBuilder.control(s"pmu_reset", UInt(1))(0).toBool
  multiClockCoreplexIO.extcr.clock := clock
  multiClockCoreplexIO.extcr.reset := reset

  // Hbwif connections
  hbwifFastClock := clock

  //SCR file generation
  val scrTL = topLevelSCRBuilder.generate(outermostMMIOParams)
  scrTL <> pBus.port("HSCRFile")
}

/////

trait HurricaneExtraTopLevel extends LazyModule {
  implicit val p: Parameters
  val pDevices: ResourceManager[AddrMapEntry]

  pDevices.add(AddrMapEntry(s"HSCRFile", MemSize(BigInt(p(HSCRFileSize)), MemAttr(AddrMapProt.RW))))
}

trait HurricaneExtraTopLevelModule {
  implicit val p: Parameters
  val hbwifFastClock: Clock = Wire(Clock())
  val topLevelSCRBuilder: SCRBuilder = new SCRBuilder
}

/////

trait HurricaneIF extends LazyModule {
  implicit val p: Parameters
}

trait HurricaneIFBundle extends HasPeripheryParameters {
  implicit val p: Parameters
  val narrowIO = new SerialIO(p(NarrowWidth)) //TODOHurricane - this should be NarrowIO, not SerialIO
}

trait HurricaneIFModule extends HasPeripheryParameters {
  implicit val p: Parameters
  val numLanes = p(HbwifKey).numLanes

  val outer: HurricaneIF
  val io: HurricaneIFBundle
  val coreplexIO: BaseCoreplexBundle
  val hbwifIO: Vec[ClientUncachedTileLinkIO] = Wire(Vec(numLanes,
    new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => "Outermost"}))))
  // TODOHurricane - implement the TL master/slave combined version
  require(p(NAcquireTransactors) > 2 || numLanes < 8)
  // TODOHurricane - why doesn't the switcher handle this gracefully
  val nBanks = nMemChannels*p(NBanksPerMemoryChannel)
  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(nBanks, numLanes+1)
      (p.alterPartial({case TLId => "Outermost"})))
  switcher.io.in <> coreplexIO.master.mem
  val lbwif = Module(new ClientUncachedTileLinkIOSerdes(p(NarrowWidth))(p.alterPartial({case TLId => "Outermost"})))

  lbwif.io.tl <> switcher.io.out(0)
  io.narrowIO <> lbwif.io.serial

  val ser = (0 until numLanes) map { i =>
    hbwifIO(i) <> switcher.io.out(i+1)
  }
  // io.mem_narrow.get <> ser(0).io.serial // TODOHurricane - Howie says to wire in and out separately for SerialIO
  // TODOHurricane - wire up the HBWIF lanes
  // switcher.io.select(0) := ... // TODOHurricane - Need to hardcode all banks to route to channel 0, but it's unclear how to do this.
                                  // Eventually this should be configurable via SCR
}
