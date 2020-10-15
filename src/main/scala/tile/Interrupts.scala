// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util.{log2Ceil, RegEnable}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

object InterruptOffsetConstants {
  def SSIPID        : Int = 1       // ssip (if S-Mode present)
  def MSIPID        : Int = 3       // msip
  def STIPID        : Int = 5       // stip (if S-Mode present)
  def MTIPID        : Int = 7       // mtip
  def SEIPID        : Int = 9       // seip from PLIC (if S-Mode present)
  def MEIPID        : Int = 11      // meip from PLIC
  def CSIPID        : Int = 12      // additional CLIC software interrupt
  def LOCALINTIDBASE: Int = 16      // 1st local interrupt ID
  def DEBUGID       : Int = 65535   // mysterious ancient constant
}
import InterruptOffsetConstants._

class TileInterrupts(implicit p: Parameters) extends CoreBundle()(p) {
  val debug = Bool()
  val mtip = Bool()
  val msip = Bool()
  val meip = Bool()
  val seip = usingSupervisor.option(Bool())
  val lip = Vec(coreParams.nLocalInterrupts, Bool())
}

/** This trait offers the nodes necessary to decode external interrupts raised
  * by debug, clint, plic, interrupt controllers and translate them into
  * the bundle format expected by the CSR file.
  */
trait SinksExternalInterrupts { this: BaseTile =>
  private val debugCSRIntIds = List(DEBUGID)
  private val clintCSRIntIds = List(MSIPID, MTIPID)
  private val plicCSRIntIds = if (usingSupervisor) List(MEIPID, SEIPID) else List(MEIPID)
  private val localCSRIntIds = List.tabulate(tileParams.core.nLocalInterrupts)(_ + LOCALINTIDBASE)
  // The order of ids in this list must match the below ordering of IntSinkPorts,
  // the indexing order used below in decodeCoreInterrupts, and
  // the order of := connections in subsystem.HasTiles.connectInterrupts
  private val csrIntIds = debugCSRIntIds ++ clintCSRIntIds ++ plicCSRIntIds ++ localCSRIntIds
  val intSinkNode = IntSinkNode(Seq(
    IntSinkPortParameters(Seq(IntSinkParameters(debugCSRIntIds.size))),
    IntSinkPortParameters(Seq(IntSinkParameters(clintCSRIntIds.size))),
    IntSinkPortParameters(Seq(IntSinkParameters(plicCSRIntIds.size))),
    IntSinkPortParameters(Seq(IntSinkParameters(localCSRIntIds.size)))
  ))
  val intInwardNode = intSinkNode

  // go from diplomatic Interrupts to bundled TileInterrupts
  def decodeCoreInterrupts(core: TileInterrupts): Unit = {
    core.debug := intSinkNode.in(0)._1(0)
    core.msip  := intSinkNode.in(1)._1(0)
    core.mtip  := intSinkNode.in(1)._1(1)
    core.meip  := intSinkNode.in(2)._1(0)
    core.seip.foreach { _ := intSinkNode.in(2)._1(1) }
    core.lip   := intSinkNode.in(3)._1
  }

  def cpuDevice: Device
  val intcDevice = new DeviceSnippet {
    override def parent = Some(cpuDevice)
    def describe(): Description = {
      Description("interrupt-controller", Map(
        "compatible"           -> "riscv,cpu-intc".asProperty,
        "interrupt-controller" -> Nil,
        "#interrupt-cells"     -> 1.asProperty))
    }
  }

  ResourceBinding {
    intSinkNode.edges.in.foreach { _.bindDevice(intcDevice, csrIntIds.lift) }
  }
}

trait SourcesExternalNotifications { this: BaseTile =>
  // Report unrecoverable error conditions
  val haltNode = IntSourceNode(IntSourcePortSimple())

  def reportHalt(could_halt: Option[Bool]): Unit = {
    val (halt_and_catch_fire, _) = haltNode.out(0)
    halt_and_catch_fire(0) := could_halt.map(h => RegEnable(true.B, false.B, BlockDuringReset(h))).getOrElse(false.B)
  }

  def reportHalt(errors: Seq[CanHaveErrors]): Unit = {
    reportHalt(errors.flatMap(_.uncorrectable).map(_.valid).reduceOption(_||_))
  }

  // Report when the tile has ceased to retire instructions
  val ceaseNode = IntSourceNode(IntSourcePortSimple())

  def reportCease(could_cease: Option[Bool], quiescenceCycles: Int = 8): Unit = {
    def waitForQuiescence(cease: Bool): Bool = {
      // don't report cease until signal is stable for longer than any pipeline depth
      val count = RegInit(0.U(log2Ceil(quiescenceCycles + 1).W))
      val saturated = count >= quiescenceCycles.U
      when (!cease) { count := 0.U }
      when (cease && !saturated) { count := count + 1.U }
      saturated
    }
    val (cease, _) = ceaseNode.out(0)
    cease(0) := could_cease.map{ c => 
      val cease = (waitForQuiescence(c))
      // Test-Only Code --
      val prev_cease = RegNext(cease, false.B)
      assert(!(prev_cease & !cease), "CEASE line can not glitch once raised") 
      cease
    }.getOrElse(false.B)
  }

  // Report when the tile is waiting for an interrupt
  val wfiNode = IntSourceNode(IntSourcePortSimple())

  def reportWFI(could_wfi: Option[Bool]): Unit = {
    val (wfi, _) = wfiNode.out(0)
    wfi(0) := could_wfi.map(w => RegNext(BlockDuringReset(w), init=false.B)).getOrElse(false.B)
  }
}
