// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

class TileInterrupts(implicit p: Parameters) extends CoreBundle()(p) {
  val debug = Bool()
  val mtip = Bool()
  val msip = Bool()
  val meip = Bool()
  val seip = usingVM.option(Bool())
  val lip = Vec(coreParams.nLocalInterrupts, Bool())
}

// Use diplomatic interrupts to external interrupts from the subsystem into the tile
trait SinksExternalInterrupts { this: BaseTile =>

  val intInwardNode = intXbar.intnode :=* IntIdentityNode()(ValName("int_local"))
  protected val intSinkNode = IntSinkNode(IntSinkPortSimple())
  intSinkNode := intXbar.intnode

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
    intSinkNode.edges.in.flatMap(_.source.sources).map { case s =>
      for (i <- s.range.start until s.range.end) {
       csrIntMap.lift(i).foreach { j =>
          s.resources.foreach { r =>
            r.bind(intcDevice, ResourceInt(j))
          }
        }
      }
    }
  }

  // TODO: the order of the following two functions must match, and
  //         also match the order which things are connected to the
  //         per-tile crossbar in subsystem.HasTiles.connectInterrupts

  // debug, msip, mtip, meip, seip, lip offsets in CSRs
  def csrIntMap: List[Int] = {
    val nlips = tileParams.core.nLocalInterrupts
    val seip = if (usingVM) Seq(9) else Nil
    List(65535, 3, 7, 11) ++ seip ++ List.tabulate(nlips)(_ + 16)
  }

  // go from flat diplomatic Interrupts to bundled TileInterrupts
  def decodeCoreInterrupts(core: TileInterrupts) {
    val async_ips = Seq(core.debug)
    val periph_ips = Seq(
      core.msip,
      core.mtip,
      core.meip)

    val seip = if (core.seip.isDefined) Seq(core.seip.get) else Nil

    val core_ips = core.lip

    val (interrupts, _) = intSinkNode.in(0)
    (async_ips ++ periph_ips ++ seip ++ core_ips).zip(interrupts).foreach { case(c, i) => c := i }
  }
}

trait SourcesExternalNotifications { this: BaseTile =>
  // Report unrecoverable error conditions
  val haltNode = IntSourceNode(IntSourcePortSimple())

  def reportHalt(could_halt: Option[Bool]) {
    val (halt_and_catch_fire, _) = haltNode.out(0)
    halt_and_catch_fire(0) := could_halt.map(RegEnable(true.B, false.B, _)).getOrElse(false.B)
  }

  def reportHalt(errors: Seq[CanHaveErrors]) {
    reportHalt(errors.flatMap(_.uncorrectable).map(_.valid).reduceOption(_||_))
  }

  // Report when the tile has ceased to retire instructions
  val ceaseNode = IntSourceNode(IntSourcePortSimple())

  def reportCease(could_cease: Option[Bool]) {
    val (cease, _) = ceaseNode.out(0)
    cease(0) := could_cease.map(RegNext(_)).getOrElse(false.B)
  }

  // Report when the tile is waiting for an interrupt
  val wfiNode = IntSourceNode(IntSourcePortSimple())

  def reportWFI(could_wfi: Option[Bool]) {
    val (wfi, _) = wfiNode.out(0)
    wfi(0) := could_wfi.map(RegNext(_)).getOrElse(false.B)
  }
}
