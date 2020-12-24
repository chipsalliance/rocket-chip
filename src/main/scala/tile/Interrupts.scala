// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

/** @todo */
class NMI(val w: Int) extends Bundle {
  val rnmi = Bool()
  val rnmi_interrupt_vector = UInt(w.W)
  val rnmi_exception_vector = UInt(w.W)
  val unmi = Bool()
  val unmi_interrupt_vector = UInt(w.W)
  val unmi_exception_vector = UInt(w.W)
}

class TileInterrupts(implicit p: Parameters) extends CoreBundle()(p) {
  /** debug interrupt.
    * see riscv debug spec.
    */
  val debug = Bool()
  /** Machine Timer Interrupt Pending
    * see riscv privileged spec.
    */
  val mtip = Bool()
  /** Machine Software Interrupt Pending
    * see riscv privileged spec.
    */
  val msip = Bool()
  /** Machine External Interrupt Pending
    * see riscv privileged spec.
    */
  val meip = Bool()
  /** Supervisor External Interrupt Pending
    * see riscv privileged spec.
    */
  val seip = usingSupervisor.option(Bool())
  /** Local Interrupt Pending
    * @todo where to where?
    */
  val lip = Vec(coreParams.nLocalInterrupts, Bool())
  /** Non Maskable Interrupt
    * see riscv privileged spec 3.4.
    */
  val nmi = usingNMI.option(new NMI(resetVectorLen))
}

/** Use diplomatic interrupts to external interrupts from the subsystem into the tile. */
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

  /** CSR address map:
    * debug: 0xffff
    * msip: 0x0003
    * mtip: 0x0007
    * meip: 0x000b
    * seip: 0x0009
    * lips: 0x0010 + offset
    *
    */
  def csrIntMap: List[Int] = {
    val nlips = tileParams.core.nLocalInterrupts
    val seip = if (usingSupervisor) Seq(9) else Nil
    List(65535, 3, 7, 11) ++ seip ++ List.tabulate(nlips)(_ + 16)
  }

  /** go from flat diplomatic Interrupts [[intSinkNode]] to core bundled [[TileInterrupts]]
    * This should be called at [[RocketTileModuleImp]].
    */
  def decodeCoreInterrupts(core: TileInterrupts): Unit = {
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
  /** Node report unrecoverable error conditions*/
  val haltNode = IntSourceNode(IntSourcePortSimple())

  /** report errors from DCache uncorrectable error to [[haltNode]] */
  def reportHalt(could_halt: Option[Bool]): Unit = {
    val (halt_and_catch_fire, _) = haltNode.out(0)
    halt_and_catch_fire(0) := could_halt.map(RegEnable(true.B, false.B, _)).getOrElse(false.B)
  }

  /** @todo set to private. */
  def reportHalt(errors: Seq[CanHaveErrors]): Unit = {
    reportHalt(errors.flatMap(_.uncorrectable).map(_.valid).reduceOption(_||_))
  }

  /** Node report when the tile has ceased to retire instructions. */
  val ceaseNode = IntSourceNode(IntSourcePortSimple())

  /** report cease to [[ceaseNode]],
    * currently connect gate signal from DCache, Frontend, PTW and Core.
    */
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

  /** Node report when the tile is waiting for an interrupt. */
  val wfiNode = IntSourceNode(IntSourcePortSimple())

  /** report Wait For Interrupt to [[wfiNode]],
    * report should come from core, and blocked during reset.
    */
  def reportWFI(could_wfi: Option[Bool]): Unit = {
    val (wfi, _) = wfiNode.out(0)
    wfi(0) := could_wfi.map(RegNext(_, init=false.B)).getOrElse(false.B)
  }
}
