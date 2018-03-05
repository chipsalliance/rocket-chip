// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo
import scala.math.min

class GatewayPLICIO extends Bundle {
  val valid = Bool(OUTPUT)
  val ready = Bool(INPUT)
  val complete = Bool(INPUT)
}

class LevelGateway extends Module {
  val io = new Bundle {
    val interrupt = Bool(INPUT)
    val plic = new GatewayPLICIO
  }

  val inFlight = Reg(init=Bool(false))
  when (io.interrupt && io.plic.ready) { inFlight := true }
  when (io.plic.complete) { inFlight := false }
  io.plic.valid := io.interrupt && !inFlight
}

object PLICConsts
{
  def maxDevices = 1023
  def maxHarts = 15872
  def priorityBase = 0x0
  def pendingBase = 0x1000
  def enableBase = 0x2000
  def hartBase = 0x200000

  def claimOffset = 4
  def priorityBytes = 4

  def enableOffset(i: Int) = i * ((maxDevices+7)/8)
  def hartOffset(i: Int) = i * 0x1000
  def enableBase(i: Int):Int = enableOffset(i) + enableBase
  def hartBase(i: Int):Int = hartOffset(i) + hartBase

  def size = hartBase(maxHarts)
  require(hartBase >= enableBase(maxHarts))
}

case class PLICParams(baseAddress: BigInt = 0xC000000, maxPriorities: Int = 7, intStages: Int = 0)
{
  require (maxPriorities >= 0)
  def address = AddressSet(baseAddress, PLICConsts.size-1)
}

case object PLICKey extends Field(PLICParams())

/** Platform-Level Interrupt Controller */
class TLPLIC(params: PLICParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule
{
  // plic0 => max devices 1023
  val device = new SimpleDevice("interrupt-controller", Seq("riscv,plic0")) {
    override val alwaysExtended = true
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val extra = Map(
        "interrupt-controller" -> Nil,
        "riscv,ndev" -> Seq(ResourceInt(nDevices)),
        "riscv,max-priority" -> Seq(ResourceInt(params.maxPriorities)),        
        "#interrupt-cells" -> Seq(ResourceInt(1)))
      Description(name, mapping ++ extra)
    }
  }

  val node = TLRegisterNode(
    address   = Seq(params.address),
    device    = device,
    beatBytes = beatBytes,
    undefZero = true,
    concurrency = 1) // limiting concurrency handles RAW hazards on claim registers

  val intnode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false)

  /* Negotiated sizes */
  def nDevices: Int = intnode.edges.in.map(_.source.num).sum
  def nPriorities = min(params.maxPriorities, nDevices)
  def nHarts = intnode.edges.out.map(_.source.num).sum

  // Assign all the devices unique ranges
  lazy val sources = intnode.edges.in.map(_.source)
  lazy val flatSources = (sources zip sources.map(_.num).scanLeft(0)(_+_).init).map {
    case (s, o) => s.sources.map(z => z.copy(range = z.range.offset(o)))
  }.flatten

  ResourceBinding {
    flatSources.foreach { s => s.resources.foreach { r =>
      // +1 because interrupt 0 is reserved
      (s.range.start until s.range.end).foreach { i => r.bind(device, ResourceInt(i+1)) }
    } }
  }

  lazy val module = new LazyModuleImp(this) {
    val (io_devices, edgesIn) = intnode.in.unzip
    val (io_harts, _) = intnode.out.unzip

    // Compact the interrupt vector the same way
    val interrupts = intnode.in.map { case (i, e) => i.take(e.source.num) }.flatten
    // This flattens the harts into an MSMSMSMSMS... or MMMMM.... sequence
    val harts = io_harts.flatten

    println(s"Interrupt map (${nHarts} harts ${nDevices} interrupts):")
    flatSources.foreach { s =>
      // +1 because 0 is reserved, +1-1 because the range is half-open
      println(s"  [${s.range.start+1}, ${s.range.end}] => ${s.name}")
    }
    println("")

    require (nDevices == interrupts.size)
    require (nHarts == harts.size)

    require(nDevices <= PLICConsts.maxDevices)
    require(nHarts > 0 && nHarts <= PLICConsts.maxHarts)

    // For now, use LevelGateways for all TL2 interrupts
    val gateways = Vec((false.B +: interrupts).map { case i =>
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := i
      gateway.io.plic
    })

    val priority =
      if (nPriorities > 0) Reg(Vec(nDevices+1, UInt(width=log2Up(nPriorities+1))))
      else Wire(init=Vec.fill(nDevices+1)(UInt(1)))
    val threshold =
      if (nPriorities > 0) Reg(Vec(nHarts, UInt(width = log2Up(nPriorities+1))))
      else Wire(init=Vec.fill(nHarts)(UInt(0)))
    val pending = Reg(init=Vec.fill(nDevices+1){Bool(false)})
    val enables = Reg(Vec(nHarts, Vec(nDevices+1, Bool())))
    
    def findMax(x: Seq[UInt]): (UInt, UInt) = {
      if (x.length > 1) {
        val half = 1 << (log2Ceil(x.length) - 1)
        val left = findMax(x take half)
        val right = findMax(x drop half)
        MuxT(left._1 >= right._1, left, (right._1, UInt(half) | right._2))
      } else (x.head, UInt(0))
    }

    val maxDevs = Reg(Vec(nHarts, UInt(width = log2Up(pending.size))))
    for (hart <- 0 until nHarts) {
      val effectivePriority = (UInt(1) << priority(0).getWidth) +:
        (for (((p, en), pri) <- (pending zip enables(hart) zip priority).tail)
          yield Cat(p && en, pri))
      val (maxPri, maxDev) = findMax(effectivePriority)

      maxDevs(hart) := maxDev
      harts(hart) := ShiftRegister(Reg(next = maxPri) > Cat(UInt(1), threshold(hart)), params.intStages)
    }

    def priorityRegDesc(i: Int) = RegFieldDesc(s"priority_$i", s"Acting priority of interrupt source $i", reset=if (nPriorities > 0) None else Some(1)) 
    def pendingRegDesc(i: Int) = RegFieldDesc(s"pending_$i", s"Set to 1 if interrupt source $i is pending, regardless of its enable or priority setting.") 
    def priorityRegField(x: UInt, i: Int) = if (nPriorities > 0) RegField(32, x, priorityRegDesc(i)) else RegField.r(32, x, priorityRegDesc(i))
    val priorityRegFields = Seq(PLICConsts.priorityBase -> RegFieldGroup("priority", Some("Acting priorities of each interrupt source. 32 bits for each interrupt source."),
      priority.zipWithIndex.map{case (p, i) => priorityRegField(p, i)}))
    val pendingRegFields = Seq(PLICConsts.pendingBase  -> RegFieldGroup("pending", Some("Pending Bit Array. 1 Bit for each interrupt source."),
      pending.zipWithIndex.map{case (b, i) => RegField.r(1, b, pendingRegDesc(i))}))

 
    val enableRegFields = enables.zipWithIndex.map { case (e, i) =>
      PLICConsts.enableBase(i) -> RegFieldGroup(s"enables_${i}", Some(s"Enable bits for each interrupt source for target $i. 1 bit for each interrupt source."),
        e.zipWithIndex.map{case (b, j) => RegField(1, b, RegFieldDesc(s"enable_${i}_${j}", s"Enable interrupt for source $j for target $i.", reset=None))})
    }

    // When a hart reads a claim/complete register, then the
    // device which is currently its highest priority is no longer pending.
    // This code exploits the fact that, practically, only one claim/complete
    // register can be read at a time. We check for this because if the address map
    // were to change, it may no longer be true.
    // Note: PLIC doesn't care which hart reads the register.
    val claimer = Wire(Vec(nHarts, Bool()))
    assert((claimer.asUInt & (claimer.asUInt - UInt(1))) === UInt(0)) // One-Hot
    val claiming = Vec.tabulate(nHarts){i => Mux(claimer(i), UIntToOH(maxDevs(i), nDevices+1), UInt(0))}
    val claimedDevs = Vec(claiming.reduceLeft( _ | _ ).toBools)

    ((pending zip gateways) zip claimedDevs) foreach { case ((p, g), c) =>
      g.ready := !p
      when (c || g.valid) { p := !c }
    }

    // When a hart writes a claim/complete register, then
    // the written device (as long as it is actually enabled for that
    // hart) is marked complete.
    // This code exploits the fact that, practically, only one claim/complete register
    // can be written at a time. We check for this because if the address map
    // were to change, it may no longer be true.
    // Note -- PLIC doesn't care which hart writes the register.
    val completer = Wire(Vec(nHarts, Bool()))
    assert((completer.asUInt & (completer.asUInt - UInt(1))) === UInt(0)) // One-Hot
    val completerDev = Wire(UInt(width = log2Up(nDevices + 1)))
    val completedDevs = Mux(completer.reduce(_ || _), UIntToOH(completerDev, nDevices+1), UInt(0))
    (gateways zip completedDevs.toBools) foreach { case (g, c) =>
       g.complete := c
    }

    def thresholdRegDesc(i: Int) = RegFieldDesc(s"threshold_$i", s"Interrupt & claim threshold for target $i", reset=if (nPriorities > 0) None else Some(1))
    def thresholdRegField(x: UInt, i: Int) = if (nPriorities > 0) RegField(32, x, thresholdRegDesc(i)) else RegField.r(32, x, thresholdRegDesc(i))

    val hartRegFields = Seq.tabulate(nHarts) { i =>
      PLICConsts.hartBase(i) -> Seq(
        thresholdRegField(threshold(i), i),
        RegField(32,
          RegReadFn { valid =>
            claimer(i) := valid
            (Bool(true), maxDevs(i))
          },
          RegWriteFn { (valid, data) =>
            assert(completerDev === data.extract(log2Ceil(nDevices+1)-1, 0), 
                   "completerDev should be consistent for all harts")
            completerDev := data.extract(log2Ceil(nDevices+1)-1, 0)
            completer(i) := valid && enables(i)(completerDev)
            Bool(true)
          },
          Some(RegFieldDesc(s"claim_complete_$i",
            s"Claim/Complete register for Target $i. Reading this register returns the claimed interrupt number and makes it no longer pending." +
            s"Writing the interrupt number back completes the interrupt.",
            reset = None,
            access = RegFieldAccessType.RWSPECIAL))
        )
      )
    } 

    node.regmap((priorityRegFields ++ pendingRegFields ++ enableRegFields ++ hartRegFields):_*)

    priority(0) := 0
    pending(0) := false
    for (e <- enables)
      e(0) := false

    if (nDevices >= 2) {
      val claimed = claimer(0) && maxDevs(0) > 0
      val completed = completer(0)
      cover(claimed && RegEnable(claimed, false.B, claimed || completed), "TWO_CLAIMS", "two claims with no intervening complete")
      cover(completed && RegEnable(completed, false.B, claimed || completed), "TWO_COMPLETES", "two completes with no intervening claim")

      val ep = enables(0).asUInt & pending.asUInt
      val ep2 = RegNext(ep)
      val diff = ep & ~ep2
      cover((diff & (diff - 1)) =/= 0, "TWO_INTS_PENDING", "two enabled interrupts became pending on same cycle")

      if (nPriorities > 0)
        ccover(maxDevs(0) > (UInt(1) << priority(0).getWidth) && maxDevs(0) <= Cat(UInt(1), threshold(0)),
               "THRESHOLD", "interrupt pending but less than threshold")
    }

    def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
      cover(cond, s"PLIC_$label", "Interrupts;;" + desc)
  }
}

/** Trait that will connect a PLIC to a subsystem */
trait HasPeripheryPLIC { this: BaseSubsystem =>
  val plic  = LazyModule(new TLPLIC(p(PLICKey), pbus.beatBytes))
  pbus.toVariableWidthSlave(Some("plic")) { plic.node }
  plic.intnode := ibus.toPLIC
}
