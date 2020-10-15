// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{HasTileLinkLocations, TLBusWrapperLocation, CBUS}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.diplomaticobjectmodel.model._

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
  def maxMaxHarts = 15872
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

  def size(maxHarts: Int): Int = {
    require(maxHarts > 0 && maxHarts <= maxMaxHarts, s"Must be: maxHarts=$maxHarts > 0 && maxHarts <= PLICConsts.maxMaxHarts=${PLICConsts.maxMaxHarts}")
    1 << log2Ceil(hartBase(maxHarts))
  }

  require(hartBase >= enableBase(maxMaxHarts))
}

case class PLICParams(baseAddress: BigInt = 0xC000000, maxPriorities: Int = 7, intStages: Int = 0, maxHarts: Int = PLICConsts.maxMaxHarts)
{
  require (maxPriorities >= 0)
  def address = AddressSet(baseAddress, PLICConsts.size(maxHarts)-1)
}

case object PLICKey extends Field[Option[PLICParams]](None)

case class PLICAttachParams(
  slaveWhere: TLBusWrapperLocation = CBUS
)

case object PLICAttachKey extends Field(PLICAttachParams())

/** Platform-Level Interrupt Controller */
class TLPLIC(params: PLICParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule
{
  // plic0 => max devices 1023
  val device: SimpleDevice = new SimpleDevice("interrupt-controller", Seq("riscv,plic0")) {
    override val alwaysExtended = true
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val extra = Map(
        "interrupt-controller" -> Nil,
        "riscv,ndev" -> Seq(ResourceInt(nDevices)),
        "riscv,max-priority" -> Seq(ResourceInt(nPriorities)),
        "#interrupt-cells" -> Seq(ResourceInt(1)))
      Description(name, mapping ++ extra)
    }
  }

  val node : TLRegisterNode = TLRegisterNode(
    address   = Seq(params.address),
    device    = device,
    beatBytes = beatBytes,
    undefZero = true,
    concurrency = 1) // limiting concurrency handles RAW hazards on claim registers

  /** The inward side of the this node is edges coming from external devices or
    * an IntXbar that has aggregated over such devices.
    * The outward side of this node has edges going to a core or local interrupt controller
    * in pairs of meip/seip lines. Each outward node picks the number of interrupts based on
    * whether or not it supports supervisor mode.
    */
  val intnode: IntNexusNode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(None, Seq(Resource(device, "int"))))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false)

  // Negotiated sizes
  def nDevices: Int = intnode.edges.in.map(_.num).sum
  def minPriorities = min(params.maxPriorities, nDevices)
  def nPriorities = (1 << log2Ceil(minPriorities+1)) - 1 // round up to next 2^n-1
  def nHarts = intnode.edges.out.map(_.num).sum

  // Deterministic sizes
  val numReserved = 1 // interrupt 0 is reserved

  // Assign all the devices unique ranges and make records of the finalized maps
  ResourceBinding {
    intnode.edges.in.zip(intnode.edges.in.runningTotal).foreach { case (edge, offset) =>
      edge.bindDevice(device, (i: Int) => Some(i + numReserved + offset))
    }
  }

  def description = {
    s"Platform-Level Interrupt Map (${nHarts} hart contexts, ${nDevices} interrupts):\n" +
    intnode.edges.in.flattenSources(numReserved).map("  " + _.description).mkString("\n")
  }

  lazy val module = new LazyModuleImp(this) {
    println(description+"\n")

    val (io_devices, edgesIn) = intnode.in.unzip
    val (io_harts, _) = intnode.out.unzip

    // Compact the incoming device interrupts into a single sequence
    val interrupts: Seq[Bool] = intnode.in.map { case (i, e) => i.take(e.num) }.flatten
    // Compact the outgoing hart interrupts into an MSMSMSMSMS... or MMMMM.... sequence
    val harts: Seq[Bool] = io_harts.flatten

    require (nDevices == interrupts.size, s"Must be: nDevices=$nDevices == interrupts.size=${interrupts.size}")
    require (nHarts == harts.size, s"Must be: nHarts=$nHarts == harts.size=${harts.size}")
    require(nDevices <= PLICConsts.maxDevices, s"Must be: nDevices=$nDevices <= PLICConsts.maxDevices=${PLICConsts.maxDevices}")
    require(nHarts > 0 && nHarts <= params.maxHarts, s"Must be: nHarts=$nHarts > 0 && nHarts <= PLICParams.maxHarts=${params.maxHarts}")

    // For now, use LevelGateways for all incoming diplomatic interrupts
    val gateways = interrupts.map { case i =>
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := i
      gateway.io.plic
    }

    val prioBits = log2Ceil(nPriorities+1)
    val priority =
      if (nPriorities > 0) Reg(Vec(nDevices, UInt(width=prioBits)))
      else Wire(init=Vec.fill(nDevices max 1)(UInt(1)))
    val threshold =
      if (nPriorities > 0) Reg(Vec(nHarts, UInt(width=prioBits)))
      else Wire(init=Vec.fill(nHarts)(UInt(0)))
    val pending = Reg(init=Vec.fill(nDevices max 1){Bool(false)})

    /* Construct the enable registers, chunked into 8-bit segments to reduce verilog size */
    val firstEnable = nDevices min 7
    val fullEnables = (nDevices - firstEnable) / 8
    val tailEnable  = nDevices - firstEnable - 8*fullEnables
    def enableRegs = (Reg(UInt(width = firstEnable)) +:
                      Seq.fill(fullEnables) { Reg(UInt(width = 8)) }) ++
                     (if (tailEnable > 0) Some(Reg(UInt(width = tailEnable))) else None)
    val enables = Seq.fill(nHarts) { enableRegs }
    val enableVec = Vec(enables.map(x => Cat(x.reverse)))
    val enableVec0 = Vec(enableVec.map(x => Cat(x, UInt(0, width=1))))
    
    val maxDevs = Reg(Vec(nHarts, UInt(width = log2Ceil(nDevices+1))))
    val pendingUInt = Cat(pending.reverse)
    for (hart <- 0 until nHarts) {
      val fanin = Module(new PLICFanIn(nDevices, prioBits))
      fanin.io.prio := priority
      fanin.io.ip   := enableVec(hart) & pendingUInt
      maxDevs(hart) := fanin.io.dev
      harts(hart)   := ShiftRegister(Reg(next = fanin.io.max) > threshold(hart), params.intStages)
    }

    def priorityRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"priority_$i",
        desc      = s"Acting priority of interrupt source $i",
        group     = Some("priority"),
        groupDesc = Some("Acting priorities of each interrupt source."),
        reset     = if (nPriorities > 0) None else Some(1))

    def pendingRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"pending_$i",
        desc      = s"Set to 1 if interrupt source $i is pending, regardless of its enable or priority setting.",
        group     = Some("pending"),
        groupDesc = Some("Pending Bit Array. 1 Bit for each interrupt source."),
        volatile = true)

    def enableRegDesc(i: Int, j: Int, wide: Int) = {
      val low = if (j == 0) 1 else j*8
      val high = low + wide - 1
      RegFieldDesc(
        name      = s"enables_${j}",
        desc      = s"Targets ${low}-${high}. Set bits to 1 if interrupt should be enabled.",
        group     = Some(s"enables_${i}"),
        groupDesc = Some(s"Enable bits for each interrupt source for target $i. 1 bit for each interrupt source."))
    }

    def priorityRegField(x: UInt, i: Int) =
      if (nPriorities > 0) {
        RegField(prioBits, x, priorityRegDesc(i))
      } else {
        RegField.r(prioBits, x, priorityRegDesc(i))
      }

    val priorityRegFields = priority.zipWithIndex.map { case (p, i) =>
      PLICConsts.priorityBase+4*(i+1) -> Seq(priorityRegField(p, i+1)) }
    val pendingRegFields = Seq(PLICConsts.pendingBase ->
      (RegField(1) +: pending.zipWithIndex.map { case (b, i) => RegField.r(1, b, pendingRegDesc(i+1))}))
    val enableRegFields = enables.zipWithIndex.map { case (e, i) =>
      PLICConsts.enableBase(i) -> (RegField(1) +: e.zipWithIndex.map { case (x, j) =>
        RegField(x.getWidth, x, enableRegDesc(i, j, x.getWidth)) }) }

    // When a hart reads a claim/complete register, then the
    // device which is currently its highest priority is no longer pending.
    // This code exploits the fact that, practically, only one claim/complete
    // register can be read at a time. We check for this because if the address map
    // were to change, it may no longer be true.
    // Note: PLIC doesn't care which hart reads the register.
    val claimer = Wire(Vec(nHarts, Bool()))
    assert((claimer.asUInt & (claimer.asUInt - UInt(1))) === UInt(0)) // One-Hot
    val claiming = Seq.tabulate(nHarts){i => Mux(claimer(i), maxDevs(i), UInt(0))}.reduceLeft(_|_)
    val claimedDevs = Vec(UIntToOH(claiming, nDevices+1).asBools)

    ((pending zip gateways) zip claimedDevs.tail) foreach { case ((p, g), c) =>
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
    (gateways zip completedDevs.asBools.tail) foreach { case (g, c) =>
       g.complete := c
    }

    def thresholdRegDesc(i: Int) =
      RegFieldDesc(
        name = s"threshold_$i",
        desc = s"Interrupt & claim threshold for target $i. Maximum value is ${nPriorities}.",
        reset    = if (nPriorities > 0) None else Some(1))

    def thresholdRegField(x: UInt, i: Int) =
      if (nPriorities > 0) {
        RegField(prioBits, x, thresholdRegDesc(i))
      } else {
        RegField.r(prioBits, x, thresholdRegDesc(i))
      }

    val hartRegFields = Seq.tabulate(nHarts) { i =>
      PLICConsts.hartBase(i) -> Seq(
        thresholdRegField(threshold(i), i),
        RegField(32-prioBits),
        RegField(32,
          RegReadFn { valid =>
            claimer(i) := valid
            (Bool(true), maxDevs(i))
          },
          RegWriteFn { (valid, data) =>
            assert(completerDev === data.extract(log2Ceil(nDevices+1)-1, 0), 
                   "completerDev should be consistent for all harts")
            completerDev := data.extract(log2Ceil(nDevices+1)-1, 0)
            completer(i) := valid && enableVec0(i)(completerDev)
            Bool(true)
          },
          Some(RegFieldDesc(s"claim_complete_$i",
            s"Claim/Complete register for Target $i. Reading this register returns the claimed interrupt number and makes it no longer pending." +
            s"Writing the interrupt number back completes the interrupt.",
            reset = None,
            wrType = Some(RegFieldWrType.MODIFY),
            rdAction = Some(RegFieldRdAction.MODIFY),
            volatile = true))
        )
      )
    }

    val omRegMap : OMRegisterMap = node.regmap((priorityRegFields ++ pendingRegFields ++ enableRegFields ++ hartRegFields):_*)

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

class PLICFanIn(nDevices: Int, prioBits: Int) extends Module {
  val io = new Bundle {
    val prio = Vec(nDevices, UInt(width = prioBits)).flip
    val ip   = UInt(width = nDevices).flip
    val dev  = UInt(width = log2Ceil(nDevices+1))
    val max  = UInt(width = prioBits)
  }

  val effectivePriority = (UInt(1) << prioBits) +: (io.ip.asBools zip io.prio).map { case (p, x) => Cat(p, x) }
  val (maxPri, maxDev) = findMax(effectivePriority)
  io.max := maxPri // strips the always-constant high '1' bit
  io.dev := maxDev
}

/** Trait that will connect a PLIC to a subsystem */
trait CanHavePeripheryPLIC { this: HasTileLinkLocations =>
  val plicOpt  = p(PLICKey).map { params =>
    val tlbus = locateTLBusWrapper(p(PLICAttachKey).slaveWhere)
    val plic = LazyModule(new TLPLIC(params, tlbus.beatBytes))
    plic.node := tlbus.coupleTo("plic") { TLFragmenter(tlbus) := _ }
    plic.intnode :=* ibus.toPLIC
    plic
  }
}
