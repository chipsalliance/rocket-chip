// See LICENSE.SiFive for license details.

package uncore.devices

import Chisel._
import Chisel.ImplicitConversions._

import junctions._
import diplomacy._
import regmapper._
import uncore.tilelink2._
import config._
import scala.math.min
import tile.XLen

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

/** Platform-Level Interrupt Controller */
class TLPLIC(maxPriorities: Int, address: BigInt = 0xC000000)(implicit p: Parameters) extends LazyModule
{
  require (maxPriorities >= 0)

  // plic0 => max devices 1023
  val device = new SimpleDevice("interrupt-controller", Seq("riscv,plic0")) {
    override val alwaysExtended = true
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val extra = Map(
        "interrupt-controller" -> Nil,
        "riscv,ndev" -> Seq(ResourceInt(nDevices)),
        "#interrupt-cells" -> Seq(ResourceInt(1)),
        "#address-cells" -> Seq(ResourceInt(0)))
      Description(name, mapping ++ extra)
    }
  }

  val node = TLRegisterNode(
    address   = AddressSet(address, PLICConsts.size-1),
    device    = device,
    beatBytes = p(XLen)/8,
    undefZero = false)

  val intnode = IntNexusNode(
    numSourcePorts = 0 to 1024,
    numSinkPorts   = 0 to 1024,
    sourceFn       = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) })

  /* Negotiated sizes */
  def nDevices: Int = intnode.edgesIn.map(_.source.num).sum
  def nPriorities = min(maxPriorities, nDevices)
  def nHarts = intnode.edgesOut.map(_.source.num).sum

  // Assign all the devices unique ranges
  lazy val sources = intnode.edgesIn.map(_.source)
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
    val io = new Bundle {
      val tl_in = node.bundleIn
      val devices = intnode.bundleIn
      val harts = intnode.bundleOut
    }

    // Compact the interrupt vector the same way
    val interrupts = (intnode.edgesIn zip io.devices).map { case (e, i) => i.take(e.source.num) }.flatten
    // This flattens the harts into an MSMSMSMSMS... or MMMMM.... sequence
    val harts = io.harts.flatten

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
    val gateways = Vec(interrupts.map { case i =>
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := i
      gateway.io.plic
    } ++ (if (interrupts.isEmpty) Some(Wire(new GatewayPLICIO)) else None))

    val priority =
      if (nPriorities > 0) Reg(Vec(nDevices+1, UInt(width=log2Up(nPriorities+1))))
      else Wire(init=Vec.fill(nDevices+1)(UInt(1)))
    val threshold =
      if (nPriorities > 0) Reg(Vec(nHarts, UInt(width = log2Up(nPriorities+1))))
      else Wire(init=Vec.fill(nHarts)(UInt(0)))
    val pending = Reg(init=Vec.fill(nDevices+1){Bool(false)})
    val enables = Reg(Vec(nHarts, Vec(nDevices+1, Bool())))

    for ((p, g) <- pending.tail zip gateways) {
      g.ready := !p
      g.complete := false
      when (g.valid) { p := true }
    }

    def findMax(x: Seq[UInt]): (UInt, UInt) = {
      if (x.length > 1) {
        val half = 1 << (log2Ceil(x.length) - 1)
        val lMax = findMax(x take half)
        val rMax = findMax(x drop half)
        val useLeft = lMax._1 >= rMax._1
        (Mux(useLeft, lMax._1, rMax._1), Mux(useLeft, lMax._2, UInt(half) | rMax._2))
      } else (x.head, UInt(0))
    }

    val maxDevs = Reg(Vec(nHarts, UInt(width = log2Up(pending.size))))
    for (hart <- 0 until nHarts) {
      val effectivePriority =
        for (((p, en), pri) <- (pending zip enables(hart) zip priority).tail)
          yield Cat(p && en, pri)
      val (maxPri, maxDev) = findMax((UInt(1) << priority(0).getWidth) +: effectivePriority)

      maxDevs(hart) := maxDev
      harts(hart) := Reg(next = maxPri) > Cat(UInt(1), threshold(hart))
    }

    def priorityRegField(x: UInt) = if (nPriorities > 0) RegField(32, x) else RegField.r(32, x)
    val priorityRegFields = Seq(PLICConsts.priorityBase -> priority.map(p => priorityRegField(p)))
    val pendingRegFields = Seq(PLICConsts.pendingBase  -> pending .map(b => RegField.r(1, b)))

    val enableRegFields = enables.zipWithIndex.map { case (e, i) =>
      PLICConsts.enableBase(i) -> e.map(b => RegField(1, b))
    }

    val hartRegFields = Seq.tabulate(nHarts) { i =>
      PLICConsts.hartBase(i) -> Seq(
        priorityRegField(threshold(i)),
        RegField(32,
          RegReadFn { valid =>
            when (valid) {
              pending(maxDevs(i)) := Bool(false)
              maxDevs(i) := UInt(0) // flush pipeline
            }
            (Bool(true), maxDevs(i))
          },
          RegWriteFn { (valid, data) =>
            when (valid && enables(i)(data)) {
              gateways(data - UInt(1)).complete := Bool(true)
            }
            Bool(true)
          }
        )
      )
    }

    node.regmap((priorityRegFields ++ pendingRegFields ++ enableRegFields ++ hartRegFields):_*)

    priority(0) := 0
    pending(0) := false
    for (e <- enables)
      e(0) := false
  }
}
