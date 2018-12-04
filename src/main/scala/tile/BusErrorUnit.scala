// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.util.Valid
import chisel3.core.DontCare
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util.property._

trait BusErrors extends Bundle {
  def toErrorList: List[Option[(Valid[UInt], String, String)]]
}

class L1BusErrors(implicit p: Parameters) extends CoreBundle()(p) with BusErrors {
  val icache = new ICacheErrors
  val dcache = new DCacheErrors

  def toErrorList = List(None, None,
      icache.correctable.map((_, "I_CORRECTABLE", "Instruction cache or ITIM correctable ECC error ")),
      icache.uncorrectable.map((_, "I_UNCORRECTABLE", "ITIM uncorrectable ECC error")),
      None,
      Some((dcache.bus, "DBUS", "Load or store TileLink bus error")),
      dcache.correctable.map((_, "D_CORRECTABLE", "Data cache correctable ECC error")),
      dcache.uncorrectable.map((_, "D_UNCORRECTABLE", "Data cache uncorrectable ECC error")))
}

case class BusErrorUnitParams(addr: BigInt, size: Int = 4096)

class BusErrorUnit[T <: BusErrors](t: => T, params: BusErrorUnitParams)(implicit p: Parameters) extends LazyModule {
  val regWidth = 64
  val device = new SimpleDevice("bus-error-unit", Seq("sifive,buserror0"))
  val intNode = IntSourceNode(IntSourcePortSimple(resources = device.int))
  val node = TLRegisterNode(
    address   = Seq(AddressSet(params.addr, params.size-1)),
    device    = device,
    beatBytes = p(XLen)/8)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val errors = t.flip
      val interrupt = Bool().asOutput
    })

    val sources_and_desc = io.errors.toErrorList
    val sources = sources_and_desc.map(_.map(_._1))
    val sources_enums = sources_and_desc.zipWithIndex.flatMap{case (s, i) => s.map {e => (BigInt(i) -> (e._2, e._3))}}

    val causeWidth = log2Ceil(sources.lastIndexWhere(_.nonEmpty) + 1)
    val (cause, cause_desc) = DescribedReg(UInt(causeWidth.W),
      "cause", "Cause of error event", reset=Some(0.U(causeWidth.W)), volatile=true, enumerations=sources_enums.toMap)

    val (value, value_desc) = DescribedReg(UInt(width = sources.flatten.map(_.bits.getWidth).max),
      "value", "Physical address of error event", reset=None, volatile=true)
    require(value.getWidth <= regWidth)

    val enable = Reg(init = Vec(sources.map(_.nonEmpty.B)))
    val enable_desc =  sources.zipWithIndex.map { case (s, i) =>
      if (s.nonEmpty) RegFieldDesc(s"enable_$i", "", reset=Some(1))
      else RegFieldDesc.reserved
    }

    val global_interrupt = Reg(init = Vec.fill(sources.size)(false.B))
    val global_interrupt_desc = sources.zipWithIndex.map { case (s, i) =>
      if (s.nonEmpty) RegFieldDesc(s"plic_interrupt_$i", "", reset=Some(0))
      else RegFieldDesc.reserved
    }

    val accrued = Reg(init = Vec.fill(sources.size)(false.B))
    val accrued_desc = sources.zipWithIndex.map { case (s, i) =>
      if (s.nonEmpty) RegFieldDesc(s"accrued_$i", "", reset=Some(0), volatile = true)
      else RegFieldDesc.reserved
    }

    val local_interrupt = Reg(init = Vec.fill(sources.size)(false.B))
    val local_interrupt_desc = sources.zipWithIndex.map { case (s, i) =>
      if (s.nonEmpty) RegFieldDesc(s"local_interrupt_$i", "", reset=Some(0))
      else RegFieldDesc.reserved
    }

    val cause_wen = Wire(init = false.B)
    val new_cause = Wire(UInt(causeWidth.W), DontCare)
    val new_value = Wire(UInt(value.getWidth.W), DontCare)
    for ((((s, en), acc), i) <- (sources zip enable zip accrued).zipWithIndex; if s.nonEmpty) {
      when (s.get.valid) {
        acc := true
        when (en) {
          cause_wen := true
          new_cause := i
          new_value := s.get.bits
        }
        cover(en, s"BusErrorCause_$i", s"Core;;BusErrorCause $i covered")
      }
    }

    when (cause === 0 && cause_wen) {
      cause := OptimizationBarrier(new_cause)
      value := OptimizationBarrier(new_value)
    }

    val (int_out, _) = intNode.out(0)
    io.interrupt := (accrued.asUInt & local_interrupt.asUInt).orR
    int_out(0) := (accrued.asUInt & global_interrupt.asUInt).orR

    def reg(r: UInt, gn: String, d: RegFieldDesc) = RegFieldGroup(gn, None, RegField.bytes(r, (r.getWidth + 7)/8, Some(d)))
    def reg(v: Vec[Bool], gn: String, gd: String, d: Seq[RegFieldDesc]) =
      RegFieldGroup(gn, Some(gd), (v zip d).map {case (r, rd) => RegField(1, r, rd)})
    def numberRegs(x: Seq[Seq[RegField]]) = x.zipWithIndex.map {case (f, i) => (i * regWidth / 8) -> f }

    node.regmap(numberRegs(Seq(
      reg(cause, "cause", cause_desc),
      reg(value, "value", value_desc),
      reg(enable, "enable", "Event enable mask", enable_desc),
      reg(global_interrupt, "plic_interrupt", "Platform-level interrupt enable mask", global_interrupt_desc),
      reg(accrued, "accrued", "Accrued event mask" ,accrued_desc),
      reg(local_interrupt,  "local_interrupt", "Hart-local interrupt-enable mask", local_interrupt_desc))):_*)

    // hardwire mask bits for unsupported sources to 0
    for ((s, i) <- sources.zipWithIndex; if s.isEmpty) {
      enable(i) := false
      global_interrupt(i) := false
      accrued(i) := false
      local_interrupt(i) := false
    }
  }
}
