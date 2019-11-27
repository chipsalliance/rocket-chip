// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.{RegEnable, Cat}

/**  These wrap behavioral
  *  shift and next registers into specific modules to allow for
  *  backend flows to replace or constrain
  *  them properly when used for CDC synchronization,
  *  rather than buffering.
  *  
  *  
  *  These are built up of *ResetSynchronizerPrimitiveShiftReg,
  *  intended to be replaced by the integrator's metastable flops chains or replaced
  *  at this level if they have a multi-bit wide synchronizer primitive.
  *  The different types vary in their reset behavior:
  *  SyncNonResetSynchronizerShiftReg  -- Register array which ties the internal synchronous reset to false.
  *  AsyncResetSynchronizerShiftReg    -- Asynchronously reset register array, constructed from W instantiations of D deep
  *                                       1-bit-wide shift registers.
  *  SyncResetSynchronizerShiftReg     -- Synchronously reset register array, constructed similarly to AsyncResetSynchronizerShiftReg
  *    
  *  
  *  
  *  ClockCrossingReg                  -- Not made up of SynchronizerPrimitiveShiftReg. This is for single-deep flops which cross
  *                                       Clock Domains.
*/

object SynchronizerResetType extends Enumeration {
  val NonSync, Sync, Async = Value
}

import SynchronizerResetType._

private class SynchronizerPrimitiveShiftReg(sync: Int = 3, resetType: SynchronizerResetType.Value)
    extends AbstractPipelineReg(1) {

  override def desiredName = s"${resetType.toString}ResetSynchronizerPrimitiveShiftReg_d${sync}"

  val local_reset = resetType match {
    case SynchronizerResetType.NonSync => reset // unused because a RegInit is not used
    case SynchronizerResetType.Sync => reset.asBool
    case SynchronizerResetType.Async => reset.asAsyncReset
  }

  withReset(local_reset){
    val chain = List.tabulate(sync) { i =>
      val reg = if (resetType == SynchronizerResetType.NonSync) Reg(Bool()) else RegInit(false.B)
      reg.suggestName(s"sync_$i")
    }
    chain.last := io.d.asBool

    (chain.init zip chain.tail).foreach { case (sink, source) =>
      sink := source
    }
    io.q := chain.head.asUInt
  }
}

private object SynchronizerPrimitiveShiftReg {
  def apply (in: Bool, sync: Int, resetType: SynchronizerResetType.Value): Bool =
    AbstractPipelineReg(new SynchronizerPrimitiveShiftReg(sync, resetType), in)
}

class AsyncResetSynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends AbstractPipelineReg(w) {
  require(sync > 1, "Sync must be greater than 1.")
  override def desiredName = s"AsyncResetSynchronizerShiftReg_w${w}_d${sync}_i0"
  val output = Seq.tabulate(w) { i => SynchronizerPrimitiveShiftReg(io.d(i), sync, SynchronizerResetType.Async)}
  io.q := Cat(output.reverse)
}

object AsyncResetSynchronizerShiftReg {
  def apply [T <: Chisel.Data](in: T, sync: Int, name: Option[String] = None): T =
    AbstractPipelineReg(new AsyncResetSynchronizerShiftReg(in.getWidth, sync), in, name)

  def apply [T <: Chisel.Data](in: T, sync: Int): T =
    apply (in, sync, None)
}

class SynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends AbstractPipelineReg(w) {
  require(sync > 1, "Sync must be greater than 1.")
  override def desiredName = s"SynchronizerShiftReg_w${w}_d${sync}"
    val output = Seq.tabulate(w) { i => SynchronizerPrimitiveShiftReg(io.d(i), sync, SynchronizerResetType.NonSync) }
    io.q := Cat(output.reverse)
}

object SynchronizerShiftReg {
  def apply [T <: Chisel.Data](in: T, sync: Int, name: Option[String] = None): T =
    AbstractPipelineReg(new SynchronizerShiftReg(in.getWidth, sync), in, name)

  def apply [T <: Chisel.Data](in: T, sync: Int): T =
    apply (in, sync, None)
}

class SyncResetSynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends AbstractPipelineReg(w) {
  require(sync > 1, "Sync must be greater than 1.")
  override def desiredName = s"SyncResetSynchronizerShiftReg_w${w}_d${sync}_i0"
  val output = Seq.tabulate(w) { i => SynchronizerPrimitiveShiftReg(io.d(i), sync, SynchronizerResetType.Sync) }
  io.q := Cat(output.reverse)
}

object SyncResetSynchronizerShiftReg {
  def apply [T <: Chisel.Data](in: T, sync: Int, name: Option[String] = None): T =
    AbstractPipelineReg(new SyncResetSynchronizerShiftReg(in.getWidth, sync), in, name)

  def apply [T <: Chisel.Data](in: T, sync: Int): T =
    apply (in, sync, None)
}

class ClockCrossingReg(w: Int = 1, doInit: Boolean) extends Module {

  override def desiredName = s"ClockCrossingReg_w${w}"

  val io = IO(new Bundle{
    val d = Input(UInt(w.W))
    val q = Output(UInt(w.W))
    val en = Input(Bool())
  })

  val cdc_reg = if (doInit) RegEnable(next=io.d, init=0.U(w.W), enable=io.en) else RegEnable(next=io.d, enable=io.en)
  io.q := cdc_reg
}


object ClockCrossingReg {
  def apply [T <: Chisel.Data](in: T, en: Bool, doInit: Boolean, name: Option[String] = None): T = {
    val cdc_reg = Module(new ClockCrossingReg(in.getWidth, doInit))
    name.foreach{ cdc_reg.suggestName(_) }
    cdc_reg.io.d := in.asUInt
    cdc_reg.io.en := en
    cdc_reg.io.q.asTypeOf(in)
  }
}
