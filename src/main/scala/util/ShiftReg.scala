// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._


object ShiftReg {
  /** Similar to Chisel ShiftRegister, but allows the user to
    * specify a name and initial value. This is different from 
    * ShiftRegInit in that it allows the enable signal to be specified. 
    * Returns the n-cycle delayed version of the input signal.
    *
    * @param in input to delay
    * @param n number of cycles to delay
    * @param en enable the shift
    * @param name set the elaborated name of the registers.
    */
  def apply[T <: Chisel.Data](in: T,
    n: Int,
    en: Chisel.Bool = Chisel.Bool(true),
    name: Option[String] = None): T = {
    // The order of tests reflects the expected use cases.
    if (n != 0) {
      val r = Chisel.RegEnable(apply(in, n-1, en, name), en)
      name.foreach { na =>  r.suggestName(s"${na}_pipe_${n-1}") }
      r
    } else {
      in
    }
  }
  
  /** Returns the n-cycle delayed version of the input signal with reset initialization.
    *
    * @param in input to delay
    * @param n number of cycles to delay
    * @param init reset value for each register in the shift
    * @param en enable the shift
    * @param name set the elaborated name of the registers.
    */
  def apply[T <: Chisel.Data](in: T, n: Int, init: T, en: Chisel.Bool, name: Option[String]): T = {
    // The order of tests reflects the expected use cases.
    if (n != 0) {
      val r = Chisel.RegEnable(apply(in, n-1, init, en, name), init, en)
      if (name.isDefined) r.suggestName(s"${name.get}_pipe_${n-1}")
      r
    } else {
      in
    }
  }

  def apply[T <: Chisel.Data](in: T, n: Int, init: T, name: Option[String]): T = {
    apply(in, n, en = Bool(true), name)
  }
}
// Similar to the Chisel ShiftRegister but allows the user to suggest a
// name to the registers that get instantiated, and
// to provide a reset value.
object ShiftRegInit {
  def apply[T <: Data](in: T, n: Int, init: T, name: Option[String] = None): T =
  ShiftReg(in, n, init, en = Bool(true), name)
}

/** These wrap behavioral
  *  shift registers  into specific
  *  modules to allow for 
  *  backend flows to replace or constrain
  *  them properly when used for CDC synchronization,
  *  rather than buffering.
  *  
  *  The 3 different types vary in their reset behavior:
  *  AsyncResetShiftReg -- This is identical to the AsyncResetSynchronizerShiftReg, 
  *      it is just named differently
  *      to distinguish its use case. This is a ShiftRegister meant for timing, 
  *      not for synchronization.
  *  AsyncResetSynchronizerShiftReg -- asynchronously reset to 0
  *  SynchronizerShiftReg -- no reset, pipeline only.
  *  
  */

abstract class AbstractPipelineReg(w: Int = 1) extends Module {
  val io = new Bundle {
    val d = UInt(INPUT, width = w)
    val q = UInt(OUTPUT, width = w)
  }
}

object AbstractPipelineReg {
  def apply [T <: Chisel.Data](gen: => AbstractPipelineReg, in: T, name: Option[String] = None): T = {
    val chain = Module(gen)
    name.foreach{ chain.suggestName(_) }
    chain.io.d := in.asUInt
    chain.io.q.asTypeOf(in)
  }
}

class AsyncResetShiftReg(w: Int = 1, depth: Int = 1, init: Int = 0, name: String = "pipe") extends AbstractPipelineReg(w) {
  require(depth > 0, "Depth must be greater than 0.")

  override def desiredName = s"AsyncResetShiftReg_w${w}_d${depth}_i${init}"

  val chain = List.tabulate(depth) { i =>
    Module (new AsyncResetRegVec(w, init)).suggestName(s"${name}_${i}")
  }

  chain.last.io.d := io.d
  chain.last.io.en := Bool(true)

  (chain.init zip chain.tail).foreach { case (sink, source) =>
    sink.io.d := source.io.q
    sink.io.en := Bool(true)
  }
  io.q := chain.head.io.q
}

object AsyncResetShiftReg {
  def apply [T <: Chisel.Data](in: T, depth: Int = 1, init: Int = 0, name: Option[String] = None ): T =
    AbstractPipelineReg(new AsyncResetShiftReg(in.getWidth, depth, init), in, name)
}

// Note that it is important to ovveride "name" in order to ensure that the Chisel dedup does
// not try to merge instances of this with instances of the superclass.
class AsyncResetSynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends AsyncResetShiftReg(w, depth = sync, name = "sync") {
  require(sync > 0, "Sync must be greater than 0.")
  override def desiredName = s"AsyncResetSynchronizerShiftReg_w${w}_d${sync}"
}

object AsyncResetSynchronizerShiftReg {
  def apply [T <: Chisel.Data](in: T, sync: Int = 3, name: Option[String] = None): T =
    AbstractPipelineReg(new AsyncResetSynchronizerShiftReg(in.getWidth, sync), in, name)
}

class SynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends AbstractPipelineReg(w) {
  require(sync > 0, "Sync must be greater than 0.")

  override def desiredName = s"SynchronizerShiftReg_w${w}_d${sync}"

  val syncv = List.tabulate(sync) { i =>
    val r = Reg(UInt(width = w))
    r.suggestName(s"sync_${i}")
  }

  syncv.last := io.d

  (syncv.init zip syncv.tail).foreach { case (sink, source) =>
    sink := source
  }
  io.q := syncv.head
}

object SynchronizerShiftReg {
  def apply [T <: Chisel.Data](in: T, sync: Int = 3, name: Option[String] = None): T =
    AbstractPipelineReg(new SynchronizerShiftReg(in.getWidth, sync), in, name)
}
