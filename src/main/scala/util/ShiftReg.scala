// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

// Similar to the Chisel ShiftRegister but allows the user to suggest a
// name to the registers that get instantiated, and
// to provide a reset value.
object ShiftRegInit {
  def apply[T <: Data](in: T, n: Int, init: T, name: Option[String] = None): T =

  (0 until n).foldRight(in) {
    case (i, next) => {
      val r = Reg(next, next = next, init = init)
      name.foreach { na => r.suggestName(s"${na}_${i}") }
      r
    }
  }
}

/** These wrap behavioral
  *  shift registers  into specific modules to allow for
  *  backend flows to replace or constrain
  *  them properly when used for CDC synchronization,
  *  rather than buffering.
  *  
  *  The different types vary in their reset behavior:
  *  AsyncResetShiftReg -- This is identical to the AsyncResetSynchronizerShiftReg, 
  *      it is just named differently to distinguish its use case.
  *      This is an async ShiftRegister meant for timing,
  *      not for synchronization.
  *  AsyncResetSynchronizerShiftReg -- asynchronously reset to specific value.
  *  SyncResetSynchronizerShiftReg  -- reset to specific value.
  *  SynchronizerShiftReg           -- no reset, pipeline only.
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
  def apply [T <: Chisel.Data](in: T, depth: Int, init: Int  = 0, name: Option[String] = None): T =
    AbstractPipelineReg(new AsyncResetShiftReg(in.getWidth, depth, init), in, name)

  def apply [T <: Chisel.Data](in: T, depth: Int, name: Option[String]): T =
    apply(in, depth, 0, name)

  def apply [T <: Chisel.Data](in: T, depth: Int, init: T, name: Option[String]): T =
    apply(in, depth, init.litValue.toInt, name)

  def apply [T <: Chisel.Data](in: T, depth: Int, init: T): T =
    apply (in, depth, init.litValue.toInt, None)
}

// Note that it is important to override "name" in order to ensure that the Chisel dedup does
// not try to merge instances of this with instances of the superclass.
class AsyncResetSynchronizerShiftReg(w: Int = 1, sync: Int = 3, init: Int = 0) extends AsyncResetShiftReg(w, depth = sync, init, name = "sync") {
  require(sync > 0, "Sync must be greater than 0.")
  override def desiredName = s"AsyncResetSynchronizerShiftReg_w${w}_d${sync}_i${init}"
}

object AsyncResetSynchronizerShiftReg {
  def apply [T <: Chisel.Data](in: T, depth: Int, init: Int  = 0, name: Option[String] = None): T =
    AbstractPipelineReg(new AsyncResetSynchronizerShiftReg(in.getWidth, depth, init), in, name)

  def apply [T <: Chisel.Data](in: T, depth: Int, name: Option[String]): T =
    apply(in, depth, 0, name)

  def apply [T <: Chisel.Data](in: T, depth: Int, init: T, name: Option[String]): T =
    apply(in, depth, init.litValue.toInt, name)

  def apply [T <: Chisel.Data](in: T, depth: Int, init: T): T =
    apply (in, depth, init.litValue.toInt, None)
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
  def apply [T <: Chisel.Data](in: T, sync: Int = 3, name: Option[String] = None): T = {
    if (sync == 0) in
    else AbstractPipelineReg(new SynchronizerShiftReg(in.getWidth, sync), in, name)
  }
}

class SyncResetSynchronizerShiftReg(w: Int = 1, sync: Int = 3, init: Int = 0) extends AbstractPipelineReg(w) {
  require (sync >= 0, "Sync must be greater than or equal to 0")

  override def desiredName = s"SyncResetSynchronizerShiftReg_w${w}_d${sync}_i${init}"

  io.q := ShiftRegInit(io.d, n = sync, init = init.U, name = Some("sync"))

}

object SyncResetSynchronizerShiftReg {
  def apply [T <: Chisel.Data](in: T, sync: Int = 3, init: T, name: Option[String] = None): T =
    AbstractPipelineReg(new SyncResetSynchronizerShiftReg(in.getWidth, sync, init.litValue.toInt), in, name)
}
