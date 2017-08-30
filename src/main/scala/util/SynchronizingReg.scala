// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

/** These wrap behavioral 
  *  shift registers  into specific
  *  modules to allow for 
  *  backend flows to replace or constrain
  *  them properly when used for CDC synchronization,
  *  rather than buffering.
  *  
  *  The 3 different types vary in their reset behavior:
  *  AsyncResetSynchronizerShiftReg -- asynchronously reset to 0
  *  SynchronizerShiftReg -- no reset, pipeline only.
  *  
  */

abstract class AbstractSynchronizerReg(w: Int = 1, sync: Int = 3) extends Module {
  require(sync > 0, "Sync must be greater than 0.")

  val io = new Bundle {
    val d = UInt(INPUT, width = w)
    val q = UInt(OUTPUT, width = w)
  }

}

object AbstractSynchronizerReg {

  def apply [T <: Chisel.Data](gen: => AbstractSynchronizerReg, in: T, sync: Int = 3, name: Option[String] = None): T = {
    val sync_chain = Module(gen)
    name.foreach{ sync_chain.suggestName(_) }
    sync_chain.io.d := in.asUInt

    sync_chain.io.q.asTypeOf(in)
  }
}

class AsyncResetSynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends AbstractSynchronizerReg(w, sync) {

  override def desiredName = s"AsyncResetSynchronizerShiftReg_w${w}_d${sync}"

  val syncv = List.tabulate(sync) { i =>
    Module (new AsyncResetRegVec(w, 0)).suggestName(s"sync_${i}")
  }

  syncv.last.io.d := io.d
  syncv.last.io.en := Bool(true)

  (syncv.init zip syncv.tail).foreach { case (sink, source) =>
    sink.io.d := source.io.q
    sink.io.en := Bool(true)
  }
  io.q := syncv.head.io.q
}

object AsyncResetSynchronizerShiftReg {

  def apply [T <: Chisel.Data](in: T, sync: Int = 3, name: Option[String] = None): T =
    AbstractSynchronizerReg(gen = {new AsyncResetSynchronizerShiftReg(in.getWidth, sync)},
      in, sync, name)
}

class SynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends AbstractSynchronizerReg(w, sync) {

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
    AbstractSynchronizerReg(gen = { new SynchronizerShiftReg(in.getWidth, sync)},
      in, sync, name)
}
