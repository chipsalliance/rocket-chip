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
  *  SynchronizerShiftRegInit -- synchronously reset to 0
  *  SynchronizerShiftReg -- no reset, pipeline only.
  *  
  */

class AsyncResetSynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends Module {

  require(sync > 0, "Sync must be greater than 0.")

  override def desiredName = s"AsyncResetSynchronizerShiftReg_w${w}_d${sync}"

  val io = new Bundle {
    val d = UInt(INPUT, width = w)
    val q = UInt(OUTPUT, width = w)
  }

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

  def apply [T <: Chisel.Data](in: T, sync: Int = 3, name: Option[String] = None): T = {
    val sync_reg = Module(new AsyncResetSynchronizerShiftReg(in.getWidth, sync))
    name.foreach{ sync_reg.suggestName(_) }

    sync_reg.io.d := in.asUInt
    (in.chiselCloneType).fromBits(sync_reg.io.q)
  }

}

class SynchronizerShiftRegInit(w: Int = 1, sync: Int = 3) extends Module {

  require(sync > 0, "Sync must be greater than 0.")

  override def desiredName = s"SynchronizerShiftRegInit_w${w}_d${sync}"

  val io = new Bundle {
    val d = UInt(INPUT, width = w)
    val q = UInt(OUTPUT, width = w)
  }

  val syncv = List.tabulate(sync) { i =>
    val r = RegInit(UInt(0, width = w))
    r.suggestName(s"sync_${i}")
  }

  syncv.last := io.d

  (syncv.init zip syncv.tail).foreach { case (sink, source) =>
    sink := source
  }

  io.q := syncv.head

}

object SynchronizerShiftRegInit {

  def apply [T <: Chisel.Data](in: T, sync: Int = 3, name: Option[String] = None): T = {
    val sync_reg = Module(new SynchronizerShiftRegInit(in.getWidth, sync))
    name.foreach{ sync_reg.suggestName(_) }

    sync_reg.io.d := in.asUInt
    (in.chiselCloneType).fromBits(sync_reg.io.q)
  }
}


class SynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends Module {

  require(sync > 0, "Sync must be greater than 0.")

  override def desiredName = s"SynchronizerShiftReg_w${w}_d${sync}"

  val io = new Bundle {
    val d = UInt(INPUT, width = w)
    val q = UInt(OUTPUT, width = w)
  }

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
    val sync_reg = Module(new SynchronizerShiftReg(in.getWidth, sync))
    name.foreach{ sync_reg.suggestName(_) }

    sync_reg.io.d := in.asUInt
    (in.chiselCloneType).fromBits(sync_reg.io.q)
  }
}
