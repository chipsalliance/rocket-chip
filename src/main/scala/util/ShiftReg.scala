// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._

// Similar to the Chisel ShiftRegister but allows the user to suggest a
// name to the registers that get instantiated, and
// to provide a reset value.
object ShiftRegInit {
  def apply[T <: Data](in: T, n: Int, init: T, name: Option[String] = None): T =

  (0 until n).foldRight(in) {
    case (i, next) => {
      val r = RegNext(next, init)
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
  *  AsyncResetShiftReg             -- Asynchronously reset register array
  *                                    A W(width) x D(depth) sized array is constructed from D instantiations of a
  *                                    W-wide register vector. Functionally identical to AsyncResetSyncrhonizerShiftReg,
  *                                    but only used for timing applications
  */

abstract class AbstractPipelineReg(w: Int = 1) extends Module {
  val io = IO(new Bundle {
    val d = Input(UInt(w.W))
    val q = Output(UInt(w.W))
  }
  )
}

object AbstractPipelineReg {
  def apply [T <: Data](gen: => AbstractPipelineReg, in: T, name: Option[String] = None): T = {
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
  chain.last.io.en := true.B

  (chain.init zip chain.tail).foreach { case (sink, source) =>
    sink.io.d := source.io.q
    sink.io.en := true.B
  }
  io.q := chain.head.io.q
}

object AsyncResetShiftReg {
  def apply [T <: Data](in: T, depth: Int, init: Int  = 0, name: Option[String] = None): T =
    AbstractPipelineReg(new AsyncResetShiftReg(in.getWidth, depth, init), in, name)

  def apply [T <: Data](in: T, depth: Int, name: Option[String]): T =
    apply(in, depth, 0, name)

  def apply [T <: Data](in: T, depth: Int, init: T, name: Option[String]): T =
    apply(in, depth, init.litValue.toInt, name)

  def apply [T <: Data](in: T, depth: Int, init: T): T =
    apply (in, depth, init.litValue.toInt, None)
}
