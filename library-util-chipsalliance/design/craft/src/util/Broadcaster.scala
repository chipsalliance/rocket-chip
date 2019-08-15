// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._

/** Takes in data on one decoupled interface and broadcasts it
  * to N decoupled output interfaces.
  */
class Broadcaster[T <: Data](typ: T, n: Int) extends Module {
  val io = new Bundle {
    val in = Decoupled(typ).flip
    val out = Vec(n, Decoupled(typ))
  }

  require (n > 0)

  if (n == 1) {
    io.out.head <> io.in
  } else {
    val idx = Reg(init = UInt(0, log2Up(n)))
    val save = Reg(typ)

    io.out.head.valid := idx === UInt(0) && io.in.valid
    io.out.head.bits := io.in.bits
    for (i <- 1 until n) {
      io.out(i).valid := idx === UInt(i)
      io.out(i).bits := save
    }
    io.in.ready := io.out.head.ready && idx === UInt(0)

    when (io.in.fire()) { save := io.in.bits }

    when (io.out(idx).fire()) {
      when (idx === UInt(n - 1)) { idx := UInt(0) }
      .otherwise { idx := idx + UInt(1) }
    }
  }
}

object Broadcaster {
  def apply[T <: Data](in: DecoupledIO[T], n: Int): Vec[DecoupledIO[T]] = {
    val split = Module(new Broadcaster(in.bits, n))
    split.io.in <> in
    split.io.out
  }
}
