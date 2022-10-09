// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/** Takes in data on one decoupled interface and broadcasts it
  * to N decoupled output interfaces.
  */
class Broadcaster[T <: Data](typ: T, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Vec(n, Decoupled(typ))
  })

  require (n > 0)

  if (n == 1) {
    io.out.head <> io.in
  } else {
    val idx = RegInit(0.U(log2Up(n).W))
    val save = Reg(typ)

    io.out.head.valid := idx === 0.U && io.in.valid
    io.out.head.bits := io.in.bits
    for (i <- 1 until n) {
      io.out(i).valid := idx === i.U
      io.out(i).bits := save
    }
    io.in.ready := io.out.head.ready && idx === 0.U

    when (io.in.fire) { save := io.in.bits }

    when (io.out(idx).fire) {
      when (idx === (n - 1).U) { idx := 0.U }
      .otherwise { idx := idx + 1.U }
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
