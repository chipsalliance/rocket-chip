// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

class LatencyPipe[T <: Data](typ: T, latency: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Decoupled(typ)
  })

  def doN[T](n: Int, func: T => T, in: T): T =
    (0 until n).foldLeft(in)((last, _) => func(last))

  io.out <> doN(latency, (last: DecoupledIO[T]) => Queue(last, 1, true), io.in)
}

object LatencyPipe {
  def apply[T <: Data](in: DecoupledIO[T], latency: Int): DecoupledIO[T] = {
    val pipe = Module(new LatencyPipe(chiselTypeOf(in.bits), latency))
    pipe.io.in <> in
    pipe.io.out
  }
}
