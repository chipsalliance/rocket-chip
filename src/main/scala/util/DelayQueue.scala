// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

object DelayQueue {
  private def apply[T <: Data](source: DecoupledIO[T], timer: UInt, delay: UInt, depth: Int): DecoupledIO[T] = {
    val q = Module(new Queue(new Bundle {
      val data = source.bits.cloneType
      val time = UInt(timer.getWidth.W)
    }, depth, flow=true))

    val delay_r = RegInit((0.U)delay.getWidth.W)
    when (delay_r =/= delay) {
      delay_r := delay
      assert(q.io.count == 0, "Cannot change delay while queue has elements.")
    }

    q.io.enq.bits.data := source.bits
    q.io.enq.bits.time := timer
    q.io.enq.valid := source.fire
    source.ready := q.io.enq.ready

    val sink = Wire(new DecoupledIO(source.bits.cloneType))
    sink.bits := q.io.deq.bits.data
    sink.valid := q.io.deq.valid && ((timer - q.io.deq.bits.time) >= delay_r)
    q.io.deq.ready := sink.fire

    sink
  }

  def apply[T <: Data](source: DecoupledIO[T], delay: Int, maxLatency: Int = 4096): DecoupledIO[T] = {
    val timer = RegInit(0.U((log2Ceil(delay.max(1)) + log2Ceil(maxLatency)).W))
    timer := timer + 1.U
    apply(sink, source, timer, delay.U, delay.max(1))
  }

  def apply[T <: Data](source: DecoupledIO[T], delay: UInt, depth: Int = 4096, maxLatency: Int = 4096): DecoupledIO[T] = {
    val timer = RegInit(0.U((1 + log2Ceil(maxLatency)).W))
    timer := timer + 1.U
    apply(sink, source, timer, delay, depth)
  }
}
