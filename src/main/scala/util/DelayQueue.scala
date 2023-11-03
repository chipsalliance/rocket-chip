// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

object DelayQueue {
  def apply[T <: Data](sink: DecoupledIO[T], source: DecoupledIO[T], timer: UInt, delay: UInt, depth: Int): Unit = {
      val q = Module(new Queue(new Bundle {
        val data = source.bits.cloneType
        val time = UInt(timer.getWidth.W)
      }, depth, flow=true))

      q.io.enq.bits.data := source.bits
      q.io.enq.bits.time := timer
      q.io.enq.valid := source.fire
      source.ready := q.io.enq.ready

      sink.bits := q.io.deq.bits.data
      sink.valid := q.io.deq.valid && ( (timer - q.io.deq.bits.time) >= delay)
      q.io.deq.ready := sink.fire
  }
}

object FixedDelayQueue {
  def apply[T <: Data](sink: DecoupledIO[T], source: DecoupledIO[T], delay: Int, maxLatency: Int = 4096): Unit = {
    val timer = RegInit(0.U((log2Ceil(delay.max(1)) + log2Ceil(maxLatency)).W))
    timer := timer + 1.U
    DelayQueue(sink, source, timer, delay.U, delay.max(1))
  }
}

object VariableDelayQueue {
  def apply[T <: Data](sink: DecoupledIO[T], source: DecoupledIO[T], delay: UInt, depth: Int = 4096, maxLatency: Int = 4096): Unit = {
    val timer = RegInit(0.U((1 + log2Ceil(maxLatency)).W))
    timer := timer + 1.U
    DelayQueue(sink, source, timer, delay, depth)
  }
}
