// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/** A queue that delays elements by a certain cycle count.
  *
  * @param gen              queue element type
  * @param entries          queue size
  *
  * @param enq              enqueue
  * @param deq              dequeue
  * @param timer            cycle count timer
  * @param entries          cycle delay
  */
class DelayQueue[T <: Data](gen: T, entries: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
    val timer = Input(UInt())
    val delay = Input(UInt())
  })

  val q = Module(new Queue(new Bundle {
    val data = gen
    val time = UInt(io.timer.getWidth.W)
  }, entries, flow=true))

  val delay_r = RegInit(0.U(io.delay.getWidth.W))
  when (delay_r =/= io.delay) {
    delay_r := io.delay
    assert(q.io.count == 0, "Undefined behavior when delay is changed while queue has elements.")
  }

  q.io.enq.bits.data := io.enq.bits
  q.io.enq.bits.time := io.timer
  q.io.enq.valid := io.enq.fire
  io.enq.ready := q.io.enq.ready

  io.deq.bits := q.io.deq.bits.data
  io.deq.valid := q.io.deq.valid && ((io.timer - q.io.deq.bits.time) >= delay_r)
  q.io.deq.ready := io.deq.fire
}

object DelayQueue {
  /** Helper to connect a delay queue.
   *
   * @param source           decoupled queue input
   * @param timer            cycle count timer
   * @param delay            cycle delay
   * @param depth            queue size
   */
  def apply[T <: Data](source: DecoupledIO[T], timer: UInt, delay: UInt, depth: Int): DecoupledIO[T] = {
    val delayQueue = Module(new DelayQueue(chiselTypeOf(source.bits), depth))
    delayQueue.io.enq <> source
    delayQueue.io.timer := timer
    delayQueue.io.delay := delay
    delayQueue.io.deq
  }

  /** Helper to connect a delay queue and instantiate a timer to keep track of cycle count.
   *
   * @param source           decoupled queue input
   * @param timerWidth       width of cycle count timer
   * @param delay            cycle delay
   * @param depth            queue size
   */
  def apply[T <: Data](source: DecoupledIO[T], timerWidth: Int, delay: UInt, depth: Int): DecoupledIO[T] = {
    val timer = RegInit(0.U(timerWidth.W))
    timer := timer + 1.U
    apply(source, timer, delay, depth)
  }

  /** Helper to connect a delay queue that delays elements by a statically defined cycle count.
   *
   * @param source           decoupled queue input
   * @param delay            static cycle delay
   */
  def apply[T <: Data](source: DecoupledIO[T], delay: Int): DecoupledIO[T] = {
    val mDelay = delay.max(1)
    apply(source, (1 + log2Ceil(mDelay)), delay.U, mDelay)
  }

  /** Helper to connect a delay queue that delays elements by a dynamically set cycle count.
   *
   * @param source           decoupled queue input
   * @param delay            cycle delay
   * @param maxDelay         maximum cycle delay
   */
  def apply[T <: Data](source: DecoupledIO[T], delay: UInt, maxDelay: Int = 4096): DecoupledIO[T] = {
    val mDelay = maxDelay.max(1)
    apply(source, (1 + log2Ceil(mDelay)), delay, mDelay)
  }
}
