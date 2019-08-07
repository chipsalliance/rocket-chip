// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/////////////////////////////// Black Box Unit Testing /////////////////////////////

import freechips.rocketchip.unittest._
import freechips.rocketchip.tilelink.LFSR64

class PositionedQueueTest(queueFactory: LanePositionedQueue, lanes: Int, rows: Int, cycles: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val ids = (cycles+1) * lanes
  val bits = log2Ceil(ids+1)

  val q = Module(queueFactory(UInt(bits.W), lanes, rows, true, false))

  val enq = RegInit(0.U(bits.W))
  val deq = RegInit(0.U(bits.W))
  val done = RegInit(false.B)

  def cap(x: UInt) = Mux(x > lanes.U, lanes.U, x) +& 1.U
  q.io.enq.valid := (LFSR64() * cap(q.io.enq.ready)) >> 64
  q.io.deq.ready := (LFSR64() * cap(q.io.deq.valid)) >> 64

  enq := enq + q.io.enq.valid
  deq := deq + q.io.deq.ready

  when (enq >= (cycles*lanes).U) { done := true.B }
  io.finished := done

  q.io.enq.bits := VecInit.tabulate(lanes) { i =>
    val pos = Mux(i.U >= q.io.enq_0_lane, i.U, (i + lanes).U) - q.io.enq_0_lane
    Mux (pos >= q.io.enq.valid, 0.U, enq + pos)
  }

  q.io.deq.bits.zipWithIndex.foreach { case (d, i) =>
    val pos = Mux(i.U >= q.io.deq_0_lane, i.U, (i + lanes).U) - q.io.deq_0_lane
    assert (pos >= q.io.deq.valid || d === deq + pos)
  }
}
