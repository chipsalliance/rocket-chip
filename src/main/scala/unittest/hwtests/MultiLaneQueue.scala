// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest.hwtests

import chisel3._
import chisel3.util.log2Ceil

import org.chipsalliance.rocketutils.MultiLaneQueue

import freechips.rocketchip.unittest.UnitTest
import freechips.rocketchip.tilelink.LFSR64

class MultiLaneQueueTest(lanes: Int, rows: Int, cycles: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val ids = (cycles+1) * lanes
  val bits = log2Ceil(ids+1)

  val q = Module(new MultiLaneQueue(UInt(bits.W), lanes, rows))

  val enq = RegInit(0.U(bits.W))
  val deq = RegInit(0.U(bits.W))
  val done = RegInit(false.B)

  when (enq >= (cycles*lanes).U) { done := true.B }
  io.finished := done

  q.io.enq_valid := (LFSR64() * (1+lanes).U) >> 64
  q.io.deq_ready := (LFSR64() * (1+lanes).U) >> 64

  when (q.io.enq_ready) { enq := enq + q.io.enq_valid }
  when (q.io.deq_valid) { deq := deq + q.io.deq_ready }

  for (i <- 0 until lanes) {
    q.io.enq_bits(i) := enq + i.U
    when (q.io.deq_valid) {
      assert (i.U >= q.io.deq_ready || q.io.deq_bits(i) === deq + i.U)
    }
  }
}
