// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

class MultiPortQueue[T <: Data](gen: T, val lanes: Int, val rows: Int, storage: LanePositionedQueue = FloppedLanePositionedQueue) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Vec(lanes, Decoupled(gen)))
    // NOTE: deq.{valid,bits} depend on deq.ready; if this is a problem, add a flow queue.
    val deq = Vec(lanes, Decoupled(gen))
  })

  val queue = Module(storage(gen, lanes, rows))

  // Compute per-enq-port ready
  val enq_valid = DensePrefixSum(io.enq.map(_.valid.asUInt))(_ +& _)
  queue.io.enq.valid := enq_valid.last
  for (i <- 0 until lanes) {
    io.enq(i).ready := enq_valid(i) <= queue.io.enq.ready
  }

  // Computer per-deq-port valid
  val deq_ready = DensePrefixSum(io.deq.map(_.ready.asUInt))(_ +& _)
  queue.io.deq.ready := deq_ready.last
  for (i <- 0 until lanes) {
    io.deq(i).valid := deq_ready(i) <= queue.io.deq.valid
  }

  // Gather data from enq ports to rotated lanes
  val enq_1hot = UIntToOH1(queue.io.enq_0_lane, lanes).pad(lanes)
  val enq_sparse = Wire(Vec(2*lanes, ValidIO(gen)))
  for (i <- 0 until lanes) {
    enq_sparse(i).valid := enq_1hot(lanes-1-i)
    enq_sparse(i).bits  := 0.U.asTypeOf(gen)
    enq_sparse(i+lanes).valid := io.enq(i).valid
    enq_sparse(i+lanes).bits  := io.enq(i).bits
  }
  val enq_dense = Gather(enq_sparse)
  queue.io.enq.bits := VecInit.tabulate(lanes) { i =>
    Mux(enq_1hot(i), enq_dense(i+lanes), enq_dense(i))
  }

  // Scatter data from rotated lanes to deq ports
  val deq_1hot = UIntToOH1(queue.io.deq_0_lane, lanes).pad(lanes)
  val deq_dense = Wire(Vec(2*lanes, ValidIO(gen)))
  for (i <- 0 until lanes) {
    deq_dense(i).valid := deq_1hot(i)
    deq_dense(i+lanes).valid := io.deq(i).ready
    deq_dense(i).bits := queue.io.deq.bits(i)
    deq_dense(i+lanes).bits := queue.io.deq.bits(i)
  }
  val deq_sparse = Scatter(deq_dense)
  for (i <- 0 until lanes) {
    io.deq(i).bits := deq_sparse(i+lanes)
  }
}

import freechips.rocketchip.unittest._
import freechips.rocketchip.tilelink.LFSR64

class MultiPortQueueTest(lanes: Int, rows: Int, cycles: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val ids = (cycles+1) * lanes
  val bits = log2Ceil(ids+1)

  val q = Module(new MultiPortQueue(UInt(bits.W), lanes, rows))

  val enq = RegInit(0.U(bits.W))
  val deq = RegInit(0.U(bits.W))
  val done = RegInit(false.B)

  when (enq >= (cycles*lanes).U) { done := true.B }
  io.finished := done

  val valid = LFSR64()(lanes-1, 0)
  val ready = LFSR64()(lanes-1, 0)

  enq := enq + PopCount(q.io.enq.map(_.fire()))
  deq := deq + PopCount(q.io.deq.map(_.fire()))

  val enq_bits = RipplePrefixSum(enq +: valid.toBools.map(x => WireInit(UInt(bits.W), x)))(_ + _)
  val deq_bits = RipplePrefixSum(deq +: ready.toBools.map(x => WireInit(UInt(bits.W), x)))(_ + _)

  for (i <- 0 until lanes) {
    q.io.enq(i).valid := valid(i)
    q.io.enq(i).bits  := Mux(valid(i), enq_bits(i), 0.U)
    q.io.deq(i).ready := ready(i)
    assert (!q.io.deq(i).fire() || q.io.deq(i).bits === deq_bits(i))
  }
}
