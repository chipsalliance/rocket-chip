// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

class MultiPortQueue[T <: Data](gen: T, val enq_lanes: Int, val deq_lanes: Int, val lanes: Int, val rows: Int, val flow: Boolean = false, storage: LanePositionedQueue = FloppedLanePositionedQueue) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Vec(enq_lanes, Decoupled(gen)))
    // NOTE: deq.{valid,bits} depend on deq.ready of lower-indexed ports
    val deq = Vec(deq_lanes, Decoupled(gen))
  })

  val queue = Module(storage(gen, lanes, rows, flow))

  MultiPortQueue.gather (io.enq, queue.io.enq, queue.io.enq_0_lane)
  MultiPortQueue.scatter(io.deq, queue.io.deq, queue.io.deq_0_lane)
}

object MultiPortQueue {
  def gather[T <: Data](sparse: Seq[DecoupledIO[T]], dense: LanePositionedDecoupledIO[T], offset: UInt = 0.U) {
    // Compute per-enq-port ready
    val enq_valid = DensePrefixSum(sparse.map(_.valid.asUInt))(_ +& _)
    val low_ready = if (dense.lanes == 1) 0.U else dense.ready(log2Ceil(dense.lanes)-1, 0)
    val cap_ready = Mux(dense.ready >= dense.lanes.U, dense.lanes.U, low_ready)
    val ready = if (dense.lanes >= sparse.size) dense.ready else cap_ready
    dense.valid := Mux(enq_valid.last <= ready, enq_valid.last, ready)
    (sparse zip (0.U +: enq_valid)) foreach { case (s, v) => s.ready := v < ready }

    // Gather data from enq ports to rotated lanes
    val popBits = log2Ceil(dense.lanes + sparse.size)
    val lowHoles = dense.lanes.U(popBits.W) - offset
    val highHoles = sparse.map(x => WireInit(UInt(popBits.W), !x.valid))
    val enq_dense = Gather(
      Seq.fill(dense.lanes) { 0.U.asTypeOf(chiselTypeOf(sparse.head.bits)) } ++ sparse.map(_.bits),
      Seq.fill(dense.lanes) { lowHoles } ++ DensePrefixSum(lowHoles +: highHoles)(_ + _).tail)

    val enq_1hot = UIntToOH1(offset, dense.lanes).pad(dense.lanes)
    dense.bits.zipWithIndex.foreach { case (bits, i) =>
      if (i < sparse.size) {
        bits := Mux(enq_1hot(i), enq_dense(i+dense.lanes), enq_dense(i))
      } else {
        bits := enq_dense(i)
      }
    }
  }

  def scatter[T <: Data](sparse: Seq[DecoupledIO[T]], dense: LanePositionedDecoupledIO[T], offset: UInt = 0.U) {
    // Computer per-deq-port valid
    val deq_ready = DensePrefixSum(sparse.map(_.ready.asUInt))(_ +& _)
    val low_valid = if (dense.lanes == 1) 0.U else dense.valid(log2Ceil(dense.lanes)-1, 0)
    val cap_valid = Mux(dense.valid >= dense.lanes.U, dense.lanes.U, low_valid)
    val valid = if (dense.lanes >= sparse.size) dense.valid else cap_valid
    dense.ready := Mux(deq_ready.last <= valid, deq_ready.last, valid)
    (sparse zip (0.U +: deq_ready)) foreach { case (s, r) => s.valid := r < valid }

    // Scatter data from rotated lanes to deq ports
    val bits = dense.bits ++ dense.bits ++ Seq.fill(sparse.size) { 0.U.asTypeOf(chiselTypeOf(sparse.head.bits)) }
    val popBits = log2Ceil(dense.lanes + sparse.size)
    val lowHoles = dense.lanes.U(popBits.W) - offset
    val highHoles = sparse.map(x => WireInit(UInt(popBits.W), !x.ready))
    val deq_sparse = Scatter(
      bits.take(dense.lanes + sparse.size),
      Seq.fill(dense.lanes) { lowHoles } ++ DensePrefixSum(lowHoles +: highHoles)(_ + _).tail)

    sparse.zipWithIndex.foreach { case (s, i) => s.bits := deq_sparse(i+dense.lanes) }
  }
}

import freechips.rocketchip.unittest._
import freechips.rocketchip.tilelink.LFSR64

class MultiPortQueueTest(lanes: Int, wlanes: Int, rows: Int, cycles: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val ids = (cycles+1) * lanes
  val bits = log2Ceil(ids+1)

  val q = Module(new MultiPortQueue(UInt(bits.W), lanes, lanes, wlanes, rows))

  val enq = RegInit(0.U(bits.W))
  val deq = RegInit(0.U(bits.W))
  val done = RegInit(false.B)

  when (enq >= (cycles*lanes).U) { done := true.B }
  io.finished := done

  val valid = LFSR64()(lanes-1, 0)
  val ready = LFSR64()(lanes-1, 0)

  enq := enq + PopCount(q.io.enq.map(_.fire()))
  deq := deq + PopCount(q.io.deq.map(_.fire()))

  val enq_bits = RipplePrefixSum(enq +: valid.asBools.map(x => WireInit(UInt(bits.W), x)))(_ + _)
  val deq_bits = RipplePrefixSum(deq +: ready.asBools.map(x => WireInit(UInt(bits.W), x)))(_ + _)

  for (i <- 0 until lanes) {
    q.io.enq(i).valid := valid(i)
    q.io.enq(i).bits  := Mux(valid(i), enq_bits(i), 0.U)
    q.io.deq(i).ready := ready(i)
    assert (!q.io.deq(i).fire() || q.io.deq(i).bits === deq_bits(i))
  }
}
