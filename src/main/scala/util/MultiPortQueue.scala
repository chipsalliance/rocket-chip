// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

class MultiPortQueue[T <: Data](gen: T, val lanes: Int, val rows: Int, storage: LanePositionedQueue = FloppedLanePositionedQueue) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Vec(lanes, Decoupled(gen)))
    // NOTE: deq.{valid,bits} depend on deq.ready of lower-indexed ports
    val deq = Vec(lanes, Decoupled(gen))
  })

  val queue = Module(storage(gen, lanes, rows))

  MultiPortQueue.gather (io.enq, queue.io.enq, queue.io.enq_0_lane)
  MultiPortQueue.scatter(io.deq, queue.io.deq, queue.io.deq_0_lane)
}

object MultiPortQueue {
  def gather[T <: Data](sparse: Seq[DecoupledIO[T]], dense: LanePositionedDecoupledIO[T], offset: UInt = 0.U) {
    val lanes = dense.lanes
    val gen = chiselTypeOf(sparse.head.bits)
    require (lanes == sparse.size)

    // Compute per-enq-port ready
    val enq_valid = DensePrefixSum(sparse.map(_.valid.asUInt))(_ +& _)
    dense.valid := enq_valid.last
    sparse(0).ready := dense.ready.orR
    for (i <- 1 until lanes) {
      sparse(i).ready := enq_valid(i-1) < dense.ready
    }

    // Gather data from enq ports to rotated lanes
    val enq_1hot = UIntToOH1(offset, lanes).pad(lanes)
    val enq_sparse = Wire(Vec(2*lanes, ValidIO(gen)))
    for (i <- 0 until lanes) {
      enq_sparse(i).valid := enq_1hot(lanes-1-i)
      enq_sparse(i).bits  := 0.U.asTypeOf(gen)
      enq_sparse(i+lanes).valid := sparse(i).valid
      enq_sparse(i+lanes).bits  := sparse(i).bits
    }
    val enq_dense = Gather(enq_sparse)
    dense.bits := VecInit.tabulate(lanes) { i =>
      Mux(enq_1hot(i), enq_dense(i+lanes), enq_dense(i))
    }
  }

  def scatter[T <: Data](sparse: Seq[DecoupledIO[T]], dense: LanePositionedDecoupledIO[T], offset: UInt = 0.U) {
    val lanes = dense.lanes
    val gen = chiselTypeOf(sparse.head.bits)
    require (lanes == sparse.size)

    // Computer per-deq-port valid
    val deq_ready = DensePrefixSum(sparse.map(_.ready.asUInt))(_ +& _)
    dense.ready := deq_ready.last
    sparse(0).valid := dense.valid.orR
    for (i <- 1 until lanes) {
      sparse(i).valid := deq_ready(i-1) < dense.valid
    }

    // Scatter data from rotated lanes to deq ports
    val deq_1hot = UIntToOH1(offset, lanes).pad(lanes)
    val deq_dense = Wire(Vec(2*lanes, ValidIO(gen)))
    for (i <- 0 until lanes) {
      deq_dense(i).valid := deq_1hot(i)
      deq_dense(i+lanes).valid := sparse(i).ready
      deq_dense(i).bits := dense.bits(i)
      deq_dense(i+lanes).bits := dense.bits(i)
    }
    val deq_sparse = Scatter(deq_dense)
    for (i <- 0 until lanes) {
      sparse(i).bits := deq_sparse(i+lanes)
    }
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
