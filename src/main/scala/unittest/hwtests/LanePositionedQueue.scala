// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest.hwtests

import chisel3._
import chisel3.util.log2Ceil

import org.chipsalliance.rocketutils.LanePositionedQueue

import freechips.rocketchip.unittest.UnitTest
import freechips.rocketchip.tilelink.LFSR64

class PositionedQueueTest(queueFactory: LanePositionedQueue, lanes: Int, rows: Int, rewind: Boolean, abort: Boolean, cycles: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val ids = (cycles+1) * lanes
  val bits = log2Ceil(ids+1)

  val q = Module(queueFactory(UInt(bits.W), lanes, rows, true, false, rewind, abort, rewind, abort))

  val enq = RegInit(0.U(bits.W))
  val deq = RegInit(0.U(bits.W))
  val com = RegInit(0.U(bits.W))
  val abt = RegInit(0.U(bits.W))
  val done = RegInit(false.B)

  def cap(x: UInt) = Mux(x > lanes.U, lanes.U, x) +& 1.U
  q.io.enq.valid := (LFSR64() * cap(q.io.enq.ready)) >> 64
  q.io.deq.ready := (LFSR64() * cap(q.io.deq.valid)) >> 64

  enq := enq + q.io.enq.valid
  deq := deq + q.io.deq.ready

  q.io.commit.foreach { c =>
    val legal = enq + q.io.enq.valid - com
    assert (c.ready || c.bits > legal)
    c.valid := LFSR64()(0)
    c.bits  := ((legal + 1.U) * LFSR64()) >> 63 // 50% likely to be legal
    when (c.fire) { com := com + c.bits }
  }

  q.io.free.foreach { f =>
    val legal = deq + q.io.deq.ready - abt
    assert (f.ready || f.bits > legal)
    f.valid := LFSR64()(0)
    f.bits  := ((legal + 1.U) * LFSR64()) >> 63
    when (f.fire) { abt := abt + f.bits }
  }

  q.io.rewind.foreach { r =>
    val f = q.io.free.get
    r := (LFSR64() & 0xf.U) === 0.U
    when (r) { deq := Mux(f.fire, abt + f.bits, abt) }
  }

  q.io.abort .foreach { a =>
    val c = q.io.commit.get
    a := (LFSR64() & 0xf.U) === 0.U
    when (a) { enq := Mux(c.fire, com + c.bits, com) }
  }

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
