// See LICENSE.Berkeley for license details.

package freechips.rocketchip.unittest.hwtests

import chisel3._
import chisel3.util.{Cat, Counter}

import org.chipsalliance.rocketutils.MultiWidthFifo

import freechips.rocketchip.unittest.UnitTest

class MultiWidthFifoTest extends UnitTest {
  val big2little = Module(new MultiWidthFifo(16, 8, 8))
  val little2big = Module(new MultiWidthFifo(8, 16, 4))

  val bl_send = RegInit(false.B)
  val lb_send = RegInit(false.B)
  val bl_recv = RegInit(false.B)
  val lb_recv = RegInit(false.B)
  val bl_finished = RegInit(false.B)
  val lb_finished = RegInit(false.B)

  val bl_data = VecInit(Seq.tabulate(4){i => ((2 * i + 1) * 256 + 2 * i).U(16.W)})
  val lb_data = VecInit(Seq.tabulate(8){i => i.U(8.W)})

  val (bl_send_cnt, bl_send_done) = Counter(big2little.io.in.fire, 4)
  val (lb_send_cnt, lb_send_done) = Counter(little2big.io.in.fire, 8)

  val (bl_recv_cnt, bl_recv_done) = Counter(big2little.io.out.fire, 8)
  val (lb_recv_cnt, lb_recv_done) = Counter(little2big.io.out.fire, 4)

  big2little.io.in.valid := bl_send
  big2little.io.in.bits := bl_data(bl_send_cnt)
  big2little.io.out.ready := bl_recv

  little2big.io.in.valid := lb_send
  little2big.io.in.bits := lb_data(lb_send_cnt)
  little2big.io.out.ready := lb_recv

  val bl_recv_data_idx = bl_recv_cnt >> 1.U
  val bl_recv_data = Mux(bl_recv_cnt(0),
    bl_data(bl_recv_data_idx)(15, 8),
    bl_data(bl_recv_data_idx)(7, 0))

  val lb_recv_data = Cat(
    lb_data(Cat(lb_recv_cnt, 1.U(1.W))),
    lb_data(Cat(lb_recv_cnt, 0.U(1.W))))

  when (io.start) {
    bl_send := true.B
    lb_send := true.B
  }

  when (bl_send_done) {
    bl_send := false.B
    bl_recv := true.B
  }

  when (lb_send_done) {
    lb_send := false.B
    lb_recv := true.B
  }

  when (bl_recv_done) {
    bl_recv := false.B
    bl_finished := true.B
  }

  when (lb_recv_done) {
    lb_recv := false.B
    lb_finished := true.B
  }

  io.finished := bl_finished && lb_finished

  val bl_start_recv = RegNext(bl_send_done)
  val lb_start_recv = RegNext(lb_send_done)

  assert(!little2big.io.out.valid || little2big.io.out.bits === lb_recv_data,
    "Little to Big data mismatch")
  assert(!big2little.io.out.valid || big2little.io.out.bits === bl_recv_data,
    "Bit to Little data mismatch")

  assert(!lb_start_recv || little2big.io.count === 4.U,
    "Little to Big count incorrect")
  assert(!bl_start_recv || big2little.io.count === 8.U,
    "Big to Little count incorrect")
}


