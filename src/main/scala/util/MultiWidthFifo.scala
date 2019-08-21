// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._
import freechips.rocketchip.unittest.UnitTest

class MultiWidthFifo(inW: Int, outW: Int, n: Int) extends Module {
  val io = new Bundle {
    val in = Decoupled(Bits(width = inW)).flip
    val out = Decoupled(Bits(width = outW))
    val count = UInt(OUTPUT, log2Up(n + 1))
  }

  if (inW == outW) {
    val q = Module(new Queue(Bits(width = inW), n))
    q.io.enq <> io.in
    io.out <> q.io.deq
    io.count := q.io.count
  } else if (inW > outW) {
    val nBeats = inW / outW

    require(inW % outW == 0, s"MultiWidthFifo: in: $inW not divisible by out: $outW")
    require(n % nBeats == 0, s"Cannot store $n output words when output beats is $nBeats")

    val wdata = Reg(Vec(n / nBeats, Bits(width = inW)))
    val rdata = Vec(wdata.flatMap { indat =>
      (0 until nBeats).map(i => indat(outW * (i + 1) - 1, outW * i)) })

    val head = Reg(init = UInt(0, log2Up(n / nBeats)))
    val tail = Reg(init = UInt(0, log2Up(n)))
    val size = Reg(init = UInt(0, log2Up(n + 1)))

    when (io.in.fire()) {
      wdata(head) := io.in.bits
      head := head + UInt(1)
    }

    when (io.out.fire()) { tail := tail + UInt(1) }

    size := MuxCase(size, Seq(
      (io.in.fire() && io.out.fire()) -> (size + UInt(nBeats - 1)),
      io.in.fire() -> (size + UInt(nBeats)),
      io.out.fire() -> (size - UInt(1))))

    io.out.valid := size > UInt(0)
    io.out.bits := rdata(tail)
    io.in.ready := size < UInt(n)
    io.count := size
  } else {
    val nBeats = outW / inW

    require(outW % inW == 0, s"MultiWidthFifo: out: $outW not divisible by in: $inW")

    val wdata = Reg(Vec(n * nBeats, Bits(width = inW)))
    val rdata = Vec.tabulate(n) { i =>
      Cat(wdata.slice(i * nBeats, (i + 1) * nBeats).reverse)}

    val head = Reg(init = UInt(0, log2Up(n * nBeats)))
    val tail = Reg(init = UInt(0, log2Up(n)))
    val size = Reg(init = UInt(0, log2Up(n * nBeats + 1)))

    when (io.in.fire()) {
      wdata(head) := io.in.bits
      head := head + UInt(1)
    }

    when (io.out.fire()) { tail := tail + UInt(1) }

    size := MuxCase(size, Seq(
      (io.in.fire() && io.out.fire()) -> (size - UInt(nBeats - 1)),
      io.in.fire() -> (size + UInt(1)),
      io.out.fire() -> (size - UInt(nBeats))))

    io.count := size >> UInt(log2Up(nBeats))
    io.out.valid := io.count > UInt(0)
    io.out.bits := rdata(tail)
    io.in.ready := size < UInt(n * nBeats)
  }
}

class MultiWidthFifoTest extends UnitTest {
  val big2little = Module(new MultiWidthFifo(16, 8, 8))
  val little2big = Module(new MultiWidthFifo(8, 16, 4))

  val bl_send = Reg(init = Bool(false))
  val lb_send = Reg(init = Bool(false))
  val bl_recv = Reg(init = Bool(false))
  val lb_recv = Reg(init = Bool(false))
  val bl_finished = Reg(init = Bool(false))
  val lb_finished = Reg(init = Bool(false))

  val bl_data = Vec.tabulate(4){i => UInt((2 * i + 1) * 256 + 2 * i, 16)}
  val lb_data = Vec.tabulate(8){i => UInt(i, 8)}

  val (bl_send_cnt, bl_send_done) = Counter(big2little.io.in.fire(), 4)
  val (lb_send_cnt, lb_send_done) = Counter(little2big.io.in.fire(), 8)

  val (bl_recv_cnt, bl_recv_done) = Counter(big2little.io.out.fire(), 8)
  val (lb_recv_cnt, lb_recv_done) = Counter(little2big.io.out.fire(), 4)

  big2little.io.in.valid := bl_send
  big2little.io.in.bits := bl_data(bl_send_cnt)
  big2little.io.out.ready := bl_recv

  little2big.io.in.valid := lb_send
  little2big.io.in.bits := lb_data(lb_send_cnt)
  little2big.io.out.ready := lb_recv

  val bl_recv_data_idx = bl_recv_cnt >> UInt(1)
  val bl_recv_data = Mux(bl_recv_cnt(0),
    bl_data(bl_recv_data_idx)(15, 8),
    bl_data(bl_recv_data_idx)(7, 0))

  val lb_recv_data = Cat(
    lb_data(Cat(lb_recv_cnt, UInt(1, 1))),
    lb_data(Cat(lb_recv_cnt, UInt(0, 1))))

  when (io.start) {
    bl_send := Bool(true)
    lb_send := Bool(true)
  }

  when (bl_send_done) {
    bl_send := Bool(false)
    bl_recv := Bool(true)
  }

  when (lb_send_done) {
    lb_send := Bool(false)
    lb_recv := Bool(true)
  }

  when (bl_recv_done) {
    bl_recv := Bool(false)
    bl_finished := Bool(true)
  }

  when (lb_recv_done) {
    lb_recv := Bool(false)
    lb_finished := Bool(true)
  }

  io.finished := bl_finished && lb_finished

  val bl_start_recv = Reg(next = bl_send_done)
  val lb_start_recv = Reg(next = lb_send_done)

  assert(!little2big.io.out.valid || little2big.io.out.bits === lb_recv_data,
    "Little to Big data mismatch")
  assert(!big2little.io.out.valid || big2little.io.out.bits === bl_recv_data,
    "Bit to Little data mismatch")

  assert(!lb_start_recv || little2big.io.count === UInt(4),
    "Little to Big count incorrect")
  assert(!bl_start_recv || big2little.io.count === UInt(8),
    "Big to Little count incorrect")
}


