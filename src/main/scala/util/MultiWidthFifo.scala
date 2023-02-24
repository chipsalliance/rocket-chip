// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest

class MultiWidthFifo(inW: Int, outW: Int, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Bits(inW.W)))
    val out = Decoupled(Bits(outW.W))
    val count = Output(UInt(log2Up(n + 1).W))
  })

  if (inW == outW) {
    val q = Module(new Queue(Bits(inW.W), n))
    q.io.enq <> io.in
    io.out <> q.io.deq
    io.count := q.io.count
  } else if (inW > outW) {
    val nBeats = inW / outW

    require(inW % outW == 0, s"MultiWidthFifo: in: $inW not divisible by out: $outW")
    require(n % nBeats == 0, s"Cannot store $n output words when output beats is $nBeats")

    val wdata = Reg(Vec(n / nBeats, Bits(inW.W)))
    val rdata = VecInit(wdata.flatMap { indat =>
      (0 until nBeats).map(i => indat(outW * (i + 1) - 1, outW * i)) })

    val head = RegInit(0.U(log2Up(n / nBeats).W))
    val tail = RegInit(0.U(log2Up(n).W))
    val size = RegInit(0.U(log2Up(n + 1).W))

    when (io.in.fire) {
      wdata(head) := io.in.bits
      head := head + 1.U
    }

    when (io.out.fire) { tail := tail + 1.U }

    size := MuxCase(size, Seq(
      (io.in.fire && io.out.fire) -> (size + (nBeats - 1).U),
      io.in.fire -> (size + nBeats.U),
      io.out.fire -> (size - 1.U)))

    io.out.valid := size > 0.U
    io.out.bits := rdata(tail)
    io.in.ready := size < n.U
    io.count := size
  } else {
    val nBeats = outW / inW

    require(outW % inW == 0, s"MultiWidthFifo: out: $outW not divisible by in: $inW")

    val wdata = Reg(Vec(n * nBeats, Bits(inW.W)))
    val rdata = VecInit(Seq.tabulate(n) { i =>
      Cat(wdata.slice(i * nBeats, (i + 1) * nBeats).reverse)})

    val head = RegInit(0.U(log2Up(n * nBeats).W))
    val tail = RegInit(0.U(log2Up(n).W))
    val size = RegInit(0.U(log2Up(n * nBeats + 1).W))

    when (io.in.fire) {
      wdata(head) := io.in.bits
      head := head + 1.U
    }

    when (io.out.fire) { tail := tail + 1.U }

    size := MuxCase(size, Seq(
      (io.in.fire && io.out.fire) -> (size - (nBeats - 1).U),
      io.in.fire -> (size + 1.U),
      io.out.fire -> (size - nBeats.U)))

    io.count := size >> log2Up(nBeats).U
    io.out.valid := io.count > 0.U
    io.out.bits := rdata(tail)
    io.in.ready := size < (n * nBeats).U
  }
}

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


