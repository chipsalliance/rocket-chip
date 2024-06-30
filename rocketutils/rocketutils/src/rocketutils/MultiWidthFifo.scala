// See LICENSE.Berkeley for license details.

package org.chipsalliance.rocketutils

import chisel3._
import chisel3.util._

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
