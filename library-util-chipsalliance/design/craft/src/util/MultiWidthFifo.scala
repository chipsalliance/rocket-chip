// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._

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
