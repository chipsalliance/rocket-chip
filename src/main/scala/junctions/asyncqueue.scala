// See LICENSE for license details.

package junctions
import Chisel._

object GrayCounter {
  def apply(bits: Int, increment: Bool = Bool(true)): UInt = {
    val binary = Module (new AsyncRegVec(w=bits, 0))//RegInit(UInt(0, width = bits))
    val incremented = binary.io.q + increment.asUInt()
    binary.io.d := incremented
    binary.io.en := Bool(true)
    incremented ^ (incremented >> UInt(1))
  }
}

object AsyncGrayCounter {
  def apply(in: UInt, sync: Int): UInt = {
    val syncv = Vec.fill(sync)(new AsyncResetRegVec(w = in.getWidth, 0))//RegInit(Vec.fill(sync){UInt(0, width = in.getWidth)})
    syncv.last.io.d := in
      (syncv.init zip syncv.tail).foreach { case (sink, source) => {
        sink.io.d := source.io.q
        sink.io.en := Bool(true)
      }
      }
    syncv(0).io.d
  }
}

class AsyncQueueSource[T <: Data](gen: T, depth: Int, sync: Int, clockIn: Clock, resetIn: Bool)
    extends Module(_clock = clockIn, _reset = resetIn) {
  val bits = log2Ceil(depth)
  val io = new Bundle {
    // These come from the source domain
    val enq  = Decoupled(gen).flip()
    // These cross to the sink clock domain
    val ridx = UInt(INPUT,  width = bits+1)
    val widx = UInt(OUTPUT, width = bits+1)
    val mem  = Vec(depth, gen).asOutput
  }

  val mem = Reg(Vec(depth, gen)) //This does NOT need to be asynchronously reset.
  val widx = GrayCounter(bits+1, io.enq.fire())
  val ridx = AsyncGrayCounter(io.ridx, sync)
  val ready = widx =/= (ridx ^ UInt(depth | depth >> 1))

  val index = if (depth == 1) UInt(0) else io.widx(bits-1, 0) ^ (io.widx(bits, bits) << (bits-1))
  when (io.enq.fire() && !reset) { mem(index) := io.enq.bits }
  val ready_reg = Module(neq AsyncResetRegVec(1, 0))
  ready_reg.io.d := ready
  ready_reg.io.en := Bool(true)
  io.enq.ready := ready_reg.io.q

  val widx_reg = Module (new AsyncResetRegVec(bits + 1, 0))
  widx_reg.io.d := widx
  widx_reg.io.en := Bool(true)
  io.widx := widx_reg.io.q

  io.mem := mem
}

class AsyncQueueSink[T <: Data](gen: T, depth: Int, sync: Int, clockIn: Clock, resetIn: Bool)
    extends Module(_clock = clockIn, _reset = resetIn) {
  val bits = log2Ceil(depth)
  val io = new Bundle {
    // These come from the sink domain
    val deq  = Decoupled(gen)
    // These cross to the source clock domain
    val ridx = UInt(OUTPUT, width = bits+1)
    val widx = UInt(INPUT,  width = bits+1)
    val mem  = Vec(depth, gen).asInput
  }

  val ridx = GrayCounter(bits+1, io.deq.fire())
  val widx = AsyncGrayCounter(io.widx, sync)
  val valid = ridx =/= widx

  // The mux is safe because timing analysis ensures ridx has reached the register
  // On an ASIC, changes to the unread location cannot affect the selected value
  // On an FPGA, only one input changes at a time => mem updates don't cause glitches
  // The register only latches when the selected valued is not being written
  val index = if (depth == 1) UInt(0) else ridx(bits-1, 0) ^ (ridx(bits, bits) << (bits-1))
  io.deq.bits  := RegEnable(io.mem(index), valid && !reset) // This does NOT need to be reset.

  val deq_reg = Module (new AsyncResetRegVec(1, 0))
  deq_reg.io.d := valid
  deq_reg.io.en := Bool(true)
  io.deq.valid := deq_reg.io.q //RegNext(valid, Bool(false))

  val ridx_reg = Module (new AsyncResetRegVec(bits + 1, 0))
  ridx_reg.io.d := ridx
  ridx_reg.io.en := Bool(true)
  io.ridx := ridx_reg.io.q //RegNext(ridx, UInt(0))
}

class AsyncQueue[T <: Data](gen: T, depth: Int = 8, sync: Int = 3) extends Crossing[T] {
  require (sync >= 2)
  require (depth > 0 && isPow2(depth))

  val io = new CrossingIO(gen)
  val source = Module(new AsyncQueueSource(gen, depth, sync, io.enq_clock, io.enq_reset))
  val sink   = Module(new AsyncQueueSink  (gen, depth, sync, io.deq_clock, io.deq_reset))

  source.io.enq <> io.enq
  io.deq <> sink.io.deq

  sink.io.mem := source.io.mem
  sink.io.widx := source.io.widx
  source.io.ridx := sink.io.ridx
}
