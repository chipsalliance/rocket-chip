// See LICENSE for license details.

package util
import Chisel._

object GrayCounter {
  def apply(bits: Int, increment: Bool = Bool(true), clear: Bool = Bool(false)): UInt = {
    val incremented = Wire(UInt(width=bits))
    val binary = AsyncResetReg(incremented)
    incremented := Mux(clear, UInt(0), binary + increment.asUInt())
    incremented ^ (incremented >> UInt(1))
  }
}

object AsyncGrayCounter {
  def apply(in: UInt, sync: Int): UInt = {
    val syncv = List.fill(sync)(Module (new AsyncResetRegVec(w = in.getWidth, 0)))
    syncv.last.io.d := in
    syncv.last.io.en := Bool(true)
      (syncv.init zip syncv.tail).foreach { case (sink, source) =>
        sink.io.d := source.io.q
        sink.io.en := Bool(true)
      }
    syncv.head.io.q
  }
}

class AsyncQueueSource[T <: Data](gen: T, depth: Int, sync: Int) extends Module {
  val bits = log2Ceil(depth)
  val io = new Bundle {
    // These come from the source domain
    val enq  = Decoupled(gen).flip
    // These cross to the sink clock domain
    val ridx = UInt(INPUT,  width = bits+1)
    val widx = UInt(OUTPUT, width = bits+1)
    val mem  = Vec(depth, gen).asOutput
    // Reset for the other side
    val sink_reset_n = Bool().flip
  }

  // extend the sink reset to a full cycle (assertion latency <= 1 cycle)
  val catch_reset_n = AsyncResetReg(Bool(true), clock, !io.sink_reset_n)
  // reset_n has a 1 cycle shorter path to ready than ridx does
  val reset_n = AsyncGrayCounter(catch_reset_n.asUInt, sync)(0)

  val mem = Reg(Vec(depth, gen)) //This does NOT need to be asynchronously reset.
  val widx = GrayCounter(bits+1, io.enq.fire(), !reset_n)
  val ridx = AsyncGrayCounter(io.ridx, sync)
  val ready = widx =/= (ridx ^ UInt(depth | depth >> 1))

  val index = if (depth == 1) UInt(0) else io.widx(bits-1, 0) ^ (io.widx(bits, bits) << (bits-1))
  when (io.enq.fire()) { mem(index) := io.enq.bits }

  val ready_reg = AsyncResetReg(ready.asUInt)(0)
  io.enq.ready := ready_reg && reset_n

  val widx_reg = AsyncResetReg(widx)
  io.widx := widx_reg

  io.mem := mem

  // It is a fatal error to reset half a Queue while it still has data
  assert (reset_n || widx === ridx)
}

class AsyncQueueSink[T <: Data](gen: T, depth: Int, sync: Int) extends Module {
  val bits = log2Ceil(depth)
  val io = new Bundle {
    // These come from the sink domain
    val deq  = Decoupled(gen)
    // These cross to the source clock domain
    val ridx = UInt(OUTPUT, width = bits+1)
    val widx = UInt(INPUT,  width = bits+1)
    val mem  = Vec(depth, gen).asInput
    // Reset for the other side
    val source_reset_n = Bool().flip
  }

  // extend the source reset to a full cycle (assertion latency <= 1 cycle)
  val catch_reset_n = AsyncResetReg(Bool(true), clock, !io.source_reset_n)
  // reset_n has a 1 cycle shorter path to valid than widx does
  val reset_n = AsyncGrayCounter(catch_reset_n.asUInt, sync)(0)

  val ridx = GrayCounter(bits+1, io.deq.fire(), !reset_n)
  val widx = AsyncGrayCounter(io.widx, sync)
  val valid = ridx =/= widx

  // The mux is safe because timing analysis ensures ridx has reached the register
  // On an ASIC, changes to the unread location cannot affect the selected value
  // On an FPGA, only one input changes at a time => mem updates don't cause glitches
  // The register only latches when the selected valued is not being written
  val index = if (depth == 1) UInt(0) else ridx(bits-1, 0) ^ (ridx(bits, bits) << (bits-1))
  // This register does not NEED to be reset, as its contents will not
  // be considered unless the asynchronously reset deq valid register is set.
  io.deq.bits  := RegEnable(io.mem(index), valid)

  val valid_reg = AsyncResetReg(valid.asUInt)(0)
  io.deq.valid := valid_reg && reset_n

  val ridx_reg = AsyncResetReg(ridx)
  io.ridx := ridx_reg

  // It is a fatal error to reset half a Queue while it still has data
  assert (reset_n || widx === ridx)
}

class AsyncQueue[T <: Data](gen: T, depth: Int = 8, sync: Int = 3) extends Crossing[T] {
  require (sync >= 2)
  require (depth > 0 && isPow2(depth))

  val io = new CrossingIO(gen)
  val source = Module(new AsyncQueueSource(gen, depth, sync))
  source.suggestName("sourceInst")
  val sink   = Module(new AsyncQueueSink  (gen, depth, sync))
  sink.suggestName("sinkInst")

  source.clock := io.enq_clock
  source.reset := io.enq_reset
  sink.clock := io.deq_clock
  sink.reset := io.deq_reset

  source.io.sink_reset_n := !io.deq_reset
  sink.io.source_reset_n := !io.enq_reset

  source.io.enq <> io.enq
  io.deq <> sink.io.deq

  sink.io.mem := source.io.mem
  sink.io.widx := source.io.widx
  source.io.ridx := sink.io.ridx
}
