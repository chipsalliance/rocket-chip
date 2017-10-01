// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

object GrayCounter {
  def apply(bits: Int, increment: Bool = Bool(true), clear: Bool = Bool(false), name: String = "binary"): UInt = {
    val incremented = Wire(UInt(width=bits))
    val binary = AsyncResetReg(incremented, name)
    incremented := Mux(clear, UInt(0), binary + increment.asUInt())
    incremented ^ (incremented >> UInt(1))
  }
}

class AsyncValidSync(sync: Int, desc: String) extends Module {
  val io = new Bundle {
    val in = Bool(INPUT)
    val out = Bool(OUTPUT)
  }
  io.out := AsyncResetSynchronizerShiftReg(io.in, sync, Some(desc))
}

class AsyncQueueSource[T <: Data](gen: T, depth: Int, sync: Int, safe: Boolean = true, narrowData: Boolean = false) extends Module {
  val bits = log2Ceil(depth)
  val io = new Bundle {
    // These come from the source domain
    val enq  = Decoupled(gen).flip
    // These cross to the sink clock domain
    val ridx = UInt(INPUT,  width = bits+1)
    val widx = UInt(OUTPUT, width = bits+1)
    val mem  = Vec(if(narrowData) 1 else depth, gen).asOutput
    val index = narrowData.option(UInt(INPUT, width = bits))
    // Signals used to self-stabilize a safe AsyncQueue
    val sink_reset_n = Bool(INPUT)
    val ridx_valid = Bool(INPUT)
    val widx_valid = Bool(OUTPUT)
  }

  val sink_ready = Wire(init = Bool(true))
  val mem = Reg(Vec(depth, gen)) // This does NOT need to be reset at all.
  val widx = GrayCounter(bits+1, io.enq.fire(), !sink_ready, "widx_bin")
  val ridx = AsyncResetSynchronizerShiftReg(io.ridx, sync, Some("ridx_gray"))
  val ready = sink_ready && widx =/= (ridx ^ UInt(depth | depth >> 1))

  val index = if (depth == 1) UInt(0) else io.widx(bits-1, 0) ^ (io.widx(bits, bits) << (bits-1))
  when (io.enq.fire()) { mem(index) := io.enq.bits }

  val ready_reg = AsyncResetReg(ready.asUInt, "ready_reg")(0)
  io.enq.ready := ready_reg && sink_ready

  val widx_reg = AsyncResetReg(widx, "widx_gray")
  io.widx := widx_reg

  if(narrowData) io.mem(0) := mem(io.index.get) else io.mem := mem

  io.widx_valid := Bool(true)
  if (safe) {
    val source_valid = Module(new AsyncValidSync(sync+1, "source_valid"))
    val sink_extend  = Module(new AsyncValidSync(1, "sink_extend"))
    val sink_valid   = Module(new AsyncValidSync(sync, "sink_valid"))
    source_valid.reset := reset || !io.sink_reset_n
    sink_extend .reset := reset || !io.sink_reset_n

    source_valid.io.in := Bool(true)
    io.widx_valid := source_valid.io.out
    sink_extend.io.in := io.ridx_valid
    sink_valid.io.in := sink_extend.io.out
    sink_ready := sink_valid.io.out

    // Assert that if there is stuff in the queue, then reset cannot happen
    //  Impossible to write because dequeue can occur on the receiving side,
    //  then reset allowed to happen, but write side cannot know that dequeue
    //  occurred.
    // TODO: write some sort of sanity check assertion for users
    // that denote don't reset when there is activity
//    assert (!(reset || !io.sink_reset_n) || !io.enq.valid, "Enque while sink is reset and AsyncQueueSource is unprotected")
//    assert (!reset_rise || prev_idx_match.toBool, "Sink reset while AsyncQueueSource not empty")
  }
}

class AsyncQueueSink[T <: Data](gen: T, depth: Int, sync: Int, safe: Boolean = true, narrowData: Boolean = false) extends Module {
  val bits = log2Ceil(depth)
  val io = new Bundle {
    // These come from the sink domain
    val deq  = Decoupled(gen)
    // These cross to the source clock domain
    val ridx = UInt(OUTPUT, width = bits+1)
    val widx = UInt(INPUT,  width = bits+1)
    val mem  = Vec(if(narrowData) 1 else depth, gen).asInput
    val index = narrowData.option(UInt(OUTPUT, width = bits))
    // Signals used to self-stabilize a safe AsyncQueue
    val source_reset_n = Bool(INPUT)
    val ridx_valid = Bool(OUTPUT)
    val widx_valid = Bool(INPUT)
  }

  val source_ready = Wire(init = Bool(true))
  val ridx = GrayCounter(bits+1, io.deq.fire(), !source_ready, "ridx_bin")
  val widx = AsyncResetSynchronizerShiftReg(io.widx, sync, Some("widx_gray"))
  val valid = source_ready && ridx =/= widx

  // The mux is safe because timing analysis ensures ridx has reached the register
  // On an ASIC, changes to the unread location cannot affect the selected value
  // On an FPGA, only one input changes at a time => mem updates don't cause glitches
  // The register only latches when the selected valued is not being written
  val index = if (depth == 1) UInt(0) else ridx(bits-1, 0) ^ (ridx(bits, bits) << (bits-1))
  if(narrowData) io.index.get := index
  // This register does not NEED to be reset, as its contents will not
  // be considered unless the asynchronously reset deq valid register is set.
  // It is possible that bits latches when the source domain is reset / has power cut
  // This is safe, because isolation gates brought mem low before the zeroed widx reached us
  val deq_bits_nxt = Mux(valid, io.mem(if(narrowData) UInt(0) else index), io.deq.bits)
  io.deq.bits  := SynchronizerShiftReg(deq_bits_nxt, sync = 1, name = Some("deq_bits_reg"))

  val valid_reg = AsyncResetReg(valid.asUInt, "valid_reg")(0)
  io.deq.valid := valid_reg && source_ready

  val ridx_reg = AsyncResetReg(ridx, "ridx_gray")
  io.ridx := ridx_reg

  io.ridx_valid := Bool(true)
  if (safe) {
    val sink_valid    = Module(new AsyncValidSync(sync+1, "sink_valid"))
    val source_extend = Module(new AsyncValidSync(1, "source_extend"))
    val source_valid  = Module(new AsyncValidSync(sync, "source_valid"))
    sink_valid   .reset := reset || !io.source_reset_n
    source_extend.reset := reset || !io.source_reset_n

    sink_valid.io.in := Bool(true)
    io.ridx_valid := sink_valid.io.out
    source_extend.io.in := io.widx_valid
    source_valid.io.in := source_extend.io.out
    source_ready := source_valid.io.out

    val reset_and_extend = !source_ready || !io.source_reset_n || reset
    val reset_and_extend_prev = Reg(Bool(), reset_and_extend, Bool(true))
    val reset_rise = !reset_and_extend_prev && reset_and_extend
    val prev_idx_match = AsyncResetReg(updateData=(io.widx===io.ridx), resetData=0)

    // TODO: write some sort of sanity check assertion for users
    // that denote don't reset when there is activity
//    assert (!reset_rise || prev_idx_match.toBool, "Source reset while AsyncQueueSink not empty")
  }
}

// If narrowData is true then the read mux is moved to the source side of the crossing.
// This reduces the number of level shifters in the case where the clock crossing is also a voltage crossing,
// at the expense of a combinational path from the sink to the source and back to the sink.
class AsyncQueue[T <: Data](gen: T, depth: Int = 8, sync: Int = 3, safe: Boolean = true, narrowData: Boolean = false) extends Crossing[T] {
  require (sync >= 2)
  require (depth > 0 && isPow2(depth))

  val io = new CrossingIO(gen)
  val source = Module(new AsyncQueueSource(gen, depth, sync, safe, narrowData))
  val sink   = Module(new AsyncQueueSink  (gen, depth, sync, safe, narrowData))

  source.clock := io.enq_clock
  source.reset := io.enq_reset
  sink.clock := io.deq_clock
  sink.reset := io.deq_reset

  source.io.sink_reset_n := !io.deq_reset
  sink.io.source_reset_n := !io.enq_reset

  source.io.enq <> io.enq
  io.deq <> sink.io.deq

  sink.io.mem := source.io.mem
  if(narrowData) source.io.index.get <> sink.io.index.get
  sink.io.widx := source.io.widx
  source.io.ridx := sink.io.ridx
  sink.io.widx_valid := source.io.widx_valid
  source.io.ridx_valid := sink.io.ridx_valid
}
