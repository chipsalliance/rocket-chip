// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.util.{ReadyValidIO}

case class AsyncQueueParams(
  depth:  Int     = 8,
  sync:   Int     = 3,
  safe:   Boolean = true,
// If safe is true, then effort is made to resynchronize the crossing indices when either side is reset.
// This makes it safe/possible to reset one side of the crossing (but not the other) when the queue is empty.
  narrow: Boolean = false)
// If narrow is true then the read mux is moved to the source side of the crossing.
// This reduces the number of level shifters in the case where the clock crossing is also a voltage crossing,
// at the expense of a combinational path from the sink to the source and back to the sink.
{
  require (depth > 0 && isPow2(depth))
  require (sync >= 2)

  val bits = log2Ceil(depth)
  val wires = if (narrow) 1 else depth
}

object AsyncQueueParams {
  // When there is only one entry, we don't need safety or narrow.
  def singleton(sync: Int = 3) = AsyncQueueParams(1, sync, false, false)
}

final class AsyncBundle[T <: Data](private val gen: T, val params: AsyncQueueParams = AsyncQueueParams()) extends Bundle
{
  // Data-path synchronization
  val mem  = Vec(params.wires, gen)
  val index = params.narrow.option(UInt(width = params.bits))
  val ridx = UInt(width = params.bits+1).flip
  val widx = UInt(width = params.bits+1)

  // Signals used to self-stabilize a safe AsyncQueue
  val ridx_valid = Bool().flip
  val widx_valid = Bool()
  val source_reset_n = Bool()
  val sink_reset_n = Bool().flip
}

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

class AsyncQueueSource[T <: Data](gen: T, params: AsyncQueueParams = AsyncQueueParams()) extends Module {
  val io = new Bundle {
    // These come from the source domain
    val enq  = Decoupled(gen).flip
    // These cross to the sink clock domain
    val async = new AsyncBundle(gen, params)
  }

  val bits = params.bits
  val sink_ready = Wire(init = Bool(true))
  val mem = Reg(Vec(params.depth, gen)) // This does NOT need to be reset at all.
  val widx = GrayCounter(bits+1, io.enq.fire(), !sink_ready, "widx_bin")
  val ridx = AsyncResetSynchronizerShiftReg(io.async.ridx, params.sync, Some("ridx_gray"))
  val ready = sink_ready && widx =/= (ridx ^ UInt(params.depth | params.depth >> 1))

  val index = if (bits == 0) UInt(0) else io.async.widx(bits-1, 0) ^ (io.async.widx(bits, bits) << (bits-1))
  when (io.enq.fire()) { mem(index) := io.enq.bits }

  val ready_reg = AsyncResetReg(ready.asUInt, "ready_reg")(0)
  io.enq.ready := ready_reg && sink_ready

  val widx_reg = AsyncResetReg(widx, "widx_gray")
  io.async.widx := widx_reg

  io.async.index match {
    case Some(index) => io.async.mem(0) := mem(index)
    case None => io.async.mem := mem
  }

  io.async.widx_valid := Bool(true)
  if (params.safe) {
    val source_valid = Module(new AsyncValidSync(params.sync+1, "source_valid"))
    val sink_extend  = Module(new AsyncValidSync(1, "sink_extend"))
    val sink_valid   = Module(new AsyncValidSync(params.sync, "sink_valid"))
    source_valid.reset := reset || !io.async.sink_reset_n
    sink_extend .reset := reset || !io.async.sink_reset_n

    source_valid.io.in := Bool(true)
    io.async.widx_valid := source_valid.io.out
    sink_extend.io.in := io.async.ridx_valid
    sink_valid.io.in := sink_extend.io.out
    sink_ready := sink_valid.io.out
    io.async.source_reset_n := !reset

    // Assert that if there is stuff in the queue, then reset cannot happen
    //  Impossible to write because dequeue can occur on the receiving side,
    //  then reset allowed to happen, but write side cannot know that dequeue
    //  occurred.
    // TODO: write some sort of sanity check assertion for users
    // that denote don't reset when there is activity
//    assert (!(reset || !io.async.sink_reset_n) || !io.enq.valid, "Enque while sink is reset and AsyncQueueSource is unprotected")
//    assert (!reset_rise || prev_idx_match.toBool, "Sink reset while AsyncQueueSource not empty")
  }
}

class AsyncQueueSink[T <: Data](gen: T, params: AsyncQueueParams = AsyncQueueParams()) extends Module {
  val io = new Bundle {
    // These come from the sink domain
    val deq  = Decoupled(gen)
    // These cross to the source clock domain
    val async = new AsyncBundle(gen, params).flip
  }

  val bits = params.bits
  val source_ready = Wire(init = Bool(true))
  val ridx = GrayCounter(bits+1, io.deq.fire(), !source_ready, "ridx_bin")
  val widx = AsyncResetSynchronizerShiftReg(io.async.widx, params.sync, Some("widx_gray"))
  val valid = source_ready && ridx =/= widx

  // The mux is safe because timing analysis ensures ridx has reached the register
  // On an ASIC, changes to the unread location cannot affect the selected value
  // On an FPGA, only one input changes at a time => mem updates don't cause glitches
  // The register only latches when the selected valued is not being written
  val index = if (bits == 0) UInt(0) else ridx(bits-1, 0) ^ (ridx(bits, bits) << (bits-1))
  io.async.index.foreach { _ := index }
  // This register does not NEED to be reset, as its contents will not
  // be considered unless the asynchronously reset deq valid register is set.
  // It is possible that bits latches when the source domain is reset / has power cut
  // This is safe, because isolation gates brought mem low before the zeroed widx reached us
  val deq_bits_nxt = Mux(valid, io.async.mem(if (params.narrow) UInt(0) else index), io.deq.bits)
  io.deq.bits := SynchronizerShiftReg(deq_bits_nxt, sync = 1, name = Some("deq_bits_reg"))

  val valid_reg = AsyncResetReg(valid.asUInt, "valid_reg")(0)
  io.deq.valid := valid_reg && source_ready

  val ridx_reg = AsyncResetReg(ridx, "ridx_gray")
  io.async.ridx := ridx_reg

  io.async.ridx_valid := Bool(true)
  if (params.safe) {
    val sink_valid    = Module(new AsyncValidSync(params.sync+1, "sink_valid"))
    val source_extend = Module(new AsyncValidSync(1, "source_extend"))
    val source_valid  = Module(new AsyncValidSync(params.sync, "source_valid"))
    sink_valid   .reset := reset || !io.async.source_reset_n
    source_extend.reset := reset || !io.async.source_reset_n

    sink_valid.io.in := Bool(true)
    io.async.ridx_valid := sink_valid.io.out
    source_extend.io.in := io.async.widx_valid
    source_valid.io.in := source_extend.io.out
    source_ready := source_valid.io.out
    io.async.sink_reset_n := !reset

    val reset_and_extend = !source_ready || !io.async.source_reset_n || reset
    val reset_and_extend_prev = Reg(Bool(), reset_and_extend, Bool(true))
    val reset_rise = !reset_and_extend_prev && reset_and_extend
    val prev_idx_match = AsyncResetReg(updateData=(io.async.widx===io.async.ridx), resetData=0)

    // TODO: write some sort of sanity check assertion for users
    // that denote don't reset when there is activity
//    assert (!reset_rise || prev_idx_match.toBool, "Source reset while AsyncQueueSink not empty")
  }
}

object FromAsyncBundle
{
  // Sometimes it makes sense for the sink to have different sync than the source
  def apply[T <: Data](x: AsyncBundle[T]): DecoupledIO[T] = apply(x, x.params.sync)
  def apply[T <: Data](x: AsyncBundle[T], sync: Int): DecoupledIO[T] = {
    val sink = Module(new AsyncQueueSink(x.mem(0).cloneType, x.params.copy(sync = sync)))
    sink.io.async <> x
    sink.io.deq
  }
}

object ToAsyncBundle
{
  def apply[T <: Data](x: ReadyValidIO[T], params: AsyncQueueParams = AsyncQueueParams()): AsyncBundle[T] = {
    val source = Module(new AsyncQueueSource(x.bits, params))
    source.io.enq <> x
    source.io.async
  }
}

class AsyncQueue[T <: Data](gen: T, params: AsyncQueueParams = AsyncQueueParams()) extends Crossing[T] {
  val io = new CrossingIO(gen)
  val source = Module(new AsyncQueueSource(gen, params))
  val sink   = Module(new AsyncQueueSink  (gen, params))

  source.clock := io.enq_clock
  source.reset := io.enq_reset
  sink.clock := io.deq_clock
  sink.reset := io.deq_reset

  source.io.enq <> io.enq
  io.deq <> sink.io.deq
  sink.io.async <> source.io.async
}
