// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._

class HellaFlowQueue[T <: Data](val entries: Int)(data: => T) extends Module {
  val io = new QueueIO(data, entries)
  require(entries > 1)

  val do_flow = Wire(Bool())
  val do_enq = io.enq.fire() && !do_flow
  val do_deq = io.deq.fire() && !do_flow

  val maybe_full = Reg(init=Bool(false))
  val enq_ptr = Counter(do_enq, entries)._1
  val (deq_ptr, deq_done) = Counter(do_deq, entries)
  when (do_enq =/= do_deq) { maybe_full := do_enq }

  val ptr_match = enq_ptr === deq_ptr
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val atLeastTwo = full || enq_ptr - deq_ptr >= UInt(2)
  do_flow := empty && io.deq.ready

  val ram = SeqMem(entries, data)
  when (do_enq) { ram.write(enq_ptr, io.enq.bits) }

  // BUG! does not hold the output of the SRAM when !ready
  // ... However, HellaQueue is correct due to the pipe stage
  val ren = io.deq.ready && (atLeastTwo || !io.deq.valid && !empty)
  val raddr = Mux(io.deq.valid, Mux(deq_done, UInt(0), deq_ptr + UInt(1)), deq_ptr)
  val ram_out_valid = Reg(next = ren)

  io.deq.valid := Mux(empty, io.enq.valid, ram_out_valid)
  io.enq.ready := !full
  io.deq.bits := Mux(empty, io.enq.bits, ram.read(raddr, ren))
}

class HellaQueue[T <: Data](val entries: Int)(data: => T) extends Module {
  val io = new QueueIO(data, entries)

  val fq = Module(new HellaFlowQueue(entries)(data))
  fq.io.enq <> io.enq
  io.deq <> Queue(fq.io.deq, 1, pipe = true)
}

object HellaQueue {
  def apply[T <: Data](enq: DecoupledIO[T], entries: Int) = {
    val q = Module((new HellaQueue(entries)) { enq.bits })
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    q.io.deq
  }
}


