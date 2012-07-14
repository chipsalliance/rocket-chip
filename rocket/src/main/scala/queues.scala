package rocket

import Chisel._
import Node._;

class ioQueue[T <: Data](entries: Int, flushable: Boolean)(data: => T) extends Bundle
{
  val flush = if (flushable) Bool(INPUT) else null
  val enq   = new FIFOIO()(data).flip
  val deq   = new FIFOIO()(data)
  val count = UFix(OUTPUT, log2Up(entries+1))
}

class queue[T <: Data](val entries: Int, pipe: Boolean = false, flushable: Boolean = false)(data: => T) extends Component
{
  val io = new ioQueue(entries, flushable)(data)

  val do_enq = io.enq.ready && io.enq.valid
  val do_deq = io.deq.ready && io.deq.valid

  var enq_ptr = UFix(0)
  var deq_ptr = UFix(0)
  val pow2 = (entries & (entries-1)) == 0

  if (entries > 1)
  {
    enq_ptr = Reg(resetVal = UFix(0, log2Up(entries)))
    deq_ptr = Reg(resetVal = UFix(0, log2Up(entries)))

    var enq_next = enq_ptr + UFix(1)
    var deq_next = deq_ptr + UFix(1)
    if (!pow2) {
      enq_next = Mux(enq_ptr === UFix(entries-1), UFix(0), enq_next)
      deq_next = Mux(deq_ptr === UFix(entries-1), UFix(0), deq_next)
    }

    when (do_deq) { deq_ptr := deq_next }
    when (do_enq) { enq_ptr := enq_next }
    if (flushable) {
      when (io.flush) {
        deq_ptr := UFix(0)
        enq_ptr := UFix(0)
      }
    }
  }

  val maybe_full = Reg(resetVal = Bool(false))
  when (do_enq != do_deq) {
    maybe_full := do_enq
  }
  if (flushable) {
    when (io.flush) {
      maybe_full := Bool(false)
    }
  }

  val ram = Mem(entries) { data }
  when (do_enq) { ram(enq_ptr) := io.enq.bits }

  val ptr_match = enq_ptr === deq_ptr
  io.deq.valid :=  maybe_full || !ptr_match
  io.enq.ready := !maybe_full || !ptr_match || (if (pipe) io.deq.ready else Bool(false))
  io.deq.bits <> ram(deq_ptr)

  val ptr_diff = enq_ptr - deq_ptr
  if (pow2)
    io.count := Cat(maybe_full && ptr_match, ptr_diff).toUFix
  else
    io.count := Mux(ptr_match, Mux(maybe_full, UFix(entries), UFix(0)), Mux(deq_ptr > enq_ptr, UFix(entries) + ptr_diff, ptr_diff))
}

object Queue
{
  def apply[T <: Data](enq: FIFOIO[T], entries: Int = 2, pipe: Boolean = false) = {
    val q = (new queue(entries, pipe)) { enq.bits.clone }
    q.io.enq <> enq
    q.io.deq
  }
}

class pipereg[T <: Data]()(data: => T) extends Component
{
  val io = new Bundle {
    val enq = new PipeIO()(data).flip
    val deq = new PipeIO()(data)
  }

  //val bits = Reg() { io.enq.bits.clone }
  //when (io.enq.valid) {
  //  bits := io.enq.bits
  //}

  val reg = Reg() { io.enq.bits.clone }
  when (io.enq.valid) { reg := io.enq.bits }

  io.deq.valid := Reg(io.enq.valid, resetVal = Bool(false))
  io.deq.bits <> reg
}

object Pipe
{
  def apply[T <: Data](enq: PipeIO[T], latency: Int = 1): PipeIO[T] = {
    val q = (new pipereg) { enq.bits.clone }
    q.io.enq <> enq
    q.io.deq

    if (latency > 1)
      Pipe(q.io.deq, latency-1)
    else
      q.io.deq
  }
}
