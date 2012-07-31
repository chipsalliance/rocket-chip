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

class queue[T <: Data](val entries: Int, pipe: Boolean = false, flow: Boolean = false, flushable: Boolean = false)(data: => T) extends Component
{
  val io = new ioQueue(entries, flushable)(data)

  val do_flow = Bool()
  val do_enq = io.enq.ready && io.enq.valid && !do_flow
  val do_deq = io.deq.ready && io.deq.valid && !do_flow

  var enq_ptr = UFix(0)
  var deq_ptr = UFix(0)

  if (entries > 1)
  {
    enq_ptr = Counter(do_enq, entries)._1
    deq_ptr = Counter(do_deq, entries)._1
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
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val maybe_flow = Bool(flow) && empty
  do_flow := maybe_flow && io.deq.ready
  io.deq.valid :=  !empty || Bool(flow) && io.enq.valid
  io.enq.ready := !full || Bool(pipe) && io.deq.ready
  io.deq.bits := Mux(maybe_flow, io.enq.bits, ram(deq_ptr))

  val ptr_diff = enq_ptr - deq_ptr
  if (isPow2(entries))
    io.count := Cat(maybe_full && ptr_match, ptr_diff).toUFix
  else
    io.count := Mux(ptr_match, Mux(maybe_full, UFix(entries), UFix(0)), Mux(deq_ptr > enq_ptr, UFix(entries) + ptr_diff, ptr_diff))
}

object Queue
{
  def apply[T <: Data](enq: FIFOIO[T], entries: Int = 2, pipe: Boolean = false) = {
    val q = (new queue(entries, pipe)) { enq.bits.clone }
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
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

  val r = Reg() { io.enq.bits.clone }
  when (io.enq.valid) { r := io.enq.bits }

  io.deq.valid := Reg(io.enq.valid, resetVal = Bool(false))
  io.deq.bits <> r
}

object Pipe
{
  def apply[T <: Data](enqValid: Bool, enqBits: T, latency: Int): PipeIO[T] = {
    val q = (new pipereg) { enqBits.clone }
    q.io.enq.valid := enqValid
    q.io.enq.bits := enqBits
    q.io.deq

    if (latency > 1)
      Pipe(q.io.deq, latency-1)
    else
      q.io.deq
  }
  def apply[T <: Data](enqValid: Bool, enqBits: T): PipeIO[T] = apply(enqValid, enqBits, 1)
  def apply[T <: Data](enq: PipeIO[T], latency: Int = 1): PipeIO[T] = apply(enq.valid, enq.bits, latency)
}

class SkidBuffer[T <: Data]()(data: => T) extends Component
{
  val io = new Bundle {
    val enq = new FIFOIO()(data).flip
    val deq = new FIFOIO()(data)
  }

  val fq = new queue(1, flow = true)(data)
  val pq = new queue(1, pipe = true)(data)

  fq.io.enq <> io.enq
  pq.io.enq <> fq.io.deq
  io.deq <> pq.io.deq
}

object SkidBuffer
{
  def apply[T <: Data](enq: FIFOIO[T]): FIFOIO[T] = {
    val s = new SkidBuffer()(enq.bits.clone)
    s.io.enq <> enq
    s.io.deq
  }
}
