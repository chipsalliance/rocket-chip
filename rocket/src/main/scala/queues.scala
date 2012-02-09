package Top

import Chisel._
import Node._;

class ioQueue[T <: Data](flushable: Boolean)(data: => T) extends Bundle
{
  val flush = if (flushable) Bool(INPUT) else null
  val enq   = new ioDecoupled()(data)
  val deq   = new ioDecoupled()(data).flip
}

class queue[T <: Data](entries: Int, flushable: Boolean = false)(data: => T) extends Component
{
  val io = new ioQueue(flushable)(data)

  val enq_ptr = Reg(resetVal = UFix(0, log2up(entries)))
  val deq_ptr = Reg(resetVal = UFix(0, log2up(entries)))
  val maybe_full = Reg(resetVal = Bool(false))

  io.deq.valid :=  maybe_full || enq_ptr != deq_ptr
  io.enq.ready := !maybe_full || enq_ptr != deq_ptr

  val do_enq = io.enq.ready && io.enq.valid
  val do_deq = io.deq.ready && io.deq.valid

  if (flushable) {
    when (io.flush) {
      deq_ptr <== UFix(0)
      enq_ptr <== UFix(0)
      maybe_full <== Bool(false)
    }
  }
  when (do_deq) {
    deq_ptr <== deq_ptr + UFix(1)
  }
  when (do_enq) {
    enq_ptr <== enq_ptr + UFix(1)
  }
  when (do_enq != do_deq) {
    maybe_full <== do_enq
  }

  Mem(entries, do_enq, enq_ptr, io.enq.bits).read(deq_ptr) <> io.deq.bits
}
