package Top
{

import Chisel._
import Node._;
import scala.math._;

class ioQueueCtrl(addr_sz: Int) extends Bundle()
{
  val q_reset = Bool('input);
  val enq_val = Bool('input);
  val enq_rdy = Bool('output);
  val deq_val = Bool('output);
  val deq_rdy = Bool('input);
  val wen     = Bool('output);
  val waddr   = UFix(addr_sz, 'output);
  val raddr   = UFix(addr_sz, 'output);
}

class queueCtrl(entries: Int) extends Component
{
  val addr_sz = log2up(entries)
  override val io = new ioQueueCtrl(addr_sz);

  // Enqueue and dequeue pointers

  val enq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz));
  val deq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz));
  val full    = Reg(width = 1, resetVal = Bool(false));
  
  when (io.q_reset) {
    enq_ptr <== UFix(0, addr_sz);
    deq_ptr <== UFix(0, addr_sz);
    full <== Bool(false);
  }

  io.waddr := enq_ptr;
  io.raddr := deq_ptr;

  // We enq/deq only when they are both ready and valid

  val do_enq = io.enq_rdy && io.enq_val;
  val do_deq = io.deq_rdy && io.deq_val;

  // Determine if we have pipeline or flowthrough behaviour and
  // set the write enable accordingly.

  val empty = ~full && (enq_ptr === deq_ptr);

  io.wen := do_enq;

  // Ready signals are calculated from full register. If pipeline
  // behavior is enabled, then the enq_rdy signal is also calculated
  // combinationally from the deq_rdy signal. If flowthrough behavior
  // is enabled then the deq_val signal is also calculated combinationally
  // from the enq_val signal.

  io.enq_rdy := ~full;
  io.deq_val := ~empty;

  // Control logic for the enq/deq pointers and full register

  val deq_ptr_inc = deq_ptr + UFix(1, 1);
  val enq_ptr_inc = enq_ptr + UFix(1, 1);

  val deq_ptr_next =
    Mux(do_deq, deq_ptr_inc,
        deq_ptr);

  val enq_ptr_next =
    Mux(do_enq, enq_ptr_inc,
        enq_ptr);

  val full_next = 
    Mux(do_enq && ~do_deq && ( enq_ptr_inc === deq_ptr ), Bool(true),
    Mux(do_deq && full,                                   Bool(false),
        full));

  enq_ptr <== enq_ptr_next;
  deq_ptr <== deq_ptr_next;
  full    <== full_next;
}

class ioQueueSimplePF[T <: Data]()(data: => T) extends Bundle
{
  val q_reset = Bool('input);
  val enq     = new ioDecoupled()(data)
  val deq     = new ioDecoupled()(data).flip
}

class queueSimplePF[T <: Data](entries: Int)(data: => T) extends Component
{
  override val io = new ioQueueSimplePF()(data);
  val ctrl = new queueCtrl(entries);
  ctrl.io.q_reset ^^ io.q_reset;
  ctrl.io.deq_val ^^ io.deq.valid;
  ctrl.io.enq_rdy ^^ io.enq.ready;
  ctrl.io.enq_val ^^ io.enq.valid;     
  ctrl.io.deq_rdy ^^ io.deq.ready;
  val ram = Mem(entries, ctrl.io.wen, ctrl.io.waddr, io.enq.bits);
  ram.read(ctrl.io.raddr) ^^ io.deq.bits;
}

// TODO: SHOULD USE INHERITANCE BUT BREAKS INTROSPECTION CODE
// class IOqueueCtrlFlow extends IOqueueCtrl 
class ioQueueCtrlFlow(addr_sz: Int) extends Bundle() /* IOqueueCtrl */
{
  val enq_val  = Bool('input);
  val enq_rdy  = Bool('output);
  val deq_val  = Bool('output);
  val deq_rdy  = Bool('input);
  val wen      = Bool('output);
  val waddr    = UFix(addr_sz, 'output);
  val raddr    = UFix(addr_sz, 'output);
  val flowthru = Bool('output);
}

class queueCtrlFlow(entries: Int) extends Component
{
  val addr_sz = log2up(entries)
  override val io = new ioQueueCtrlFlow(addr_sz);
  // Enqueue and dequeue pointers

  val enq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz));
  val deq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz));
  val full    = Reg(width = 1, resetVal = Bool(false));

  io.waddr := enq_ptr;
  io.raddr := deq_ptr;

  // We enq/deq only when they are both ready and valid

  val do_enq = io.enq_rdy && io.enq_val;
  val do_deq = io.deq_rdy && io.deq_val;

  // Determine if we have pipeline or flowthrough behaviour and
  // set the write enable accordingly.

  val empty       = ~full && (enq_ptr === deq_ptr);
  val do_flowthru = empty && do_enq && do_deq;
  io.flowthru  := do_flowthru;

  io.wen    := do_enq && ~do_flowthru;

  // Ready signals are calculated from full register. If pipeline
  // behavior is enabled, then the enq_rdy signal is also calculated
  // combinationally from the deq_rdy signal. If flowthrough behavior
  // is enabled then the deq_val signal is also calculated combinationally
  // from the enq_val signal.

  io.enq_rdy  := ~full;
  io.deq_val  := ~empty || ( empty && io.enq_val );

  // Control logic for the enq/deq pointers and full register

  val deq_ptr_inc = deq_ptr + UFix(1, 1);
  val enq_ptr_inc = enq_ptr + UFix(1, 1);

  val deq_ptr_next =
    Mux(do_deq && ~do_flowthru, deq_ptr_inc,
        deq_ptr);

  val enq_ptr_next =
    Mux(do_enq && ~do_flowthru, enq_ptr_inc,
        enq_ptr);

  val full_next = 
    Mux(do_enq && ~do_deq && ( enq_ptr_inc === deq_ptr ), Bool(true),
    Mux(do_deq && full,                                   Bool(false),
        full));

  enq_ptr <== enq_ptr_next;
  deq_ptr <== deq_ptr_next;
  full    <== full_next;
}

class ioQueueDpathFlow[T <: Data](addr_sz: Int)(data: => T) extends Bundle()
{
  val wen         = Bool('input);
  val flowthru    = Bool('input);
  val deq_bits    = data.asOutput;
  val enq_bits    = data.asInput;
  val waddr       = UFix(addr_sz, 'input);
  val raddr       = UFix(addr_sz, 'input);
}

class queueDpathFlow[T <: Data](entries: Int)(data: => T) extends Component
{
  val addr_sz = log2up(entries)
  override val io = new ioQueueDpathFlow(addr_sz)(data);
  val ram  = Mem(entries, io.wen, io.waddr, io.enq_bits);
  val rout = ram(io.raddr);
  Mux(io.flowthru, io.enq_bits, rout) ^^ io.deq_bits;
}

class ioQueueFlowPF[T <: Data](data: => T) extends Bundle()
{
  val enq_val     = Bool('input);
  val enq_rdy     = Bool('output);
  val enq_bits    = data.asInput;
  val deq_val     = Bool('output);
  val deq_rdy     = Bool('input);
  val deq_bits    = data.asOutput;
}

class queueFlowPF[T <: Data](entries: Int)(data: => T) extends Component
{
  override val io = new ioQueueFlowPF(data);
  val ctrl  = new queueCtrlFlow(entries);
  val dpath = new queueDpathFlow(entries)(data);
  
  ctrl.io.deq_rdy   ^^ io.deq_rdy;
  ctrl.io.wen       <> dpath.io.wen;
  ctrl.io.raddr     <> dpath.io.raddr;
  ctrl.io.waddr     <> dpath.io.waddr;
  ctrl.io.flowthru  <> dpath.io.flowthru;
  ctrl.io.enq_val   ^^ io.enq_val;       
  dpath.io.enq_bits ^^ io.enq_bits;

  ctrl.io.deq_val   ^^ io.deq_val;
  ctrl.io.enq_rdy   ^^ io.enq_rdy;
  dpath.io.deq_bits ^^ io.deq_bits;
}

}
