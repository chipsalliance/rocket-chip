package Top
{
import Chisel._
import Node._;
import Constants._;

class ioCtrlSboard extends Bundle()
{
  val clr0    = Bool('input);
  val clr0a   = UFix(5, 'input);
  val clr1    = Bool('input);
  val clr1a   = UFix(5, 'input);
  val set     = Bool('input);
  val seta    = UFix(5, 'input);
  val raddra  = UFix(5, 'input);
  val raddrb  = UFix(5, 'input);
  val raddrc  = UFix(5, 'input);
  val stalla  = Bool('output);
  val stallb  = Bool('output);
  val stallc  = Bool('output);
  val stallra = Bool('output);
}

class rocketCtrlSboard extends Component
{
  override val io = new ioCtrlSboard();
  val reg_busy = Reg(width = 32, resetVal = Bits(0, 32));
  
  val set_mask  = Mux(io.set, UFix(1,1) << io.seta, UFix(0,32));
  val clr0_mask = Mux(io.clr0, ~(UFix(1,1) << io.clr0a), ~UFix(0,32));
  val clr1_mask = Mux(io.clr1, ~(UFix(1,1) << io.clr1a), ~UFix(0,32));
  reg_busy <== ((reg_busy | set_mask) & clr0_mask) & clr1_mask;
  
  io.stalla  := reg_busy(io.raddra).toBool;
  io.stallb  := reg_busy(io.raddrb).toBool;
  io.stallc  := reg_busy(io.raddrc).toBool;
  io.stallra := reg_busy(RA).toBool;
}

class ioCtrlCnt extends Bundle()
{
  val enq   = Bool('input);
  val deq   = Bool('input);
  val empty = Bool('output);
  val full  = Bool('output);
}

class rocketCtrlCnt(n_bits: Int, limit: Int) extends Component
{
  override val io = new ioCtrlCnt();
  val counter = Reg(width = n_bits, resetVal = UFix(0, n_bits));  
  when (io.enq && !io.deq) {
    counter <== counter + UFix(1, n_bits);
  }
  when (!io.enq && io.deq) {
    counter <== counter - UFix(1, n_bits);
  }
  io.empty := counter === UFix(0,     n_bits);
  io.full  := counter === UFix(limit, n_bits);
}

}
