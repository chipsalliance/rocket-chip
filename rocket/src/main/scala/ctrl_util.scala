package Top
{
import Chisel._
import Node._;
import Constants._;

class ioCtrlSboard extends Bundle()
{
  val clr    = Bool(INPUT);
  val clra   = UFix(5, INPUT);
  val set     = Bool(INPUT);
  val seta    = UFix(5, INPUT);
  val raddra  = UFix(5, INPUT);
  val raddrb  = UFix(5, INPUT);
  val raddrc  = UFix(5, INPUT);
  val stalla  = Bool(OUTPUT);
  val stallb  = Bool(OUTPUT);
  val stallc  = Bool(OUTPUT);
}

class rocketCtrlSboard extends Component
{
  override val io = new ioCtrlSboard();
  val reg_busy = Reg(width = 32, resetVal = Bits(0, 32));
  
  val set_mask =   io.set.toUFix << io.seta;
  val clr_mask = ~(io.clr.toUFix << io.clra);
  reg_busy <== (reg_busy | set_mask) & clr_mask
  
  io.stalla  := reg_busy(io.raddra).toBool;
  io.stallb  := reg_busy(io.raddrb).toBool;
  io.stallc  := reg_busy(io.raddrc).toBool;
}

class ioCtrlCnt extends Bundle()
{
  val enq   = Bool(INPUT);
  val deq   = Bool(INPUT);
  val empty = Bool(OUTPUT);
  val full  = Bool(OUTPUT);
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
