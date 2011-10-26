package Top
{

import Chisel._
import Node._;

import Constants._;

class ioDpathBTB extends Bundle()
{
  val current_pc4    = UFix(32, 'input);
  val hit            = Bool('output);
  val target         = UFix(32, 'output);
  val wen            = Bool('input);
  val correct_pc4    = UFix(32, 'input);
  val correct_target = UFix(32, 'input);
}

class rocketDpathBTB extends Component
{
  override val io = new ioDpathBTB();
  val rst_lwlr_pf  = Mem(4, io.wen, io.correct_pc4(3, 2), UFix(1, 1), resetVal = UFix(0, 1)); 
  val lwlr_pf      = Mem(4, io.wen, io.correct_pc4(3, 2), 
                         Cat(io.correct_pc4(31,4), io.correct_target(31,2)), resetVal = UFix(0, 1));
  val is_val       = rst_lwlr_pf(io.current_pc4(3, 2));
  val tag_target   = lwlr_pf(io.current_pc4(3, 2));
  io.hit    := (is_val & (tag_target(57,30) === io.current_pc4(31, 4))).toBool;
  io.target := Cat(tag_target(29, 0), Bits(0,2)).toUFix;
}

class ioDpathPCR extends Bundle()
{
  val host  = new ioHost(List("from", "from_wen", "to"));
  val debug = new ioDebug();
  val r     = new ioReadPort();
  val w     = new ioWritePort();
  
  val status 		= Bits(8, 'output);
  val exception = Bool('input);
  val cause 		= UFix(5, 'input);
  val pc    		= UFix(32, 'input);
  val eret  		= Bool('input);
}

class rocketDpathPCR extends Component
{
  override val io = new ioDpathPCR();
  
  val HAVE_FPU = Bool(false);
  val HAVE_VEC = Bool(false);
  val w = 32;
  val reg_epc      = Reg(resetVal = Bits(0, w)); 
  val reg_badvaddr = Reg(resetVal = Bits(0, w)); 
  val reg_ebase    = Reg(resetVal = Bits(0, w)); 
  val reg_count    = Reg(resetVal = Bits(0, w)); 
  val reg_compare  = Reg(resetVal = Bits(0, w)); 
  val reg_cause    = Reg(resetVal = Bits(0, 5));
  val reg_tohost   = Reg(resetVal = Bits(0, w)); 
  val reg_fromhost = Reg(resetVal = Bits(0, w));
  val reg_k0       = Reg(resetVal = Bits(0, 2*w));
  val reg_k1       = Reg(resetVal = Bits(0, 2*w));
  
  val reg_error_mode  = Reg(resetVal = Bool(false));
  val reg_log_control = Reg(resetVal = Bool(false));
  val reg_status_im   = Reg(resetVal = Bits(0,8));
  val reg_status_sx   = Reg(resetVal = Bool(true));
  val reg_status_ux   = Reg(resetVal = Bool(true));
  val reg_status_ef   = Reg(resetVal = Bool(false));
  val reg_status_ev   = Reg(resetVal = Bool(false));
  val reg_status_s    = Reg(resetVal = Bool(true));
  val reg_status_ps   = Reg(resetVal = Bool(false));
  val reg_status_et   = Reg(resetVal = Bool(false));
  
  val reg_status = Cat(reg_status_sx, reg_status_ux, reg_status_s, reg_status_ps, Bits(0,1), reg_status_ev, reg_status_ef, reg_status_et);
  val rdata = Wire() { Bits() };

  io.status  						:= reg_status;
  io.host.to 						:= Mux(io.host.from_wen, Bits(0, w), reg_tohost);
  io.debug.error_mode  	:= reg_error_mode;
  io.debug.log_control 	:= reg_log_control;
  io.r.data := rdata;

  when (io.host.from_wen) {
    reg_tohost   <== Bits(0, w);
    reg_fromhost <== io.host.from;
  } 
  otherwise {
    when (!io.exception && io.w.en && (io.w.addr === PCR_TOHOST)) {
      reg_tohost   <== io.w.data(w-1, 0);
      reg_fromhost <== Bits(0, w);
    }
  }
  
  when (io.exception && !reg_status_et) {
  	reg_error_mode <== Bool(true);
  }
  
  when (io.exception && reg_status_et) {
  	reg_status_s  <== Bool(true);
  	reg_status_ps <== reg_status_s;
  	reg_status_et <== Bool(false);
  	reg_epc       <== io.pc;
  	reg_cause     <== io.cause;
  }
  
  when (!io.exception && io.eret) {
  	reg_status_s  <== reg_status_ps;
  	reg_status_et <== Bool(true);
  }

  when (!io.exception && !io.eret && io.w.en) {
  	when (io.w.addr === PCR_STATUS) {
	    reg_status_im <== io.w.data(15,8);
      reg_status_sx <== io.w.data(7).toBool;
      reg_status_ux <== io.w.data(6).toBool;
      reg_status_s  <== io.w.data(5).toBool;
      reg_status_ps <== io.w.data(4).toBool;
      reg_status_ev <== HAVE_VEC && io.w.data(2).toBool;
      reg_status_ef <== HAVE_FPU && io.w.data(1).toBool;
      reg_status_et <== io.w.data(0).toBool;
  	}
  	when (io.w.addr === PCR_EPC) 			{ reg_epc      		<== io.w.data(w-1,0); }
  	when (io.w.addr === PCR_BADVADDR) { reg_badvaddr 		<== io.w.data(w-1,0); }
  	when (io.w.addr === PCR_EVEC) 		{ reg_ebase 			<== io.w.data(w-1,0); }
  	when (io.w.addr === PCR_COUNT) 		{ reg_count 			<== io.w.data(w-1,0); }
  	when (io.w.addr === PCR_COMPARE) 	{ reg_compare 		<== io.w.data(w-1,0); }
  	when (io.w.addr === PCR_CAUSE) 		{ reg_cause 			<== io.w.data(4,0); }
  	when (io.w.addr === PCR_LOG) 			{ reg_log_control	<== io.w.data(0).toBool; }
  	when (io.w.addr === PCR_FROMHOST) { reg_fromhost 		<== io.w.data(w-1,0); }
  	when (io.w.addr === PCR_K0) 			{ reg_k0  				<== io.w.data; }
  	when (io.w.addr === PCR_K1) 			{ reg_k1  				<== io.w.data; }
  }

  when (!io.r.en) { rdata <== Bits(0,2*w); }
  switch (io.r.addr) {
    is (PCR_STATUS) 	{ rdata <== Cat(Bits(0,w+16), reg_status_im, reg_status); }
    is (PCR_EPC) 			{ rdata <== Cat(Fill(w, reg_epc(w-1)), reg_epc); }
    is (PCR_BADVADDR) { rdata <== Cat(Fill(w, reg_badvaddr(w-1)), reg_badvaddr); }
    is (PCR_EVEC) 		{ rdata <== Cat(Fill(w, reg_ebase(w-1)), reg_ebase); }
    is (PCR_COUNT) 		{ rdata <== Cat(Fill(w, reg_count(w-1)), reg_count); }
    is (PCR_COMPARE) 	{ rdata <== Cat(Fill(w, reg_compare(w-1)), reg_compare); }
    is (PCR_CAUSE) 		{ rdata <== Cat(Bits(0,w+27), reg_cause); }
    is (PCR_MEMSIZE) 	{ rdata <== Bits("h2000", 2*w); }
    is (PCR_LOG) 	    { rdata <== Cat(Bits(0,63), reg_log_control); }
    is (PCR_FROMHOST) { rdata <== Cat(Fill(w, reg_fromhost(w-1)), reg_fromhost); }
    is (PCR_TOHOST)  	{ rdata <== Cat(Fill(w, reg_tohost(w-1)), reg_tohost); }
    is (PCR_K0) 			{ rdata <== reg_k0; }
    is (PCR_K1) 			{ rdata <== reg_k1; }
    otherwise					{ rdata <== Bits(0,2*w); }
  }
}

class ioReadPort extends Bundle()
{
  val addr = UFix(5, 'input);
  val en   = Bool('input);
  val data = Bits(64, 'output);
}

class ioWritePort extends Bundle()
{
  val addr = UFix(5, 'input);
  val en   = Bool('input);
  val data = Bits(64, 'input);
}

class ioRegfile extends Bundle()
{
  val r0 = new ioReadPort();
  val r1 = new ioReadPort();
  val w0 = new ioWritePort();
  val w1 = new ioWritePort();
}

class rocketDpathRegfile extends Component
{
  override val io = new ioRegfile();
  val regfile = Mem(32, io.w0.en && (io.w0.addr != UFix(0,5)), io.w0.addr, io.w0.data);  
  regfile.write(io.w1.en && (io.w1.addr != UFix(0,5)), io.w1.addr, io.w1.data); 
  io.r0.data := Mux((io.r0.addr === UFix(0, 5)) || !io.r0.en, Bits(0, 64), regfile(io.r0.addr));
  io.r1.data := Mux((io.r1.addr === UFix(0, 5)) || !io.r1.en, Bits(0, 64), regfile(io.r1.addr));
}

}
