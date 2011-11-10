package Top
{

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioDpathBTB extends Bundle()
{
  val current_pc4    = UFix(VADDR_BITS, 'input);
  val hit            = Bool('output);
  val target         = UFix(VADDR_BITS, 'output);
  val wen            = Bool('input);
  val correct_pc4    = UFix(VADDR_BITS, 'input);
  val correct_target = UFix(VADDR_BITS, 'input);
}

// basic direct-mapped branch target buffer
class rocketDpathBTB(entries: Int) extends Component
{
  val io  = new ioDpathBTB();
  
  val addr_bits = ceil(log10(entries)/log10(2)).toInt;
  val idxlsb = 2;
  val idxmsb = idxlsb+addr_bits-1;
  val tagmsb = (VADDR_BITS-idxmsb-1)+(VADDR_BITS-idxlsb)-1;
  val taglsb = (VADDR_BITS-idxlsb);
  
  val rst_lwlr_pf  = Mem(entries, io.wen, io.correct_pc4(idxmsb,idxlsb), UFix(1,1), resetVal = UFix(0,1)); 
  val lwlr_pf      = Mem(entries, io.wen, io.correct_pc4(idxmsb,idxlsb), 
                         Cat(io.correct_pc4(VADDR_BITS-1,idxmsb+1), io.correct_target(VADDR_BITS-1,idxlsb)), resetVal = UFix(0,1));
  val is_val       = rst_lwlr_pf(io.current_pc4(idxmsb,idxlsb));
  val tag_target   = lwlr_pf(io.current_pc4(idxmsb, idxlsb));
  
  io.hit    := (is_val & (tag_target(tagmsb,taglsb) === io.current_pc4(VADDR_BITS-1, idxmsb+1))).toBool;
  io.target := Cat(tag_target(taglsb-1, 0), Bits(0,idxlsb)).toUFix;
  
//   val rst_lwlr_pf  = Mem(entries, io.wen, io.correct_pc4(3, 2), UFix(1, 1), resetVal = UFix(0, 1)); 
//   val lwlr_pf      = Mem(entries, io.wen, io.correct_pc4(3, 2), 
//                          Cat(io.correct_pc4(VADDR_BITS-1,4), io.correct_target(VADDR_BITS-1,2)), resetVal = UFix(0, 1));
//   val is_val       = rst_lwlr_pf(io.current_pc4(3, 2));
//   val tag_target   = lwlr_pf(io.current_pc4(3, 2));
//   
//   io.hit    := (is_val & (tag_target(2*VADDR_BITS-7,VADDR_BITS-2) === io.current_pc4(VADDR_BITS-1, 4))).toBool;
//   io.target := Cat(tag_target(VADDR_BITS-3, 0), Bits(0,2)).toUFix;
}

class ioDpathPCR extends Bundle()
{
  val host  = new ioHost(List("from", "from_wen", "to"));
  val debug = new ioDebug(List("error_mode", "log_control"));
  val r     = new ioReadPort();
  val w     = new ioWritePort();
  
  val status 		= Bits(17, 'output);
  val ptbr      = UFix(PADDR_BITS, 'output);
  val evec      = UFix(VADDR_BITS, 'output);
  val exception = Bool('input);
  val cause 		= UFix(5, 'input);
  val badvaddr_wen = Bool('input);
  val pc    		= UFix(VADDR_BITS, 'input);
  val badvaddr  = UFix(VADDR_BITS, 'input);
  val eret  		= Bool('input);
}

class rocketDpathPCR extends Component
{
  val io = new ioDpathPCR();
  
  val reg_epc      = Reg(resetVal = UFix(0, VADDR_BITS)); 
  val reg_badvaddr = Reg(resetVal = UFix(0, VADDR_BITS)); 
  val reg_ebase    = Reg(resetVal = UFix(0, VADDR_BITS)); 
  val reg_count    = Reg(resetVal = Bits(0, 32)); 
  val reg_compare  = Reg(resetVal = Bits(0, 32)); 
  val reg_cause    = Reg(resetVal = Bits(0, 5));
  val reg_tohost   = Reg(resetVal = Bits(0, 32)); 
  val reg_fromhost = Reg(resetVal = Bits(0, 32));
  val reg_k0       = Reg(resetVal = Bits(0, 64));
  val reg_k1       = Reg(resetVal = Bits(0, 64));
  val reg_ptbr     = Reg(resetVal = UFix(0, PADDR_BITS));
  
  val reg_error_mode  = Reg(resetVal = Bool(false));
  val reg_status_vm   = Reg(resetVal = Bool(false));
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

  io.status  						:= Cat(reg_status_vm, reg_status_im, reg_status);
  io.evec               := reg_ebase;
  io.ptbr               := reg_ptbr;
  io.host.to 						:= Mux(io.host.from_wen, Bits(0,32), reg_tohost);
  io.debug.error_mode  	:= reg_error_mode;
  io.r.data := rdata;

  when (io.host.from_wen) {
    reg_tohost   <== Bits(0,32);
    reg_fromhost <== io.host.from;
  } 
  otherwise {
    when (!io.exception && io.w.en && (io.w.addr === PCR_TOHOST)) {
      reg_tohost   <== io.w.data(31,0);
      reg_fromhost <== Bits(0,32);
    }
  }
  
  when (io.badvaddr_wen) {
    reg_badvaddr 		<== io.badvaddr;
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
  	  reg_status_vm <== io.w.data(16).toBool;
	    reg_status_im <== io.w.data(15,8);
      reg_status_sx <== io.w.data(7).toBool;
      reg_status_ux <== io.w.data(6).toBool;
      reg_status_s  <== io.w.data(5).toBool;
      reg_status_ps <== io.w.data(4).toBool;
      reg_status_ev <== HAVE_VEC && io.w.data(2).toBool;
      reg_status_ef <== HAVE_FPU && io.w.data(1).toBool;
      reg_status_et <== io.w.data(0).toBool;
  	}
  	when (io.w.addr === PCR_EPC) 			{ reg_epc      		<== io.w.data(VADDR_BITS-1,0).toUFix; }
  	when (io.w.addr === PCR_BADVADDR) { reg_badvaddr 		<== io.w.data(VADDR_BITS-1,0).toUFix; }
  	when (io.w.addr === PCR_EVEC) 		{ reg_ebase 			<== io.w.data(VADDR_BITS-1,0).toUFix; }
  	when (io.w.addr === PCR_COUNT) 		{ reg_count 			<== io.w.data(31,0); }
  	when (io.w.addr === PCR_COMPARE) 	{ reg_compare 		<== io.w.data(31,0); }
  	when (io.w.addr === PCR_CAUSE) 		{ reg_cause 			<== io.w.data(4,0); }
  	when (io.w.addr === PCR_FROMHOST) { reg_fromhost 		<== io.w.data(31,0); }
  	when (io.w.addr === PCR_K0) 			{ reg_k0  				<== io.w.data; }
  	when (io.w.addr === PCR_K1) 			{ reg_k1  				<== io.w.data; }
  	when (io.w.addr === PCR_PTBR) 		{ reg_ptbr  			<== Cat(io.w.data(PADDR_BITS-1, PGIDX_BITS), Bits(0, PGIDX_BITS)).toUFix; }
  }

  when (!io.r.en) { rdata <== Bits(0,64); }
  switch (io.r.addr) {
    is (PCR_STATUS) 	{ rdata <== Cat(Bits(0,47), reg_status_vm, reg_status_im, reg_status); }
    is (PCR_EPC) 			{ rdata <== Cat(Fill(64-VADDR_BITS, reg_epc(VADDR_BITS-1)), reg_epc); }
    is (PCR_BADVADDR) { rdata <== Cat(Fill(64-VADDR_BITS, reg_badvaddr(VADDR_BITS-1)), reg_badvaddr); }
    is (PCR_EVEC) 		{ rdata <== Cat(Fill(64-VADDR_BITS, reg_ebase(VADDR_BITS-1)), reg_ebase); }
    is (PCR_COUNT) 		{ rdata <== Cat(Fill(32, reg_count(31)), reg_count); }
    is (PCR_COMPARE) 	{ rdata <== Cat(Fill(32, reg_compare(31)), reg_compare); }
    is (PCR_CAUSE) 		{ rdata <== Cat(Bits(0,59), reg_cause); }
    is (PCR_MEMSIZE) 	{ rdata <== Bits(MEMSIZE_PAGES,64); }
    is (PCR_COREID) 	{ rdata <== Bits(COREID,64); }
    is (PCR_NUMCORES) { rdata <== Bits(NUMCORES,64); }
    is (PCR_FROMHOST) { rdata <== Cat(Fill(32, reg_fromhost(31)), reg_fromhost); }
    is (PCR_TOHOST)  	{ rdata <== Cat(Fill(32, reg_tohost(31)), reg_tohost); }
    is (PCR_K0) 			{ rdata <== reg_k0; }
    is (PCR_K1) 			{ rdata <== reg_k1; }
    is (PCR_PTBR) 		{ rdata <== Cat(Bits(0,64-PADDR_BITS), reg_ptbr); }
    otherwise					{ rdata <== Bits(0,64); }
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
