package Top
{

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioDpathBTB extends Bundle()
{
  val current_pc4    = UFix(VADDR_BITS, INPUT);
  val hit            = Bool(OUTPUT);
  val target         = UFix(VADDR_BITS, OUTPUT);
  val wen            = Bool(INPUT);
  val clr            = Bool(INPUT);
  val correct_pc4    = UFix(VADDR_BITS, INPUT);
  val correct_target = UFix(VADDR_BITS, INPUT);
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
  
  val vb_array = Mem(entries, io.wen || io.clr, io.correct_pc4(idxmsb,idxlsb), !io.clr, resetVal = Bool(false)); 
  val tag_target_array = Mem(entries, io.wen, io.correct_pc4(idxmsb,idxlsb), 
                             Cat(io.correct_pc4(VADDR_BITS-1,idxmsb+1), io.correct_target(VADDR_BITS-1,idxlsb)))
  val is_val       = vb_array(io.current_pc4(idxmsb,idxlsb));
  val tag_target   = tag_target_array(io.current_pc4(idxmsb, idxlsb));
  
  io.hit    := is_val && (tag_target(tagmsb,taglsb) === io.current_pc4(VADDR_BITS-1, idxmsb+1));
  io.target := Cat(tag_target(taglsb-1, 0), Bits(0,idxlsb)).toUFix;
}

class ioDpathPCR extends Bundle()
{
  val host  = new ioHost(List("from", "from_wen", "to"));
  val debug = new ioDebug(List("error_mode", "log_control"));
  val r     = new ioReadPort();
  val w     = new ioWritePort();
  
  val status 		= Bits(17, OUTPUT);
  val ptbr      = UFix(PADDR_BITS, OUTPUT);
  val evec      = UFix(VADDR_BITS, OUTPUT);
  val exception = Bool(INPUT);
  val cause 		= UFix(5, INPUT);
  val badvaddr_wen = Bool(INPUT);
  val pc    		= UFix(VADDR_BITS, INPUT);
  val eret  		= Bool(INPUT);
  val ei        = Bool(INPUT);
  val di        = Bool(INPUT);
  val ptbr_wen  = Bool(OUTPUT);
  val irq_timer = Bool(OUTPUT);
  val irq_ipi   = Bool(OUTPUT);
  val console_data = Bits(8, OUTPUT);
  val console_val  = Bool(OUTPUT);
}

class rocketDpathPCR extends Component
{
  val io = new ioDpathPCR();
  
  val reg_epc      = Reg() { UFix() };
  val reg_badvaddr = Reg() { UFix() };
  val reg_ebase    = Reg() { UFix() };
  val reg_count    = Reg() { UFix() };
  val reg_compare  = Reg() { UFix() };
  val reg_cause    = Reg() { Bits() };
  val reg_tohost   = Reg(resetVal = Bits(0, 64)); 
  val reg_fromhost = Reg(resetVal = Bits(0, 64));
  val reg_k0       = Reg() { Bits() };
  val reg_k1       = Reg() { Bits() };
  val reg_ptbr     = Reg() { UFix() };
  
  val reg_error_mode  = Reg(resetVal = Bool(false));
  val reg_status_vm   = Reg(resetVal = Bool(false));
  val reg_status_im   = Reg(resetVal = Bits(0,8));
  val reg_status_sx   = Reg(resetVal = Bool(true));
  val reg_status_ux   = Reg(resetVal = Bool(true));
  val reg_status_ec   = Reg(resetVal = Bool(false));
  val reg_status_ef   = Reg(resetVal = Bool(false));
  val reg_status_ev   = Reg(resetVal = Bool(false));
  val reg_status_s    = Reg(resetVal = Bool(true));
  val reg_status_ps   = Reg(resetVal = Bool(false));
  val reg_status_et   = Reg(resetVal = Bool(false));
  
  val r_irq_timer = Reg(resetVal = Bool(false));
  val r_irq_ipi   = Reg(resetVal = Bool(false));
  
  val reg_status = Cat(reg_status_sx, reg_status_ux, reg_status_s, reg_status_ps, reg_status_ec, reg_status_ev, reg_status_ef, reg_status_et);
  val rdata = Wire() { Bits() };

  io.ptbr_wen           := reg_status_vm.toBool && !io.exception && io.w.en && (io.w.addr === PCR_PTBR);
  io.status  						:= Cat(reg_status_vm, reg_status_im, reg_status);
  io.evec               := reg_ebase;
  io.ptbr               := reg_ptbr;
  io.host.to 						:= Mux(io.host.from_wen, Bits(0), reg_tohost);
  io.debug.error_mode  	:= reg_error_mode;
  io.r.data := rdata;

  val console_wen = !io.exception && io.w.en && (io.w.addr === PCR_CONSOLE);
  io.console_data := Mux(console_wen, io.w.data(7,0), Bits(0,8));
  io.console_val := console_wen;

  when (io.host.from_wen) {
    reg_tohost   <== Bits(0);
    reg_fromhost <== io.host.from;
  } 
  otherwise {
    when (!io.exception && io.w.en && (io.w.addr === PCR_TOHOST)) {
      reg_tohost   <== io.w.data;
      reg_fromhost <== Bits(0);
    }
  }

  when (io.badvaddr_wen) {
    reg_badvaddr 		<== io.w.data.toUFix;
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
  
  when (!io.exception && io.di) {
  	reg_status_et <== Bool(false);
  }    
  
  when (!io.exception && io.ei) {
  	reg_status_et <== Bool(true);
  }    
  
  when (!io.exception && io.eret) {
  	reg_status_s  <== reg_status_ps;
  	reg_status_et <== Bool(true);
  }

  when (!io.exception && !io.eret && io.w.en) {
  	when (io.w.addr === PCR_STATUS) {
  	  reg_status_vm <== io.w.data(SR_VM).toBool;
	    reg_status_im <== io.w.data(15,8);
      reg_status_sx <== io.w.data(SR_SX).toBool;
      reg_status_ux <== io.w.data(SR_UX).toBool;
      reg_status_s  <== io.w.data(SR_S).toBool;
      reg_status_ps <== io.w.data(SR_PS).toBool;
      reg_status_ev <== HAVE_VEC && io.w.data(SR_EV).toBool;
      reg_status_ef <== HAVE_FPU && io.w.data(SR_EF).toBool;
      reg_status_ec <== HAVE_RVC && io.w.data(SR_EC).toBool;
      reg_status_et <== io.w.data(SR_ET).toBool;
  	}
  	when (io.w.addr === PCR_EPC) 			{ reg_epc      		<== io.w.data(VADDR_BITS-1,0).toUFix; }
  	when (io.w.addr === PCR_BADVADDR) { reg_badvaddr 		<== io.w.data(VADDR_BITS-1,0).toUFix; }
  	when (io.w.addr === PCR_EVEC) 		{ reg_ebase 			<== io.w.data(VADDR_BITS-1,0).toUFix; }
  	when (io.w.addr === PCR_COUNT) 		{ reg_count 			<== io.w.data(31,0).toUFix; }
  	when (io.w.addr === PCR_COMPARE) 	{ reg_compare 		<== io.w.data(31,0).toUFix; r_irq_timer <== Bool(false); }
  	when (io.w.addr === PCR_CAUSE) 		{ reg_cause 			<== io.w.data(4,0); }
  	when (io.w.addr === PCR_FROMHOST) { reg_fromhost 		<== io.w.data; }
  	when (io.w.addr === PCR_SEND_IPI) { r_irq_ipi  			<== Bool(true); }
  	when (io.w.addr === PCR_CLR_IPI)  { r_irq_ipi  			<== Bool(false); }
  	when (io.w.addr === PCR_K0) 			{ reg_k0  				<== io.w.data; }
  	when (io.w.addr === PCR_K1) 			{ reg_k1  				<== io.w.data; }
  	when (io.w.addr === PCR_PTBR) 		{ reg_ptbr  			<== Cat(io.w.data(PADDR_BITS-1, PGIDX_BITS), Bits(0, PGIDX_BITS)).toUFix; }
  }

  reg_count <== reg_count + UFix(1);
  when (reg_count === reg_compare) {
    r_irq_timer <== Bool(true);
  }
  io.irq_timer := r_irq_timer;
  io.irq_ipi := r_irq_ipi;

  when (!io.r.en) { rdata <== Bits(0,64); }
  switch (io.r.addr) {
    is (PCR_STATUS) 	{ rdata <== Cat(Bits(0,47), reg_status_vm, reg_status_im, reg_status); }
    is (PCR_EPC) 			{ rdata <== Cat(Fill(64-VADDR_BITS, reg_epc(VADDR_BITS-1)), reg_epc); }
    is (PCR_BADVADDR) { rdata <== Cat(Fill(64-VADDR_BITS, reg_badvaddr(VADDR_BITS-1)), reg_badvaddr); }
    is (PCR_EVEC) 		{ rdata <== Cat(Fill(64-VADDR_BITS, reg_ebase(VADDR_BITS-1)), reg_ebase); }
    is (PCR_COUNT) 		{ rdata <== Cat(Fill(32, reg_count(31)), reg_count); }
    is (PCR_COMPARE) 	{ rdata <== Cat(Fill(32, reg_compare(31)), reg_compare); }
    is (PCR_CAUSE) 		{ rdata <== Cat(Bits(0,59), reg_cause); }
    is (PCR_COREID) 	{ rdata <== Bits(COREID,64); }
    is (PCR_FROMHOST) { rdata <== reg_fromhost; }
    is (PCR_TOHOST)  	{ rdata <== reg_tohost; }
    is (PCR_K0) 			{ rdata <== reg_k0; }
    is (PCR_K1) 			{ rdata <== reg_k1; }
    is (PCR_PTBR) 		{ rdata <== Cat(Bits(0,64-PADDR_BITS), reg_ptbr); }
    otherwise					{ rdata <== Bits(0,64); }
  }
}

class ioReadPort extends Bundle()
{
  val addr = UFix(5, INPUT);
  val en   = Bool(INPUT);
  val data = Bits(64, OUTPUT);
}

class ioWritePort extends Bundle()
{
  val addr = UFix(5, INPUT);
  val en   = Bool(INPUT);
  val data = Bits(64, INPUT);
}

class ioRegfile extends Bundle()
{
  val r0 = new ioReadPort();
  val r1 = new ioReadPort();
  val w0 = new ioWritePort();
}

class rocketDpathRegfile extends Component
{
  override val io = new ioRegfile();

  // FIXME: remove the first "if" case once Mem4 C backend bug is fixed
  if (SRAM_READ_LATENCY == 0) {
    val regfile = Mem(32, io.w0.en && (io.w0.addr != UFix(0,5)), io.w0.addr, io.w0.data);  
    io.r0.data := Mux((io.r0.addr === UFix(0, 5)) || !io.r0.en, Bits(0, 64), regfile(io.r0.addr));
    io.r1.data := Mux((io.r1.addr === UFix(0, 5)) || !io.r1.en, Bits(0, 64), regfile(io.r1.addr));
  }
  else {
    val regfile = Mem4(32, io.w0.data);
    regfile.setReadLatency(0);
    regfile.setTarget('inst);
    regfile.write(io.w0.addr, io.w0.data, io.w0.en);
    io.r0.data := Mux((io.r0.addr === UFix(0, 5)) || !io.r0.en, Bits(0, 64), regfile(io.r0.addr));
    io.r1.data := Mux((io.r1.addr === UFix(0, 5)) || !io.r1.en, Bits(0, 64), regfile(io.r1.addr));
  }
}

}
