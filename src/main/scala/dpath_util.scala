package Top
{

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioDpathBTB extends Bundle()
{
  val current_pc     = UFix(VADDR_BITS, INPUT);
  val hit            = Bool(OUTPUT);
  val target         = UFix(VADDR_BITS, OUTPUT);
  val wen            = Bool(INPUT);
  val clr            = Bool(INPUT);
  val invalidate     = Bool(INPUT);
  val correct_pc     = UFix(VADDR_BITS, INPUT);
  val correct_target = UFix(VADDR_BITS, INPUT);
}

// fully-associative branch target buffer
class rocketDpathBTB(entries: Int) extends Component
{
  val io = new ioDpathBTB();

  val do_update = io.wen || io.clr
  val expected_tag = Mux(do_update, io.correct_pc, io.current_pc)

  val repl_way = LFSR16(io.wen)(log2up(entries)-1,0) // TODO: pseudo-LRU

  var hit_reduction = Bool(false)
  val hit = Wire() { Bool() }
  val mux = (new Mux1H(entries)) { Bits(width = VADDR_BITS) }

  for (i <- 0 until entries) {
    val tag = Reg() { UFix() }
    val target = Reg() { UFix() }
    val valid = Reg(resetVal = Bool(false))
    val my_hit = valid && tag === expected_tag
    val my_clr = io.clr && my_hit || io.invalidate
    val my_wen = io.wen && (my_hit || !hit && UFix(i) === repl_way)

    when (my_clr) {
      valid <== Bool(false)
    }
    when (my_wen) {
      valid <== Bool(true)
      tag <== io.correct_pc
      target <== io.correct_target
    }

    hit_reduction = hit_reduction || my_hit
    mux.io.sel(i) := my_hit
    mux.io.in(i) := target
  }
  hit := hit_reduction

  io.hit    := hit
  io.target := mux.io.out.toUFix
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
  val pc    		= UFix(VADDR_BITS+1, INPUT);
  val eret  		= Bool(INPUT);
  val ei        = Bool(INPUT);
  val di        = Bool(INPUT);
  val ptbr_wen  = Bool(OUTPUT);
  val irq_timer = Bool(OUTPUT);
  val irq_ipi   = Bool(OUTPUT);
  val console_data = Bits(8, OUTPUT);
  val console_val  = Bool(OUTPUT);
  val vecbank   = Bits(8, OUTPUT)
  val vecbankcnt = UFix(4, OUTPUT)
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
  val reg_vecbank  = Reg(resetVal = Bits("b1111_1111", 8))
  
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

  io.vecbank := reg_vecbank
  var cnt = UFix(0)
  for (i <- 0 until 8)
    cnt = cnt + reg_vecbank(i)
  io.vecbankcnt := cnt(3,0)

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

  val badvaddr_sign = Mux(io.w.data(VADDR_BITS-1), ~io.w.data(63,VADDR_BITS) === UFix(0), io.w.data(63,VADDR_BITS) != UFix(0))
  when (io.badvaddr_wen) {
    reg_badvaddr 		<== Cat(badvaddr_sign, io.w.data(VADDR_BITS-1,0)).toUFix;
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
      reg_status_ev <== Bool(HAVE_VEC) && io.w.data(SR_EV).toBool;
      reg_status_ef <== Bool(HAVE_FPU) && io.w.data(SR_EF).toBool;
      reg_status_ec <== Bool(HAVE_RVC) && io.w.data(SR_EC).toBool;
      reg_status_et <== io.w.data(SR_ET).toBool;
  	}
  	when (io.w.addr === PCR_EPC) 			{ reg_epc      		<== io.w.data(VADDR_BITS,0).toUFix; }
  	when (io.w.addr === PCR_BADVADDR) { reg_badvaddr 		<== io.w.data(VADDR_BITS,0).toUFix; }
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
    when (io.w.addr === PCR_VECBANK)  { reg_vecbank     <== io.w.data(7,0) }
  }

  otherwise {
    reg_count <== reg_count + UFix(1);
  }
  when (reg_count === reg_compare) {
    r_irq_timer <== Bool(true);
  }
  io.irq_timer := r_irq_timer;
  io.irq_ipi := r_irq_ipi;

  when (!io.r.en) { rdata <== Bits(0,64); }
  switch (io.r.addr) {
    is (PCR_STATUS) 	{ rdata <== Cat(Bits(0,47), reg_status_vm, reg_status_im, reg_status); }
    is (PCR_EPC) 			{ rdata <== Cat(Fill(64-VADDR_BITS-1, reg_epc(VADDR_BITS)), reg_epc); }
    is (PCR_BADVADDR) { rdata <== Cat(Fill(64-VADDR_BITS-1, reg_badvaddr(VADDR_BITS)), reg_badvaddr); }
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
    is (PCR_VECBANK)  { rdata <== Cat(Bits(0, 56), reg_vecbank) }
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

  val regfile = Mem4(32, io.w0.data);
  regfile.setReadLatency(0);
  regfile.setTarget('inst);
  regfile.write(io.w0.addr, io.w0.data, io.w0.en);
  io.r0.data := Mux((io.r0.addr === UFix(0, 5)) || !io.r0.en, Bits(0, 64), regfile(io.r0.addr));
  io.r1.data := Mux((io.r1.addr === UFix(0, 5)) || !io.r1.en, Bits(0, 64), regfile(io.r1.addr));
}

}
