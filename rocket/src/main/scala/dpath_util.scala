package rocket

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

  val repl_way = LFSR16(io.wen)(log2up(entries)-1,0) // TODO: pseudo-LRU

  var hit_reduction = Bool(false)
  val hit = Wire() { Bool() }
  val update = Wire() { Bool() }
  var update_reduction = Bool(false)
  val mux = (new Mux1H(entries)) { Bits(width = VADDR_BITS) }

  for (i <- 0 until entries) {
    val tag = Reg() { UFix() }
    val target = Reg() { UFix() }
    val valid = Reg(resetVal = Bool(false))
    val my_hit = valid && tag === io.current_pc
    val my_update = valid && tag === io.correct_pc
    val my_clr = io.clr && my_update || io.invalidate
    val my_wen = io.wen && (my_update || !update && UFix(i) === repl_way)

    valid := !my_clr && (valid || my_wen)
    when (my_wen) {
      tag := io.correct_pc
      target := io.correct_target
    }

    hit_reduction = hit_reduction || my_hit
    update_reduction = update_reduction || my_update
    mux.io.sel(i) := my_hit
    mux.io.in(i) := target
  }
  hit := hit_reduction
  update := update_reduction

  io.hit    := hit
  io.target := mux.io.out.toUFix
}

class ioDpathPCR extends Bundle()
{
  val host  = new ioHTIF()
  val debug = new ioDebug(List("error_mode", "log_control"));
  val r     = new ioReadPort();
  val w     = new ioWritePort();
  
  val status     = Bits(17, OUTPUT);
  val ptbr      = UFix(PADDR_BITS, OUTPUT);
  val evec      = UFix(VADDR_BITS, OUTPUT);
  val exception = Bool(INPUT);
  val cause     = UFix(5, INPUT);
  val badvaddr_wen = Bool(INPUT);
  val pc        = UFix(VADDR_BITS+1, INPUT);
  val eret      = Bool(INPUT);
  val ei        = Bool(INPUT);
  val di        = Bool(INPUT);
  val ptbr_wen  = Bool(OUTPUT);
  val irq_timer = Bool(OUTPUT);
  val irq_ipi   = Bool(OUTPUT);
  val vecbank   = Bits(8, OUTPUT)
  val vecbankcnt = UFix(4, OUTPUT)
  val vec_eaddr  = Bits(VADDR_BITS, OUTPUT)
  val vec_exception = Bool(OUTPUT)
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
  val reg_vec_eaddr = Reg() { Bits() }
  val reg_vec_exception = Reg() { Bool() }
  
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

  val ren = io.r.en || io.host.pcr_ren
  val raddr = Mux(io.r.en, io.r.addr, io.host.pcr_addr)
  io.host.pcr_rdata := rdata

  val wen = io.w.en || io.host.pcr_wen
  val waddr = Mux(io.w.en, io.w.addr, io.host.pcr_addr)
  val wdata = Mux(io.w.en, io.w.data, io.host.pcr_wdata)
  io.host.pcr_rdy := Mux(io.host.pcr_wen, !io.w.en, !io.r.en)

  io.ptbr_wen           := reg_status_vm.toBool && wen && (waddr === PCR_PTBR);
  io.status              := Cat(reg_status_vm, reg_status_im, reg_status);
  io.evec               := reg_ebase;
  io.ptbr               := reg_ptbr;
  io.debug.error_mode    := reg_error_mode;
  io.r.data := rdata;

  io.vecbank := reg_vecbank
  var cnt = UFix(0,4)
  for (i <- 0 until 8)
    cnt = cnt + reg_vecbank(i)
  io.vecbankcnt := cnt(3,0)

  io.vec_eaddr := reg_vec_eaddr
  io.vec_exception := reg_vec_exception

  val badvaddr_sign = Mux(io.w.data(VADDR_BITS-1), ~io.w.data(63,VADDR_BITS) === UFix(0), io.w.data(63,VADDR_BITS) != UFix(0))
  when (io.badvaddr_wen) {
    reg_badvaddr     := Cat(badvaddr_sign, io.w.data(VADDR_BITS-1,0)).toUFix;
  }

  when (io.exception) {
    when (!reg_status_et) {
      reg_error_mode := Bool(true)
    }
    .otherwise {
      reg_status_s  := Bool(true);
      reg_status_ps := reg_status_s;
      reg_status_et := Bool(false);
      reg_epc       := io.pc;
      reg_cause     := io.cause;
    }
  }
  
  when (io.di) {
    reg_status_et := Bool(false);
  }    
  
  when (io.ei) {
    reg_status_et := Bool(true);
  }    
  
  when (io.eret) {
    reg_status_s  := reg_status_ps;
    reg_status_et := Bool(true);
  }
  
  when (reg_count === reg_compare) {
    r_irq_timer := Bool(true);
  }
  reg_count := reg_count + UFix(1);

  io.irq_timer := r_irq_timer;
  io.irq_ipi := r_irq_ipi;

  when (wen) {
    when (waddr === PCR_STATUS) {
      reg_status_vm := wdata(SR_VM).toBool;
      reg_status_im := wdata(15,8);
      reg_status_sx := wdata(SR_SX).toBool;
      reg_status_ux := wdata(SR_UX).toBool;
      reg_status_s  := wdata(SR_S).toBool;
      reg_status_ps := wdata(SR_PS).toBool;
      reg_status_ev := Bool(HAVE_VEC) && wdata(SR_EV).toBool;
      reg_status_ef := Bool(HAVE_FPU) && wdata(SR_EF).toBool;
      reg_status_ec := Bool(HAVE_RVC) && wdata(SR_EC).toBool;
      reg_status_et := wdata(SR_ET).toBool;
    }
    when (waddr === PCR_EPC)      { reg_epc          := wdata(VADDR_BITS,0).toUFix; }
    when (waddr === PCR_BADVADDR) { reg_badvaddr     := wdata(VADDR_BITS,0).toUFix; }
    when (waddr === PCR_EVEC)     { reg_ebase       := wdata(VADDR_BITS-1,0).toUFix; }
    when (waddr === PCR_COUNT)    { reg_count       := wdata(31,0).toUFix; }
    when (waddr === PCR_COMPARE)  { reg_compare     := wdata(31,0).toUFix; r_irq_timer := Bool(false); }
    when (waddr === PCR_CAUSE)    { reg_cause       := wdata(4,0); }
    when (waddr === PCR_TOHOST)   { reg_tohost       := wdata; reg_fromhost := Bits(0) }
    when (waddr === PCR_FROMHOST) { reg_fromhost     := wdata; reg_tohost := Bits(0) }
    when (waddr === PCR_SEND_IPI) { r_irq_ipi        := Bool(true); }
    when (waddr === PCR_CLR_IPI)  { r_irq_ipi        := Bool(false); }
    when (waddr === PCR_K0)       { reg_k0          := wdata; }
    when (waddr === PCR_K1)       { reg_k1          := wdata; }
    when (waddr === PCR_PTBR)     { reg_ptbr        := Cat(wdata(PADDR_BITS-1, PGIDX_BITS), Bits(0, PGIDX_BITS)).toUFix; }
    when (waddr === PCR_VECBANK)  { reg_vecbank     := wdata(7,0) }
    when (waddr === PCR_VEC_EADDR) { reg_vec_eaddr    := wdata(VADDR_BITS,0) }
    when (waddr === PCR_VEC_XCPT) { reg_vec_exception:= wdata(0) }
  }

  rdata := Bits(0, 64)
  when (ren) {
    switch (raddr) {
      is (PCR_STATUS)   { rdata := Cat(Bits(0,47), reg_status_vm, reg_status_im, reg_status); }
      is (PCR_EPC)      { rdata := Cat(Fill(64-VADDR_BITS-1, reg_epc(VADDR_BITS)), reg_epc); }
      is (PCR_BADVADDR) { rdata := Cat(Fill(64-VADDR_BITS-1, reg_badvaddr(VADDR_BITS)), reg_badvaddr); }
      is (PCR_EVEC)     { rdata := Cat(Fill(64-VADDR_BITS, reg_ebase(VADDR_BITS-1)), reg_ebase); }
      is (PCR_COUNT)    { rdata := Cat(Fill(32, reg_count(31)), reg_count); }
      is (PCR_COMPARE)  { rdata := Cat(Fill(32, reg_compare(31)), reg_compare); }
      is (PCR_CAUSE)    { rdata := Cat(Bits(0,59), reg_cause); }
      is (PCR_COREID)   { rdata := Bits(COREID,64); }
      is (PCR_FROMHOST) { rdata := reg_fromhost; }
      is (PCR_TOHOST)   { rdata := reg_tohost; }
      is (PCR_K0)       { rdata := reg_k0; }
      is (PCR_K1)       { rdata := reg_k1; }
      is (PCR_PTBR)     { rdata := Cat(Bits(0,64-PADDR_BITS), reg_ptbr); }
      is (PCR_VECBANK)  { rdata := Cat(Bits(0, 56), reg_vecbank) }
    }
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

  val regfile = Mem(32){ Bits(width=64) }
  regfile.setReadLatency(0);
  regfile.setTarget('inst);
  regfile.write(io.w0.addr, io.w0.data, io.w0.en);
  io.r0.data := Mux((io.r0.addr === UFix(0, 5)) || !io.r0.en, Bits(0, 64), regfile(io.r0.addr));
  io.r1.data := Mux((io.r1.addr === UFix(0, 5)) || !io.r1.en, Bits(0, 64), regfile(io.r1.addr));
}
