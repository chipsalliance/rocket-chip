package rocket

import Chisel._
import Node._
import Constants._
import scala.math._
import Util._

class ioDpathBTB extends Bundle()
{
  val current_pc     = UFix(INPUT, VADDR_BITS);
  val hit            = Bool(OUTPUT);
  val target         = UFix(OUTPUT, VADDR_BITS);
  val wen            = Bool(INPUT);
  val clr            = Bool(INPUT);
  val invalidate     = Bool(INPUT);
  val correct_pc     = UFix(INPUT, VADDR_BITS);
  val correct_target = UFix(INPUT, VADDR_BITS);
}

// fully-associative branch target buffer
class rocketDpathBTB(entries: Int) extends Component
{
  val io = new ioDpathBTB();

  val repl_way = LFSR16(io.wen)(log2Up(entries)-1,0) // TODO: pseudo-LRU

  var hit_reduction = Bool(false)
  val hit = Bool()
  val update = Bool()
  var update_reduction = Bool(false)
  val hits = Vec(entries) { Bool() }
  val updates = Vec(entries) { Bool() }
  val targets = Vec(entries) { Reg() { UFix() } }
  val anyUpdate = updates.toBits.orR

  for (i <- 0 until entries) {
    val tag = Reg() { UFix() }
    val valid = Reg(resetVal = Bool(false))
    hits(i) := valid && tag === io.current_pc
    updates(i) := valid && tag === io.correct_pc

    when (io.wen && (updates(i) || !anyUpdate && UFix(i) === repl_way)) {
      valid := Bool(false)
      when (!io.clr) {
        valid := Bool(true)
        tag := io.correct_pc
        targets(i) := io.correct_target
      }
    }
  }

  io.hit    := hits.toBits.orR
  io.target := Mux1H(hits, targets)
}

class ioDpathPCR(implicit conf: RocketConfiguration) extends Bundle
{
  val host  = new ioHTIF(conf.ntiles)
  val r     = new ioReadPort(32, 64)
  val w     = new ioWritePort(32, 64)
  
  val status     = Bits(OUTPUT, 32);
  val ptbr      = UFix(OUTPUT, PADDR_BITS);
  val evec      = UFix(OUTPUT, VADDR_BITS);
  val exception = Bool(INPUT);
  val cause     = UFix(INPUT, 6);
  val badvaddr_wen = Bool(INPUT);
  val vec_irq_aux = Bits(INPUT, 64)
  val vec_irq_aux_wen = Bool(INPUT)
  val pc        = UFix(INPUT, VADDR_BITS+1);
  val eret      = Bool(INPUT);
  val ei        = Bool(INPUT);
  val di        = Bool(INPUT);
  val ptbr_wen  = Bool(OUTPUT);
  val irq_timer = Bool(OUTPUT);
  val irq_ipi   = Bool(OUTPUT);
  val replay    = Bool(OUTPUT)
  val vecbank   = Bits(OUTPUT, 8)
  val vecbankcnt = UFix(OUTPUT, 4)
  val vec_appvl = UFix(INPUT, 12)
  val vec_nxregs = UFix(INPUT, 6)
  val vec_nfregs = UFix(INPUT, 6)
}

class rocketDpathPCR(implicit conf: RocketConfiguration) extends Component
{
  val io = new ioDpathPCR
  
  val reg_epc = Reg{Fix()}
  val reg_badvaddr = Reg{Fix()}
  val reg_ebase = Reg{Fix()}
  val reg_count = WideCounter(32)
  val reg_compare  = Reg() { UFix() };
  val reg_cause    = Reg() { Bits() };
  val reg_tohost   = Reg(resetVal = Bits(0, 64)); 
  val reg_fromhost = Reg(resetVal = Bits(0, 64));
  val reg_coreid   = Reg() { Bits() }
  val reg_k0       = Reg() { Bits() };
  val reg_k1       = Reg() { Bits() };
  val reg_ptbr     = Reg() { UFix() };
  val reg_vecbank  = Reg(resetVal = Bits("b1111_1111", 8))
  
  val reg_error_mode  = Reg(resetVal = Bool(false));
  val reg_status_vm   = Reg(resetVal = Bool(false));
  val reg_status_im   = Reg(resetVal = Bits(0,SR_IM_WIDTH));
  val reg_status_sx   = Reg(resetVal = Bool(true));
  val reg_status_ux   = Reg(resetVal = Bool(true));
  val reg_status_ec   = Reg(resetVal = Bool(false));
  val reg_status_ef   = Reg(resetVal = Bool(false));
  val reg_status_ev   = Reg(resetVal = Bool(false));
  val reg_status_s    = Reg(resetVal = Bool(true));
  val reg_status_ps   = Reg(resetVal = Bool(false));
  val reg_status_et   = Reg(resetVal = Bool(false));
  
  val r_irq_timer = Reg(resetVal = Bool(false));
  val r_irq_ipi   = Reg(resetVal = Bool(true))
  
  val rdata = Bits();

  val raddr = Mux(io.r.en, io.r.addr, io.host.pcr_req.bits.addr(4,0))
  io.host.pcr_rep.valid := io.host.pcr_req.fire()
  io.host.pcr_rep.bits := rdata

  val wen = io.w.en || !io.r.en && io.host.pcr_req.valid && io.host.pcr_req.bits.rw
  val waddr = Mux(io.w.en, io.w.addr, io.host.pcr_req.bits.addr)
  val wdata = Mux(io.w.en, io.w.data, io.host.pcr_req.bits.data)
  io.host.pcr_req.ready := !io.w.en && !io.r.en

  io.ptbr_wen           := reg_status_vm.toBool && wen && (waddr === PCR_PTBR);
  io.status             := Cat(reg_status_im, Bits(0,7), reg_status_vm, reg_status_sx, reg_status_ux, reg_status_s, reg_status_ps, reg_status_ec, reg_status_ev, reg_status_ef, reg_status_et);
  io.evec               := Mux(io.exception, reg_ebase, reg_epc).toUFix
  io.ptbr               := reg_ptbr;
  io.host.debug.error_mode    := reg_error_mode;
  io.r.data := rdata;

  io.vecbank := reg_vecbank
  var cnt = UFix(0,4)
  for (i <- 0 until 8)
    cnt = cnt + reg_vecbank(i)
  io.vecbankcnt := cnt(3,0)

  val badvaddr_sign = Mux(io.w.data(VADDR_BITS-1), ~io.w.data(63,VADDR_BITS) === UFix(0), io.w.data(63,VADDR_BITS) != UFix(0))
  when (io.badvaddr_wen) {
    reg_badvaddr     := Cat(badvaddr_sign, io.w.data(VADDR_BITS-1,0)).toUFix;
  }
  when (io.vec_irq_aux_wen) {
    reg_badvaddr := io.vec_irq_aux.toUFix
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
  
  when (io.eret) {
    reg_status_s  := reg_status_ps;
    reg_status_et := Bool(true);
  }
  
  when (reg_count === reg_compare) {
    r_irq_timer := Bool(true);
  }

  io.irq_timer := r_irq_timer;
  io.irq_ipi := r_irq_ipi;
  io.host.ipi_req.valid := io.w.en && io.w.addr === PCR_SEND_IPI
  io.host.ipi_req.bits := io.w.data
  io.replay := io.host.ipi_req.valid && !io.host.ipi_req.ready

  when (io.host.pcr_req.fire() && !io.host.pcr_req.bits.rw && io.host.pcr_req.bits.addr === PCR_TOHOST) { reg_tohost := UFix(0) }

  when (wen) {
    when (waddr === PCR_STATUS) {
      reg_status_vm := wdata(SR_VM).toBool;
      reg_status_im := wdata(SR_IM_WIDTH+SR_IM,SR_IM);
      reg_status_sx := wdata(SR_S64).toBool;
      reg_status_ux := wdata(SR_U64).toBool;
      reg_status_s  := wdata(SR_S).toBool;
      reg_status_ps := wdata(SR_PS).toBool;
      reg_status_ev := Bool(conf.vec) && wdata(SR_EV).toBool;
      reg_status_ef := Bool(conf.fpu) && wdata(SR_EF).toBool;
      reg_status_ec := Bool(conf.rvc) && wdata(SR_EC).toBool;
      reg_status_et := wdata(SR_ET).toBool;
    }
    when (waddr === PCR_EPC)      { reg_epc := wdata(VADDR_BITS,0).toFix }
    when (waddr === PCR_EVEC)     { reg_ebase := wdata(VADDR_BITS-1,0).toUFix; }
    when (waddr === PCR_COUNT)    { reg_count := wdata.toUFix }
    when (waddr === PCR_COMPARE)  { reg_compare := wdata(31,0).toUFix; r_irq_timer := Bool(false); }
    when (waddr === PCR_COREID)   { reg_coreid := wdata(15,0) }
    when (waddr === PCR_FROMHOST) { when (reg_fromhost === UFix(0) || io.w.en) { reg_fromhost := wdata } }
    when (waddr === PCR_TOHOST)   { when (reg_tohost === UFix(0)) { reg_tohost := wdata } }
    when (waddr === PCR_CLR_IPI)  { r_irq_ipi := wdata(0) }
    when (waddr === PCR_K0)       { reg_k0 := wdata; }
    when (waddr === PCR_K1)       { reg_k1 := wdata; }
    when (waddr === PCR_PTBR)     { reg_ptbr := Cat(wdata(PADDR_BITS-1, PGIDX_BITS), Bits(0, PGIDX_BITS)).toUFix; }
    when (waddr === PCR_VECBANK)  { reg_vecbank:= wdata(7,0) }
  }

  io.host.ipi_rep.ready := Bool(true)
  when (io.host.ipi_rep.valid) { r_irq_ipi := Bool(true) }

  rdata := io.status // raddr === PCR_STATUS
  switch (raddr) {
    is (PCR_EPC)      { rdata := reg_epc }
    is (PCR_BADVADDR) { rdata := reg_badvaddr }
    is (PCR_EVEC)     { rdata := reg_ebase }
    is (PCR_COUNT)    { rdata := reg_count }
    is (PCR_COMPARE)  { rdata := reg_compare }
    is (PCR_CAUSE)    { rdata := reg_cause(5) << 63 | reg_cause(4,0) }
    is (PCR_COREID)   { rdata := reg_coreid }
    is (PCR_IMPL)     { rdata := Bits(2) }
    is (PCR_FROMHOST) { rdata := reg_fromhost; }
    is (PCR_TOHOST)   { rdata := reg_tohost; }
    is (PCR_K0)       { rdata := reg_k0; }
    is (PCR_K1)       { rdata := reg_k1; }
    is (PCR_PTBR)     { rdata := reg_ptbr }
    is (PCR_VECBANK)  { rdata := Cat(Bits(0, 56), reg_vecbank) }
    is (PCR_VECCFG)   { rdata := Cat(Bits(0, 40), io.vec_nfregs, io.vec_nxregs, io.vec_appvl) }
  }
}

class ioReadPort(d: Int, w: Int) extends Bundle
{
  val addr = UFix(INPUT, log2Up(d))
  val en   = Bool(INPUT)
  val data = Bits(OUTPUT, w)
  override def clone = new ioReadPort(d, w).asInstanceOf[this.type]
}

class ioWritePort(d: Int, w: Int) extends Bundle
{
  val addr = UFix(INPUT, log2Up(d))
  val en   = Bool(INPUT)
  val data = Bits(INPUT, w)
  override def clone = new ioWritePort(d, w).asInstanceOf[this.type]
}
