package rocket

import Chisel._
import Util._
import Node._
import uncore.constants.AddressConstants._
import scala.math._

class DpathBTBIO extends Bundle
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
  val io = new DpathBTBIO

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

class Status extends Bundle {
  val ip = Bits(width = 8)
  val im = Bits(width = 8)
  val zero = Bits(width = 7)
  val vm = Bool()
  val s64 = Bool()
  val u64 = Bool()
  val s = Bool()
  val ps = Bool()
  val ec = Bool()
  val ev = Bool()
  val ef = Bool()
  val et = Bool()
}

object PCR
{
  // commands
  val SZ = 3
  val X = Bits("b???", 3)
  val N = Bits(0,3)
  val F = Bits(1,3) // mfpcr
  val T = Bits(4,3) // mtpcr
  val C = Bits(6,3) // clearpcr
  val S = Bits(7,3) // setpcr

  // regs
  val STATUS   =  0
  val EPC      =  1
  val BADVADDR =  2
  val EVEC     =  3
  val COUNT    =  4
  val COMPARE  =  5
  val CAUSE    =  6
  val PTBR     =  7
  val SEND_IPI =  8
  val CLR_IPI  =  9
  val COREID   = 10
  val IMPL     = 11
  val K0       = 12
  val K1       = 13
  val VECBANK  = 18
  val VECCFG   = 19
  val STATS    = 28
  val RESET    = 29
  val TOHOST   = 30
  val FROMHOST = 31
}

class PCR(implicit conf: RocketConfiguration) extends Component
{
  val io = new Bundle {
    val host = new HTIFIO(conf.lnConf.nClients)
    val rw = new Bundle {
      val addr = UFix(INPUT, log2Up(conf.nxpr))
      val cmd = Bits(INPUT, PCR.SZ)
      val rdata = Bits(OUTPUT, conf.xprlen)
      val wdata = Bits(INPUT, conf.xprlen)
    }
    
    val status = new Status().asOutput
    val ptbr = UFix(OUTPUT, PADDR_BITS)
    val evec = UFix(OUTPUT, VADDR_BITS)
    val exception = Bool(INPUT)
    val cause = UFix(INPUT, 6)
    val badvaddr_wen = Bool(INPUT)
    val vec_irq_aux = Bits(INPUT, conf.xprlen)
    val vec_irq_aux_wen = Bool(INPUT)
    val pc = UFix(INPUT, VADDR_BITS+1)
    val eret = Bool(INPUT)
    val ei = Bool(INPUT)
    val di = Bool(INPUT)
    val ptbr_wen = Bool(OUTPUT)
    val irq_timer = Bool(OUTPUT)
    val irq_ipi = Bool(OUTPUT)
    val replay = Bool(OUTPUT)
    val vecbank = Bits(OUTPUT, 8)
    val vecbankcnt = UFix(OUTPUT, 4)
    val stats = Bool(OUTPUT)
    val vec_appvl = UFix(INPUT, 12)
    val vec_nxregs = UFix(INPUT, 6)
    val vec_nfregs = UFix(INPUT, 6)
  }
  import PCR._
 
  val reg_epc = Reg{Bits(width = conf.xprlen)}
  val reg_badvaddr = Reg{Bits(width = conf.xprlen)}
  val reg_ebase = Reg{Bits(width = conf.xprlen)}
  val reg_count = WideCounter(32)
  val reg_compare = Reg{Bits(width = 32)}
  val reg_cause = Reg{Bits(width = io.cause.getWidth)}
  val reg_tohost = Reg(resetVal = Bits(0, conf.xprlen))
  val reg_fromhost = Reg(resetVal = Bits(0, conf.xprlen))
  val reg_coreid = Reg{Bits(width = 16)}
  val reg_k0 = Reg{Bits(width = conf.xprlen)}
  val reg_k1 = Reg{Bits(width = conf.xprlen)}
  val reg_ptbr = Reg{UFix(width = PADDR_BITS)}
  val reg_vecbank = Reg(resetVal = Fix(-1,8).toBits)
  val reg_stats = Reg(resetVal = Bool(false))
  val reg_error_mode  = Reg(resetVal = Bool(false))
  val reg_status = Reg{new Status} // reset down below

  val r_irq_timer = Reg(resetVal = Bool(false))
  val r_irq_ipi   = Reg(resetVal = Bool(true))

  val host_pcr_req_valid = Reg{Bool()} // don't reset
  val host_pcr_req_fire = host_pcr_req_valid && io.rw.cmd === PCR.N
  val host_pcr_rep_valid = Reg{Bool()} // don't reset
  val host_pcr_bits = Reg{io.host.pcr_req.bits.clone}
  io.host.pcr_req.ready := !host_pcr_req_valid && !host_pcr_rep_valid
  io.host.pcr_rep.valid := host_pcr_rep_valid
  io.host.pcr_rep.bits := host_pcr_bits.data
  when (io.host.pcr_req.fire()) {
    host_pcr_req_valid := true
    host_pcr_bits := io.host.pcr_req.bits
  }
  when (host_pcr_req_fire) {
    host_pcr_req_valid := false
    host_pcr_rep_valid := true
    host_pcr_bits.data := io.rw.rdata
  }
  when (io.host.pcr_rep.fire()) { host_pcr_rep_valid := false }

  val addr = Mux(io.rw.cmd != PCR.N, io.rw.addr, host_pcr_bits.addr)
  val wen = io.rw.cmd === PCR.T || io.rw.cmd === PCR.S || io.rw.cmd === PCR.C ||
    host_pcr_req_fire && host_pcr_bits.rw
  val wdata = Mux(io.rw.cmd != PCR.N, io.rw.wdata, host_pcr_bits.data)

  io.status := reg_status
  io.status.ip := Cat(r_irq_timer, reg_fromhost.orR, r_irq_ipi,   Bool(false),
                      Bool(false), Bool(false),      Bool(false), Bool(false))
  io.ptbr_wen := wen && addr === PTBR
  io.evec := Mux(io.exception, reg_ebase, reg_epc).toUFix
  io.ptbr := reg_ptbr
  io.host.debug.error_mode := reg_error_mode

  io.vecbank := reg_vecbank
  var cnt = UFix(0,4)
  for (i <- 0 until 8)
    cnt = cnt + reg_vecbank(i)
  io.vecbankcnt := cnt(3,0)

  io.stats := reg_stats

  when (io.badvaddr_wen || io.vec_irq_aux_wen) {
    val wdata = Mux(io.badvaddr_wen, io.rw.wdata, io.vec_irq_aux)
    val (upper, lower) = Split(wdata, VADDR_BITS)
    val sign = Mux(lower.toFix < Fix(0), upper.andR, upper.orR)
    reg_badvaddr := Cat(sign, lower).toFix
  }

  when (io.exception) {
    when (!reg_status.et) {
      reg_error_mode := true
    }
    reg_status.s := true
    reg_status.ps := reg_status.s
    reg_status.et := false
    reg_epc := io.pc.toFix
    reg_cause := io.cause
  }
  
  when (io.eret) {
    reg_status.s := reg_status.ps
    reg_status.et := true
  }
  
  when (reg_count === reg_compare) {
    r_irq_timer := Bool(true);
  }

  io.irq_timer := r_irq_timer;
  io.irq_ipi := r_irq_ipi;
  io.host.ipi_req.valid := io.rw.cmd === PCR.T && io.rw.addr === SEND_IPI
  io.host.ipi_req.bits := io.rw.wdata
  io.replay := io.host.ipi_req.valid && !io.host.ipi_req.ready

  when (host_pcr_req_fire && !host_pcr_bits.rw && host_pcr_bits.addr === TOHOST) { reg_tohost := UFix(0) }

  val read_impl = Bits(2)
  val read_ptbr = reg_ptbr(PADDR_BITS-1,PGIDX_BITS) << PGIDX_BITS
  val read_veccfg = if (conf.vec) Cat(io.vec_nfregs, io.vec_nxregs, io.vec_appvl) else Bits(0)
  val read_cause = reg_cause(reg_cause.getWidth-1) << conf.xprlen-1 | reg_cause(reg_cause.getWidth-2,0)
  io.rw.rdata := AVec[Bits](
    io.status.toBits,  reg_epc,          reg_badvaddr,     reg_ebase,
    reg_count,         reg_compare,      read_cause,       read_ptbr,
    reg_coreid/*x*/,   read_impl/*x*/,   reg_coreid,       read_impl,
    reg_k0,            reg_k1,           reg_k0/*x*/,      reg_k1/*x*/,
    reg_vecbank/*x*/,  read_veccfg/*x*/, reg_vecbank,      read_veccfg,
    reg_vecbank/*x*/,  read_veccfg/*x*/, reg_vecbank/*x*/, read_veccfg/*x*/,
    reg_vecbank/*x*/,  read_veccfg/*x*/, reg_tohost/*x*/,  reg_fromhost/*x*/,
    reg_stats/*x*/,    read_veccfg/*x*/, reg_tohost,       reg_fromhost
  )(addr)

  when (wen) {
    when (addr === STATUS) {
      val sr_wdata = Mux(io.rw.cmd === PCR.S, reg_status.toBits | wdata,
                     Mux(io.rw.cmd === PCR.C, reg_status.toBits & ~wdata,
                     wdata))
      reg_status := new Status().fromBits(sr_wdata)

      reg_status.zero := 0
      if (!conf.vec) reg_status.ev := false
      if (!conf.fpu) reg_status.ef := false
      if (!conf.rvc) reg_status.ec := false
    }
    when (addr === EPC)      { reg_epc := wdata(VADDR_BITS,0).toFix }
    when (addr === EVEC)     { reg_ebase := wdata(VADDR_BITS-1,0).toFix }
    when (addr === COUNT)    { reg_count := wdata.toUFix }
    when (addr === COMPARE)  { reg_compare := wdata(31,0).toUFix; r_irq_timer := Bool(false); }
    when (addr === COREID)   { reg_coreid := wdata(15,0) }
    when (addr === FROMHOST) { when (reg_fromhost === UFix(0) || !host_pcr_req_fire) { reg_fromhost := wdata } }
    when (addr === TOHOST)   { when (reg_tohost === UFix(0)) { reg_tohost := wdata } }
    when (addr === CLR_IPI)  { r_irq_ipi := wdata(0) }
    when (addr === K0)       { reg_k0 := wdata; }
    when (addr === K1)       { reg_k1 := wdata; }
    when (addr === PTBR)     { reg_ptbr := Cat(wdata(PADDR_BITS-1, PGIDX_BITS), Bits(0, PGIDX_BITS)).toUFix; }
    when (addr === VECBANK)  { reg_vecbank:= wdata(7,0) }
    when (addr === STATS)    { reg_stats := wdata(0) }
  }

  io.host.ipi_rep.ready := Bool(true)
  when (io.host.ipi_rep.valid) { r_irq_ipi := Bool(true) }

  when (reset) {
    reg_status.et := false
    reg_status.ef := false
    reg_status.ev := false
    reg_status.ec := false
    reg_status.ps := false
    reg_status.s := true
    reg_status.u64 := true
    reg_status.s64 := true
    reg_status.vm := false
    reg_status.zero := 0
    reg_status.im := 0
    reg_status.ip := 0
  }
}

class ioReadPort(d: Int, w: Int) extends Bundle
{
  override def clone = new ioReadPort(d, w).asInstanceOf[this.type]
}

class ioWritePort(d: Int, w: Int) extends Bundle
{
  val addr = UFix(INPUT, log2Up(d))
  val en   = Bool(INPUT)
  val data = Bits(INPUT, w)
  override def clone = new ioWritePort(d, w).asInstanceOf[this.type]
}
