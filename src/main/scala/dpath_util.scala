package rocket

import Chisel._
import Util._
import Node._
import uncore.constants.AddressConstants._
import scala.math._

class DpathBTBIO extends Bundle
{
  val current_pc     = UInt(INPUT, VADDR_BITS);
  val hit            = Bool(OUTPUT);
  val target         = UInt(OUTPUT, VADDR_BITS);
  val wen            = Bool(INPUT);
  val clr            = Bool(INPUT);
  val invalidate     = Bool(INPUT);
  val correct_pc     = UInt(INPUT, VADDR_BITS);
  val correct_target = UInt(INPUT, VADDR_BITS);
}

// fully-associative branch target buffer
class rocketDpathBTB(entries: Int) extends Module
{
  val io = new DpathBTBIO

  var hit_reduction = Bool(false)
  val hit = Bool()
  val update = Bool()
  var update_reduction = Bool(false)
  val valid = Vec.fill(entries){Reg(init=Bool(false))}
  val hits = Vec.fill(entries){Bool()}
  val updates = Vec.fill(entries){Bool()}
  val targets = Vec.fill(entries){Reg(UInt())}
  val anyUpdate = updates.toBits.orR

  val random_way = Random(entries, io.wen)
  val invalid_way = valid.indexWhere((x: Bool) => !x)
  val repl_way = Mux(valid.contains(Bool(false)), invalid_way, random_way)

  for (i <- 0 until entries) {
    val tag = Reg(UInt())
    hits(i) := valid(i) && tag === io.current_pc
    updates(i) := valid(i) && tag === io.correct_pc

    when (io.wen && (updates(i) || !anyUpdate && UInt(i) === repl_way)) {
      valid(i) := Bool(false)
      when (!io.clr) {
        valid(i) := Bool(true)
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
  val ev = Bool()
  val vm = Bool()
  val s64 = Bool()
  val u64 = Bool()
  val ef = Bool()
  val pei = Bool()
  val ei = Bool()
  val ps = Bool()
  val s = Bool()
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
  val SUP0     =  0
  val SUP1     =  1
  val EPC      =  2
  val BADVADDR =  3
  val PTBR     =  4
  val ASID     =  5
  val COUNT    =  6
  val COMPARE  =  7
  val EVEC     =  8
  val CAUSE    =  9
  val STATUS   = 10
  val HARTID   = 11
  val IMPL     = 12
  val FATC     = 13
  val SEND_IPI = 14
  val CLR_IPI  = 15
  val VECBANK  = 18
  val VECCFG   = 19
  val STATS    = 28
  val RESET    = 29
  val TOHOST   = 30
  val FROMHOST = 31
}

class PCR(implicit conf: RocketConfiguration) extends Module
{
  val io = new Bundle {
    val host = new HTIFIO(conf.tl.ln.nClients)
    val rw = new Bundle {
      val addr = UInt(INPUT, log2Up(conf.nxpr))
      val cmd = Bits(INPUT, PCR.SZ)
      val rdata = Bits(OUTPUT, conf.xprlen)
      val wdata = Bits(INPUT, conf.xprlen)
    }

    // there is a fixed constant related to this in PCRReq.addr
    require(log2Up(conf.nxpr) == 5)
    
    val status = new Status().asOutput
    val ptbr = UInt(OUTPUT, PADDR_BITS)
    val evec = UInt(OUTPUT, VADDR_BITS+1)
    val exception = Bool(INPUT)
    val cause = UInt(INPUT, 6)
    val badvaddr_wen = Bool(INPUT)
    val vec_irq_aux = Bits(INPUT, conf.xprlen)
    val vec_irq_aux_wen = Bool(INPUT)
    val pc = UInt(INPUT, VADDR_BITS+1)
    val eret = Bool(INPUT)
    val ei = Bool(INPUT)
    val di = Bool(INPUT)
    val fatc = Bool(OUTPUT)
    val irq_timer = Bool(OUTPUT)
    val irq_ipi = Bool(OUTPUT)
    val replay = Bool(OUTPUT)
    val vecbank = Bits(OUTPUT, 8)
    val vecbankcnt = UInt(OUTPUT, 4)
    val stats = Bool(OUTPUT)
    val vec_appvl = UInt(INPUT, 12)
    val vec_nxregs = UInt(INPUT, 6)
    val vec_nfregs = UInt(INPUT, 6)
  }
  import PCR._
 
  val reg_epc = Reg(Bits(width = VADDR_BITS+1))
  val reg_badvaddr = Reg(Bits(width = VADDR_BITS))
  val reg_evec = Reg(Bits(width = VADDR_BITS))
  val reg_count = WideCounter(32)
  val reg_compare = Reg(Bits(width = 32))
  val reg_cause = Reg(Bits(width = io.cause.getWidth))
  val reg_tohost = Reg(init=Bits(0, conf.xprlen))
  val reg_fromhost = Reg(init=Bits(0, conf.xprlen))
  val reg_sup0 = Reg(Bits(width = conf.xprlen))
  val reg_sup1 = Reg(Bits(width = conf.xprlen))
  val reg_ptbr = Reg(UInt(width = PADDR_BITS))
  val reg_vecbank = Reg(init=SInt(-1,8).toBits)
  val reg_stats = Reg(init=Bool(false))
  val reg_status = Reg(new Status) // reset down below

  val r_irq_timer = Reg(init=Bool(false))
  val r_irq_ipi = Reg(init=Bool(true))

  val host_pcr_req_valid = Reg(Bool()) // don't reset
  val host_pcr_req_fire = host_pcr_req_valid && io.rw.cmd === PCR.N
  val host_pcr_rep_valid = Reg(Bool()) // don't reset
  val host_pcr_bits = Reg(io.host.pcr_req.bits)
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
  io.fatc := wen && addr === FATC
  io.evec := Mux(io.exception, reg_evec.toSInt, reg_epc).toUInt
  io.ptbr := reg_ptbr

  io.vecbank := reg_vecbank
  var cnt = UInt(0,4)
  for (i <- 0 until 8)
    cnt = cnt + reg_vecbank(i)
  io.vecbankcnt := cnt(3,0)

  io.stats := reg_stats

  when (io.badvaddr_wen || io.vec_irq_aux_wen) {
    val wdata = Mux(io.badvaddr_wen, io.rw.wdata, io.vec_irq_aux)
    val (upper, lower) = Split(wdata, VADDR_BITS)
    val sign = Mux(lower.toSInt < SInt(0), upper.andR, upper.orR)
    reg_badvaddr := Cat(sign, lower).toSInt
  }

  when (io.exception) {
    reg_status.s := true
    reg_status.ps := reg_status.s
    reg_status.ei := false
    reg_status.pei := reg_status.ei
    reg_epc := io.pc.toSInt
    reg_cause := io.cause
  }
  
  when (io.eret) {
    reg_status.s := reg_status.ps
    reg_status.ei := reg_status.pei
  }
  
  when (reg_count === reg_compare) {
    r_irq_timer := Bool(true);
  }

  io.irq_timer := r_irq_timer;
  io.irq_ipi := r_irq_ipi;
  io.host.ipi_req.valid := io.rw.cmd === PCR.T && io.rw.addr === SEND_IPI
  io.host.ipi_req.bits := io.rw.wdata
  io.replay := io.host.ipi_req.valid && !io.host.ipi_req.ready

  when (host_pcr_req_fire && !host_pcr_bits.rw && host_pcr_bits.addr === TOHOST) { reg_tohost := UInt(0) }

  val read_impl = Bits(2)
  val read_ptbr = reg_ptbr(PADDR_BITS-1,PGIDX_BITS) << PGIDX_BITS
  val read_veccfg = if (conf.vec) Cat(io.vec_nfregs, io.vec_nxregs, io.vec_appvl) else Bits(0)
  val read_cause = reg_cause(reg_cause.getWidth-1) << conf.xprlen-1 | reg_cause(reg_cause.getWidth-2,0)
  io.rw.rdata := AVec[Bits](
    reg_sup0,          reg_sup1,         reg_epc,          reg_badvaddr,
    reg_ptbr,          Bits(0)/*asid*/,  reg_count,        reg_compare,
    reg_evec,          reg_cause,        io.status.toBits, io.host.id,
    read_impl,         read_impl/*x*/,   read_impl/*x*/,   read_impl/*x*/,
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

      reg_status.s64 := true
      reg_status.u64 := true
      reg_status.zero := 0
      if (!conf.vec) reg_status.ev := false
      if (!conf.fpu) reg_status.ef := false
    }
    when (addr === EPC)      { reg_epc := wdata(VADDR_BITS,0).toSInt }
    when (addr === EVEC)     { reg_evec := wdata(VADDR_BITS-1,0).toSInt }
    when (addr === COUNT)    { reg_count := wdata.toUInt }
    when (addr === COMPARE)  { reg_compare := wdata(31,0).toUInt; r_irq_timer := Bool(false); }
    when (addr === FROMHOST) { when (reg_fromhost === UInt(0) || !host_pcr_req_fire) { reg_fromhost := wdata } }
    when (addr === TOHOST)   { when (reg_tohost === UInt(0)) { reg_tohost := wdata } }
    when (addr === CLR_IPI)  { r_irq_ipi := wdata(0) }
    when (addr === SUP0)     { reg_sup0 := wdata; }
    when (addr === SUP1)     { reg_sup1 := wdata; }
    when (addr === PTBR)     { reg_ptbr := Cat(wdata(PADDR_BITS-1, PGIDX_BITS), Bits(0, PGIDX_BITS)).toUInt; }
    when (addr === VECBANK)  { reg_vecbank:= wdata(7,0) }
    when (addr === STATS)    { reg_stats := wdata(0) }
  }

  io.host.ipi_rep.ready := Bool(true)
  when (io.host.ipi_rep.valid) { r_irq_ipi := Bool(true) }

  when(this.reset) {
    reg_status.ei := false
    reg_status.pei := false
    reg_status.ef := false
    reg_status.ev := false
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
  val addr = UInt(INPUT, log2Up(d))
  val en   = Bool(INPUT)
  val data = Bits(INPUT, w)
  override def clone = new ioWritePort(d, w).asInstanceOf[this.type]
}
