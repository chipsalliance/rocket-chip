package rocket

import Chisel._
import Util._
import Node._
import uncore.HTIFIO
import uncore.constants.AddressConstants._
import scala.math._

class DpathBTBIO extends Bundle
{
  val current_pc     = UInt(INPUT, VADDR_BITS)
  val hit            = Bool(OUTPUT)
  val target         = UInt(OUTPUT, VADDR_BITS)
  val wen            = Bool(INPUT)
  val clr            = Bool(INPUT)
  val invalidate     = Bool(INPUT)
  val correct_pc     = UInt(INPUT, VADDR_BITS)
  val correct_target = UInt(INPUT, VADDR_BITS)
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
  val er = Bool()
  val vm = Bool()
  val s64 = Bool()
  val u64 = Bool()
  val ef = Bool()
  val pei = Bool()
  val ei = Bool()
  val ps = Bool()
  val s = Bool()
}

object CSR
{
  // commands
  val SZ = 2
  val X =  Bits("b??", 2)
  val N =  Bits(0,2)
  val W =  Bits(1,2)
  val S =  Bits(2,2)
  val C =  Bits(3,2)
}

class CSRFile(implicit conf: RocketConfiguration) extends Module
{
  val io = new Bundle {
    val host = new HTIFIO(conf.tl.ln.nClients)
    val rw = new Bundle {
      val addr = UInt(INPUT, 12)
      val cmd = Bits(INPUT, CSR.SZ)
      val rdata = Bits(OUTPUT, conf.xprlen)
      val wdata = Bits(INPUT, conf.xprlen)
    }
    
    val status = new Status().asOutput
    val ptbr = UInt(OUTPUT, PADDR_BITS)
    val evec = UInt(OUTPUT, VADDR_BITS+1)
    val exception = Bool(INPUT)
    val retire = UInt(INPUT, log2Up(1+conf.retireWidth))
    val uarch_counters = Vec.fill(16)(UInt(INPUT, log2Up(1+conf.retireWidth)))
    val cause = UInt(INPUT, conf.xprlen)
    val badvaddr_wen = Bool(INPUT)
    val pc = UInt(INPUT, VADDR_BITS+1)
    val sret = Bool(INPUT)
    val fatc = Bool(OUTPUT)
    val replay = Bool(OUTPUT)
    val time = UInt(OUTPUT, conf.xprlen)
    val fcsr_rm = Bits(OUTPUT, FPConstants.RM_SZ)
    val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ)).flip
    val rocc = new RoCCInterface().flip
  }
 
  val reg_epc = Reg(Bits(width = VADDR_BITS+1))
  val reg_badvaddr = Reg(Bits(width = VADDR_BITS))
  val reg_evec = Reg(Bits(width = VADDR_BITS))
  val reg_compare = Reg(Bits(width = 32))
  val reg_cause = Reg(Bits(width = conf.xprlen))
  val reg_tohost = Reg(init=Bits(0, conf.xprlen))
  val reg_fromhost = Reg(init=Bits(0, conf.xprlen))
  val reg_sup0 = Reg(Bits(width = conf.xprlen))
  val reg_sup1 = Reg(Bits(width = conf.xprlen))
  val reg_ptbr = Reg(UInt(width = PADDR_BITS))
  val reg_stats = Reg(init=Bool(false))
  val reg_status = Reg(new Status) // reset down below
  val reg_time = WideCounter(conf.xprlen)
  val reg_instret = WideCounter(conf.xprlen, io.retire)
  val reg_uarch_counters = io.uarch_counters.map(WideCounter(conf.xprlen, _))
  val reg_fflags = Reg(UInt(width = 5))
  val reg_frm = Reg(UInt(width = 3))

  val r_irq_timer = Reg(init=Bool(false))
  val r_irq_ipi = Reg(init=Bool(true))
  val irq_rocc = Bool(!conf.rocc.isEmpty) && io.rocc.interrupt

  val cpu_req_valid = io.rw.cmd != CSR.N
  val host_pcr_req_valid = Reg(Bool()) // don't reset
  val host_pcr_req_fire = host_pcr_req_valid && !cpu_req_valid
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
  
  io.host.debug_stats_pcr := reg_stats // direct export up the hierarchy

  val addr = Mux(cpu_req_valid, io.rw.addr, host_pcr_bits.addr | 0x500)
  val decoded_addr = {
    val default = List(Bits("b" + ("?"*CSRs.all.size), CSRs.all.size))
    val outs = for (i <- 0 until CSRs.all.size)
      yield UInt(CSRs.all(i), addr.getWidth) -> List(UInt(BigInt(1) << i, CSRs.all.size))

    val d = DecodeLogic(addr, default, outs).toArray
    val a = Array.fill(CSRs.all.max+1)(null.asInstanceOf[Bool])
    for (i <- 0 until CSRs.all.size)
      a(CSRs.all(i)) = d(0)(i)
    a
  }

  val wen = cpu_req_valid || host_pcr_req_fire && host_pcr_bits.rw
  val wdata = Mux(cpu_req_valid, io.rw.wdata, host_pcr_bits.data)

  io.status := reg_status
  io.status.ip := Cat(r_irq_timer, reg_fromhost.orR, r_irq_ipi,   Bool(false),
                      Bool(false), irq_rocc,         Bool(false), Bool(false))
  io.fatc := wen && decoded_addr(CSRs.fatc)
  io.evec := Mux(io.exception, reg_evec.toSInt, reg_epc).toUInt
  io.ptbr := reg_ptbr

  when (io.badvaddr_wen) {
    val wdata = io.rw.wdata
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
  
  when (io.sret) {
    reg_status.s := reg_status.ps
    reg_status.ei := reg_status.pei
  }
  
  when (reg_time(reg_compare.getWidth-1,0) === reg_compare) {
    r_irq_timer := true
  }

  io.time := reg_time
  io.host.ipi_req.valid := cpu_req_valid && decoded_addr(CSRs.send_ipi)
  io.host.ipi_req.bits := io.rw.wdata
  io.replay := io.host.ipi_req.valid && !io.host.ipi_req.ready

  when (host_pcr_req_fire && !host_pcr_bits.rw && decoded_addr(CSRs.tohost)) { reg_tohost := UInt(0) }

  val read_impl = Bits(2)
  val read_ptbr = reg_ptbr(PADDR_BITS-1,PGIDX_BITS) << PGIDX_BITS

  val read_mapping = collection.mutable.Map[Int,Bits](
    CSRs.fflags -> (if (conf.fpu) reg_fflags else UInt(0)),
    CSRs.frm -> (if (conf.fpu) reg_frm else UInt(0)),
    CSRs.fcsr -> (if (conf.fpu) Cat(reg_frm, reg_fflags) else UInt(0)),
    CSRs.cycle -> reg_time,
    CSRs.time -> reg_time,
    CSRs.instret -> reg_instret,
    CSRs.sup0 -> reg_sup0,
    CSRs.sup1 -> reg_sup1,
    CSRs.epc -> reg_epc,
    CSRs.badvaddr -> reg_badvaddr,
    CSRs.ptbr -> read_ptbr,
    CSRs.asid -> UInt(0),
    CSRs.count -> reg_time,
    CSRs.compare -> reg_compare,
    CSRs.evec -> reg_evec,
    CSRs.cause -> reg_cause,
    CSRs.status -> io.status.toBits,
    CSRs.hartid -> io.host.id,
    CSRs.impl -> read_impl,
    CSRs.fatc -> read_impl, // don't care
    CSRs.send_ipi -> read_impl, // don't care
    CSRs.clear_ipi -> read_impl, // don't care
    CSRs.stats -> reg_stats,
    CSRs.tohost -> reg_tohost,
    CSRs.fromhost -> reg_fromhost)

  for (i <- 0 until reg_uarch_counters.size)
    read_mapping += (CSRs.uarch0 + i) -> reg_uarch_counters(i)

  io.rw.rdata := Mux1H(for ((k, v) <- read_mapping) yield decoded_addr(k) -> v)

  io.fcsr_rm := reg_frm
  when (io.fcsr_flags.valid) {
    reg_fflags := reg_fflags | io.fcsr_flags.bits
  }

  when (wen) {
    when (decoded_addr(CSRs.status)) {
      reg_status := new Status().fromBits(wdata)
      reg_status.s64 := true
      reg_status.u64 := true
      reg_status.zero := 0
      if (!conf.vm) reg_status.vm := false
      if (conf.rocc.isEmpty) reg_status.er := false
      if (!conf.fpu) reg_status.ef := false
    }
    when (decoded_addr(CSRs.fflags))   { reg_fflags := wdata }
    when (decoded_addr(CSRs.frm))      { reg_frm := wdata }
    when (decoded_addr(CSRs.fcsr))     { reg_fflags := wdata; reg_frm := wdata >> reg_fflags.getWidth }
    when (decoded_addr(CSRs.epc))      { reg_epc := wdata(VADDR_BITS,0).toSInt }
    when (decoded_addr(CSRs.evec))     { reg_evec := wdata(VADDR_BITS-1,0).toSInt }
    when (decoded_addr(CSRs.count))    { reg_time := wdata.toUInt }
    when (decoded_addr(CSRs.compare))  { reg_compare := wdata(31,0).toUInt; r_irq_timer := Bool(false) }
    when (decoded_addr(CSRs.fromhost)) { when (reg_fromhost === UInt(0) || !host_pcr_req_fire) { reg_fromhost := wdata } }
    when (decoded_addr(CSRs.tohost))   { when (reg_tohost === UInt(0) || host_pcr_req_fire) { reg_tohost := wdata } }
    when (decoded_addr(CSRs.clear_ipi)){ r_irq_ipi := wdata(0) }
    when (decoded_addr(CSRs.sup0))     { reg_sup0 := wdata }
    when (decoded_addr(CSRs.sup1))     { reg_sup1 := wdata }
    when (decoded_addr(CSRs.ptbr))     { reg_ptbr := Cat(wdata(PADDR_BITS-1, PGIDX_BITS), Bits(0, PGIDX_BITS)).toUInt }
    when (decoded_addr(CSRs.stats))    { reg_stats := wdata(0) }
  }

  io.host.ipi_rep.ready := Bool(true)
  when (io.host.ipi_rep.valid) { r_irq_ipi := Bool(true) }

  when(this.reset) {
    reg_status.ei := false
    reg_status.pei := false
    reg_status.ef := false
    reg_status.er := false
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
