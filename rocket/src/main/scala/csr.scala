// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import Instructions._
import uncore._
import scala.math._

class MStatus extends Bundle {
  val sd = Bool()
  val zero2 = UInt(width = 31)
  val sd_rv32 = UInt(width = 1)
  val zero1 = UInt(width = 9)
  val vm = UInt(width = 5)
  val mprv = Bool()
  val xs = UInt(width = 2)
  val fs = UInt(width = 2)
  val prv3 = UInt(width = 2)
  val ie3 = Bool()
  val prv2 = UInt(width = 2)
  val ie2 = Bool()
  val prv1 = UInt(width = 2)
  val ie1 = Bool()
  val prv = UInt(width = 2)
  val ie = Bool()
}

class SStatus extends Bundle {
  val sd = Bool()
  val zero4 = UInt(width = 31)
  val sd_rv32 = UInt(width = 1)
  val zero3 = UInt(width = 14)
  val mprv = Bool()
  val xs = UInt(width = 2)
  val fs = UInt(width = 2)
  val zero2 = UInt(width = 7)
  val ps = Bool()
  val pie = Bool()
  val zero1 = UInt(width = 2)
  val ie = Bool()
}

class MIP extends Bundle {
  val mtip = Bool()
  val htip = Bool()
  val stip = Bool()
  val utip = Bool()
  val msip = Bool()
  val hsip = Bool()
  val ssip = Bool()
  val usip = Bool()
}

object CSR
{
  // commands
  val SZ = 3
  val X = BitPat.DC(SZ)
  val N = UInt(0,SZ)
  val W = UInt(1,SZ)
  val S = UInt(2,SZ)
  val C = UInt(3,SZ)
  val I = UInt(4,SZ)
  val R = UInt(5,SZ)
}

class CSRFileIO extends CoreBundle {
  val host = new HTIFIO
  val rw = new Bundle {
    val addr = UInt(INPUT, 12)
    val cmd = Bits(INPUT, CSR.SZ)
    val rdata = Bits(OUTPUT, xLen)
    val wdata = Bits(INPUT, xLen)
  }

  val csr_replay = Bool(OUTPUT)
  val csr_stall = Bool(OUTPUT)
  val csr_xcpt = Bool(OUTPUT)
  val eret = Bool(OUTPUT)

  val status = new MStatus().asOutput
  val ptbr = UInt(OUTPUT, paddrBits)
  val evec = UInt(OUTPUT, vaddrBitsExtended)
  val exception = Bool(INPUT)
  val retire = UInt(INPUT, log2Up(1+retireWidth))
  val uarch_counters = Vec.fill(16)(UInt(INPUT, log2Up(1+retireWidth)))
  val custom_mrw_csrs = Vec.fill(params(NCustomMRWCSRs))(UInt(INPUT, xLen))
  val cause = UInt(INPUT, xLen)
  val pc = UInt(INPUT, vaddrBitsExtended)
  val fatc = Bool(OUTPUT)
  val time = UInt(OUTPUT, xLen)
  val fcsr_rm = Bits(OUTPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ)).flip
  val rocc = new RoCCInterface().flip
  val interrupt = Bool(OUTPUT)
  val interrupt_cause = UInt(OUTPUT, xLen)
}

class CSRFile extends CoreModule
{
  val io = new CSRFileIO

  val reg_mstatus = Reg(new MStatus)
  val reg_mie = Reg(init=new MIP().fromBits(0))
  val reg_mip = Reg(init=new MIP().fromBits(0))
  val reg_mepc = Reg(UInt(width = vaddrBitsExtended))
  val reg_mcause = Reg(Bits(width = xLen))
  val reg_mbadaddr = Reg(UInt(width = vaddrBitsExtended))
  val reg_mscratch = Reg(Bits(width = xLen))

  val reg_sepc = Reg(UInt(width = vaddrBitsExtended))
  val reg_scause = Reg(Bits(width = xLen))
  val reg_sbadaddr = Reg(UInt(width = vaddrBitsExtended))
  val reg_sscratch = Reg(Bits(width = xLen))
  val reg_stvec = Reg(UInt(width = vaddrBits))
  val reg_mtimecmp = Reg(Bits(width = xLen))
  val reg_sptbr = Reg(UInt(width = paddrBits))
  val reg_wfi = Reg(init=Bool(false))

  val reg_tohost = Reg(init=Bits(0, xLen))
  val reg_fromhost = Reg(init=Bits(0, xLen))
  val reg_stats = Reg(init=Bool(false))
  val reg_time = Reg(UInt(width = xLen))
  val reg_cycle = WideCounter(xLen)
  val reg_instret = WideCounter(xLen, io.retire)
  val reg_uarch_counters = io.uarch_counters.map(WideCounter(xLen, _))
  val reg_fflags = Reg(UInt(width = 5))
  val reg_frm = Reg(UInt(width = 3))

  val irq_rocc = Bool(!params(BuildRoCC).isEmpty) && io.rocc.interrupt

  io.interrupt_cause := 0
  io.interrupt := io.interrupt_cause(xLen-1)
  val some_interrupt_pending = Wire(init=Bool(false))
  def checkInterrupt(max_priv: UInt, cond: Bool, num: Int) = {
    when (cond && (reg_mstatus.prv < max_priv || reg_mstatus.prv === max_priv && reg_mstatus.ie)) {
      io.interrupt_cause := UInt((BigInt(1) << (xLen-1)) + num)
    }
    when (cond && reg_mstatus.prv <= max_priv) {
      some_interrupt_pending := true
    }
  }

  checkInterrupt(PRV_S, reg_mie.ssip && reg_mip.ssip, 0)
  checkInterrupt(PRV_M, reg_mie.msip && reg_mip.msip, 0)
  checkInterrupt(PRV_S, reg_mie.stip && reg_mip.stip, 1)
  checkInterrupt(PRV_M, reg_mie.mtip && reg_mip.mtip, 1)
  checkInterrupt(PRV_M, reg_fromhost != 0, 2)
  checkInterrupt(PRV_M, irq_rocc, 3)

  val system_insn = io.rw.cmd === CSR.I
  val cpu_ren = io.rw.cmd != CSR.N && !system_insn

  val host_pcr_req_valid = Reg(Bool()) // don't reset
  val host_pcr_req_fire = host_pcr_req_valid && !cpu_ren
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

  val read_mstatus = io.status.toBits
  val isa_string = "IMA" +
    (if (params(UseVM)) "S" else "") +
    (if (!params(BuildFPU).isEmpty) "FD" else "") +
    (if (!params(BuildRoCC).isEmpty) "X" else "")
  val cpuid = ((if (xLen == 32) BigInt(0) else BigInt(2)) << (xLen-2)) |
    isa_string.map(x => 1 << (x - 'A')).reduce(_|_)
  val impid = 1

  val read_mapping = collection.mutable.LinkedHashMap[Int,Bits](
    CSRs.fflags -> (if (!params(BuildFPU).isEmpty) reg_fflags else UInt(0)),
    CSRs.frm -> (if (!params(BuildFPU).isEmpty) reg_frm else UInt(0)),
    CSRs.fcsr -> (if (!params(BuildFPU).isEmpty) Cat(reg_frm, reg_fflags) else UInt(0)),
    CSRs.cycle -> reg_cycle,
    CSRs.cyclew -> reg_cycle,
    CSRs.instret -> reg_instret,
    CSRs.instretw -> reg_instret,
    CSRs.time -> reg_time,
    CSRs.timew -> reg_time,
    CSRs.stime -> reg_time,
    CSRs.stimew -> reg_time,
    CSRs.mtime -> reg_time,
    CSRs.mcpuid -> UInt(cpuid),
    CSRs.mimpid -> UInt(impid),
    CSRs.mstatus -> read_mstatus,
    CSRs.mtdeleg -> UInt(0),
    CSRs.mreset -> UInt(0),
    CSRs.mtvec -> UInt(MTVEC),
    CSRs.mip -> reg_mip.toBits,
    CSRs.mie -> reg_mie.toBits,
    CSRs.mscratch -> reg_mscratch,
    CSRs.mepc -> reg_mepc.sextTo(xLen),
    CSRs.mbadaddr -> reg_mbadaddr.sextTo(xLen),
    CSRs.mcause -> reg_mcause,
    CSRs.mtimecmp -> reg_mtimecmp,
    CSRs.mhartid -> io.host.id,
    CSRs.send_ipi -> io.host.id, /* don't care */
    CSRs.stats -> reg_stats,
    CSRs.mtohost -> reg_tohost,
    CSRs.mfromhost -> reg_fromhost)

  if (params(UseVM)) {
    val read_sstatus = Wire(init=new SStatus().fromBits(read_mstatus))
    read_sstatus.zero1 := 0
    read_sstatus.zero2 := 0
    read_sstatus.zero3 := 0
    read_sstatus.zero4 := 0

    val read_sip = Wire(init=new MIP().fromBits(0))
    read_sip.ssip := reg_mip.ssip
    read_sip.stip := reg_mip.stip

    val read_sie = Wire(init=new MIP().fromBits(0))
    read_sie.ssip := reg_mie.ssip
    read_sie.stip := reg_mie.stip

    read_mapping += CSRs.sstatus -> read_sstatus.toBits
    read_mapping += CSRs.sip -> read_sip.toBits
    read_mapping += CSRs.sie -> read_sie.toBits
    read_mapping += CSRs.sscratch -> reg_sscratch
    read_mapping += CSRs.scause -> reg_scause
    read_mapping += CSRs.sbadaddr -> reg_sbadaddr.sextTo(xLen)
    read_mapping += CSRs.sptbr -> reg_sptbr
    read_mapping += CSRs.sasid -> UInt(0)
    read_mapping += CSRs.sepc -> reg_sepc.sextTo(xLen)
    read_mapping += CSRs.stvec -> reg_stvec.sextTo(xLen)
  }

  for (i <- 0 until reg_uarch_counters.size)
    read_mapping += (CSRs.uarch0 + i) -> reg_uarch_counters(i)

  for (i <- 0 until params(NCustomMRWCSRs)) {
    val addr = 0x790 + i // turn 0x790 into parameter CustomMRWCSRBase?
    require(addr >= 0x780 && addr <= 0x7ff, "custom MRW CSR address " + i + " is out of range")
    require(!read_mapping.contains(addr), "custom MRW CSR address " + i + " is already in use")
    read_mapping += addr -> io.custom_mrw_csrs(i)
  }

  val addr = Mux(cpu_ren, io.rw.addr, host_pcr_bits.addr)
  val decoded_addr = read_mapping map { case (k, v) => k -> (addr === k) }

  val addr_valid = decoded_addr.values.reduce(_||_)
  val fp_csr = decoded_addr(CSRs.fflags) || decoded_addr(CSRs.frm) || decoded_addr(CSRs.fcsr)
  val csr_addr_priv = io.rw.addr(9,8)
  val priv_sufficient = reg_mstatus.prv >= csr_addr_priv
  val read_only = io.rw.addr(11,10).andR
  val cpu_wen = cpu_ren && io.rw.cmd != CSR.R && priv_sufficient
  val wen = cpu_wen && !read_only || host_pcr_req_fire && host_pcr_bits.rw
  val wdata = Mux(io.rw.cmd === CSR.W, io.rw.wdata,
              Mux(io.rw.cmd === CSR.C, io.rw.rdata & ~io.rw.wdata,
              Mux(io.rw.cmd === CSR.S, io.rw.rdata | io.rw.wdata,
              host_pcr_bits.data)))

  val opcode = io.rw.addr
  val insn_call = !opcode(8) && !opcode(0) && system_insn
  val insn_break = !opcode(8) && opcode(0) && system_insn
  val insn_ret = opcode(8) && !opcode(1) && !opcode(0) && system_insn && priv_sufficient
  val insn_sfence_vm = opcode(8) && !opcode(1) && opcode(0) && system_insn && priv_sufficient
  val maybe_insn_redirect_trap = opcode(2) && system_insn
  val insn_redirect_trap = maybe_insn_redirect_trap && priv_sufficient
  val insn_wfi = opcode(8) && opcode(1) && !opcode(0) && system_insn && priv_sufficient

  val csr_xcpt = (cpu_wen && read_only) ||
    (cpu_ren && (!priv_sufficient || !addr_valid || fp_csr && !io.status.fs.orR)) ||
    (system_insn && !priv_sufficient) ||
    insn_call || insn_break

  when (insn_wfi) { reg_wfi := true }
  when (some_interrupt_pending) { reg_wfi := false }

  io.fatc := insn_sfence_vm
  io.evec := Mux(io.exception || csr_xcpt, (reg_mstatus.prv << 6) + MTVEC,
             Mux(maybe_insn_redirect_trap, reg_stvec.sextTo(vaddrBitsExtended),
             Mux(reg_mstatus.prv(1), reg_mepc, reg_sepc)))
  io.ptbr := reg_sptbr
  io.csr_xcpt := csr_xcpt
  io.eret := insn_ret || insn_redirect_trap
  io.status := reg_mstatus
  io.status.fs := Fill(2, reg_mstatus.fs.orR) // either off or dirty (no clean/initial support yet)
  io.status.xs := Fill(2, reg_mstatus.xs.orR) // either off or dirty (no clean/initial support yet)
  io.status.sd := io.status.fs.andR || io.status.xs.andR
  if (xLen == 32)
    io.status.sd_rv32 := io.status.sd

  when (io.exception || csr_xcpt) {
    reg_mstatus.ie := false
    reg_mstatus.prv := PRV_M
    reg_mstatus.mprv := false
    reg_mstatus.prv1 := reg_mstatus.prv
    reg_mstatus.ie1 := reg_mstatus.ie
    reg_mstatus.prv2 := reg_mstatus.prv1
    reg_mstatus.ie2 := reg_mstatus.ie1

    reg_mepc := ~(~io.pc | (coreInstBytes-1))
    reg_mcause := io.cause
    when (csr_xcpt) {
      reg_mcause := Causes.illegal_instruction
      when (insn_break) { reg_mcause := Causes.breakpoint }
      when (insn_call) { reg_mcause := reg_mstatus.prv + Causes.user_ecall }
    }

    reg_mbadaddr := io.pc
    when (io.cause === Causes.fault_load || io.cause === Causes.misaligned_load ||
          io.cause === Causes.fault_store || io.cause === Causes.misaligned_store) {
      val (upper, lower) = Split(io.rw.wdata, vaddrBits)
      val sign = Mux(lower.toSInt < SInt(0), upper.andR, upper.orR)
      reg_mbadaddr := Cat(sign, lower)
    }
  }
  
  when (insn_ret) {
    reg_mstatus.ie := reg_mstatus.ie1
    reg_mstatus.prv := reg_mstatus.prv1
    reg_mstatus.prv1 := reg_mstatus.prv2
    reg_mstatus.ie1 := reg_mstatus.ie2
    reg_mstatus.prv2 := PRV_U
    reg_mstatus.ie2 := true
  }
  
  when (insn_redirect_trap) {
    reg_mstatus.prv := PRV_S
    reg_sbadaddr := reg_mbadaddr
    reg_scause := reg_mcause
    reg_sepc := reg_mepc
  }

  assert(PopCount(insn_ret :: insn_redirect_trap :: io.exception :: csr_xcpt :: io.csr_replay :: Nil) <= 1, "these conditions must be mutually exclusive")

  when (reg_time >= reg_mtimecmp) {
    reg_mip.mtip := true
  }

  io.time := reg_cycle
  io.host.ipi_req.valid := cpu_wen && decoded_addr(CSRs.send_ipi)
  io.host.ipi_req.bits := io.rw.wdata
  io.csr_replay := io.host.ipi_req.valid && !io.host.ipi_req.ready
  io.csr_stall := reg_wfi

  when (host_pcr_req_fire && !host_pcr_bits.rw && decoded_addr(CSRs.mtohost)) { reg_tohost := UInt(0) }

  io.rw.rdata := Mux1H(for ((k, v) <- read_mapping) yield decoded_addr(k) -> v)

  io.fcsr_rm := reg_frm
  when (io.fcsr_flags.valid) {
    reg_fflags := reg_fflags | io.fcsr_flags.bits
  }

  when (wen) {
    when (decoded_addr(CSRs.mstatus)) {
      val new_mstatus = new MStatus().fromBits(wdata)
      reg_mstatus.ie := new_mstatus.ie
      reg_mstatus.ie1 := new_mstatus.ie1

      val supportedModes = Vec((PRV_M :: PRV_U :: (if (params(UseVM)) List(PRV_S) else Nil)).map(UInt(_)))
      if (supportedModes.size > 1) {
        reg_mstatus.mprv := new_mstatus.mprv
        when (supportedModes contains new_mstatus.prv) { reg_mstatus.prv := new_mstatus.prv }
        when (supportedModes contains new_mstatus.prv1) { reg_mstatus.prv1 := new_mstatus.prv1 }
        if (supportedModes.size > 2) {
          when (supportedModes contains new_mstatus.prv2) { reg_mstatus.prv2 := new_mstatus.prv2 }
          reg_mstatus.ie2 := new_mstatus.ie2
        }
      }

      if (params(UseVM)) {
        val vm_on = if (xLen == 32) 8 else 9
        when (new_mstatus.vm === 0) { reg_mstatus.vm := 0 }
        when (new_mstatus.vm === vm_on) { reg_mstatus.vm := vm_on }
      }
      if (params(UseVM) || !params(BuildFPU).isEmpty) reg_mstatus.fs := new_mstatus.fs
      if (!params(BuildRoCC).isEmpty) reg_mstatus.xs := new_mstatus.xs
    }
    when (decoded_addr(CSRs.mip)) {
      val new_mip = new MIP().fromBits(wdata)
      if (params(UseVM)) {
        reg_mip.ssip := new_mip.ssip
        reg_mip.stip := new_mip.stip
      }
      reg_mip.msip := new_mip.msip
    }
    when (decoded_addr(CSRs.mie)) {
      val new_mie = new MIP().fromBits(wdata)
      if (params(UseVM)) {
        reg_mie.ssip := new_mie.ssip
        reg_mie.stip := new_mie.stip
      }
      reg_mie.msip := new_mie.msip
      reg_mie.mtip := new_mie.mtip
    }
    when (decoded_addr(CSRs.fflags))   { reg_fflags := wdata }
    when (decoded_addr(CSRs.frm))      { reg_frm := wdata }
    when (decoded_addr(CSRs.fcsr))     { reg_fflags := wdata; reg_frm := wdata >> reg_fflags.getWidth }
    when (decoded_addr(CSRs.mepc))     { reg_mepc := ~(~wdata | (coreInstBytes-1)) }
    when (decoded_addr(CSRs.mscratch)) { reg_mscratch := wdata }
    when (decoded_addr(CSRs.mcause))   { reg_mcause := wdata & UInt((BigInt(1) << (xLen-1)) + 31) /* only implement 5 LSBs and MSB */ }
    when (decoded_addr(CSRs.mbadaddr)) { reg_mbadaddr := wdata(vaddrBitsExtended-1,0) }
    when (decoded_addr(CSRs.instretw)) { reg_instret := wdata }
    when (decoded_addr(CSRs.mtimecmp)) { reg_mtimecmp := wdata; reg_mip.mtip := false }
    when (decoded_addr(CSRs.mreset) /* XXX used by HTIF to write mtime */) { reg_time := wdata }
    when (decoded_addr(CSRs.mfromhost)){ when (reg_fromhost === UInt(0) || !host_pcr_req_fire) { reg_fromhost := wdata } }
    when (decoded_addr(CSRs.mtohost))  { when (reg_tohost === UInt(0) || host_pcr_req_fire) { reg_tohost := wdata } }
    when (decoded_addr(CSRs.stats))    { reg_stats := wdata(0) }
    if (params(UseVM)) {
      when (decoded_addr(CSRs.sstatus)) {
        val new_sstatus = new SStatus().fromBits(wdata)
        reg_mstatus.ie := new_sstatus.ie
        reg_mstatus.ie1 := new_sstatus.pie
        reg_mstatus.prv1 := Mux[UInt](new_sstatus.ps, PRV_S, PRV_U)
        reg_mstatus.mprv := new_sstatus.mprv
        reg_mstatus.fs := new_sstatus.fs // even without an FPU
        if (!params(BuildRoCC).isEmpty) reg_mstatus.xs := new_sstatus.xs
      }
      when (decoded_addr(CSRs.sip)) {
        val new_sip = new MIP().fromBits(wdata)
        reg_mip.ssip := new_sip.ssip
      }
      when (decoded_addr(CSRs.sie)) {
        val new_sie = new MIP().fromBits(wdata)
        reg_mie.ssip := new_sie.ssip
        reg_mie.stip := new_sie.stip
      }
      when (decoded_addr(CSRs.sscratch)) { reg_sscratch := wdata }
      when (decoded_addr(CSRs.sptbr))    { reg_sptbr := Cat(wdata(paddrBits-1, pgIdxBits), Bits(0, pgIdxBits)) }
      when (decoded_addr(CSRs.sepc))     { reg_sepc := ~(~wdata | (coreInstBytes-1)) }
      when (decoded_addr(CSRs.stvec))    { reg_stvec := ~(~wdata | (coreInstBytes-1)) }
    }
  }

  io.host.ipi_rep.ready := true
  when (io.host.ipi_rep.valid) { reg_mip.msip := true }

  when(this.reset) {
    reg_mstatus.zero1 := 0
    reg_mstatus.zero2 := 0
    reg_mstatus.ie := false
    reg_mstatus.prv := PRV_M
    reg_mstatus.ie1 := false
    reg_mstatus.prv1 := PRV_M /* hard-wired to M when missing user mode */
    reg_mstatus.ie2 := false  /* hard-wired to 0 when missing supervisor mode */
    reg_mstatus.prv2 := PRV_U /* hard-wired to 0 when missing supervisor mode */
    reg_mstatus.ie3 := false  /* hard-wired to 0 when missing hypervisor mode */
    reg_mstatus.prv3 := PRV_U /* hard-wired to 0 when missing hypervisor mode */
    reg_mstatus.mprv := false
    reg_mstatus.vm := 0
    reg_mstatus.fs := 0
    reg_mstatus.xs := 0
    reg_mstatus.sd_rv32 := false
    reg_mstatus.sd := false
  }
}
