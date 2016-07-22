// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import Instructions._
import cde.{Parameters, Field}
import uncore.devices._
import junctions.AddrMap

class MStatus extends Bundle {
  val debug = Bool() // not truly part of mstatus, but convenient
  val prv = UInt(width = PRV.SZ) // not truly part of mstatus, but convenient
  val sd = Bool()
  val zero3 = UInt(width = 31)
  val sd_rv32 = Bool()
  val zero2 = UInt(width = 2)
  val vm = UInt(width = 5)
  val zero1 = UInt(width = 4)
  val mxr = Bool()
  val pum = Bool()
  val mprv = Bool()
  val xs = UInt(width = 2)
  val fs = UInt(width = 2)
  val mpp = UInt(width = 2)
  val hpp = UInt(width = 2)
  val spp = UInt(width = 1)
  val mpie = Bool()
  val hpie = Bool()
  val spie = Bool()
  val upie = Bool()
  val mie = Bool()
  val hie = Bool()
  val sie = Bool()
  val uie = Bool()
}

class DCSR extends Bundle {
  val xdebugver = UInt(width = 2)
  val ndreset = Bool()
  val fullreset = Bool()
  val hwbpcount = UInt(width = 12)
  val ebreakm = Bool()
  val ebreakh = Bool()
  val ebreaks = Bool()
  val ebreaku = Bool()
  val zero2 = Bool()
  val stopcycle = Bool()
  val stoptime = Bool()
  val cause = UInt(width = 3)
  val debugint = Bool()
  val zero1 = Bool()
  val halt = Bool()
  val step = Bool()
  val prv = UInt(width = PRV.SZ)
}

class MIP extends Bundle {
  val rocc = Bool()
  val meip = Bool()
  val heip = Bool()
  val seip = Bool()
  val ueip = Bool()
  val mtip = Bool()
  val htip = Bool()
  val stip = Bool()
  val utip = Bool()
  val msip = Bool()
  val hsip = Bool()
  val ssip = Bool()
  val usip = Bool()
}

class PTBR(implicit p: Parameters) extends CoreBundle()(p) {
  require(maxPAddrBits - pgIdxBits + asIdBits <= xLen)
  val asid = UInt(width = asIdBits)
  val ppn = UInt(width = maxPAddrBits - pgIdxBits)
}

object PRV
{
  val SZ = 2
  val U = 0
  val S = 1
  val H = 2
  val M = 3
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

  val ADDRSZ = 12
}

class CSRFileIO(implicit p: Parameters) extends CoreBundle {
  val prci = new PRCITileIO().flip
  val rw = new Bundle {
    val addr = UInt(INPUT, CSR.ADDRSZ)
    val cmd = Bits(INPUT, CSR.SZ)
    val rdata = Bits(OUTPUT, xLen)
    val wdata = Bits(INPUT, xLen)
  }

  val csr_stall = Bool(OUTPUT)
  val csr_xcpt = Bool(OUTPUT)
  val eret = Bool(OUTPUT)
  val singleStep = Bool(OUTPUT)

  val status = new MStatus().asOutput
  val ptbr = new PTBR().asOutput
  val evec = UInt(OUTPUT, vaddrBitsExtended)
  val exception = Bool(INPUT)
  val retire = UInt(INPUT, log2Up(1+retireWidth))
  val custom_mrw_csrs = Vec(nCustomMrwCsrs, UInt(INPUT, xLen))
  val cause = UInt(INPUT, xLen)
  val pc = UInt(INPUT, vaddrBitsExtended)
  val badaddr = UInt(INPUT, vaddrBitsExtended)
  val fatc = Bool(OUTPUT)
  val time = UInt(OUTPUT, xLen)
  val fcsr_rm = Bits(OUTPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ)).flip
  val rocc = new RoCCInterface().flip
  val interrupt = Bool(OUTPUT)
  val interrupt_cause = UInt(OUTPUT, xLen)
  val bp = Vec(p(NBreakpoints), new BP).asOutput
}

class CSRFile(implicit p: Parameters) extends CoreModule()(p)
{
  val io = new CSRFileIO

  val reset_mstatus = Wire(init=new MStatus().fromBits(0))
  reset_mstatus.mpp := PRV.M
  reset_mstatus.prv := PRV.M
  val reg_mstatus = Reg(init=reset_mstatus)

  val reset_dcsr = Wire(init=new DCSR().fromBits(0))
  reset_dcsr.xdebugver := 1
  reset_dcsr.prv := PRV.M
  val reg_dcsr = Reg(init=reset_dcsr)

  val (supported_interrupts, delegable_interrupts) = {
    val sup = Wire(init=new MIP().fromBits(0))
    sup.ssip := Bool(p(UseVM))
    sup.msip := true
    sup.stip := Bool(p(UseVM))
    sup.mtip := true
    sup.meip := true
    sup.seip := Bool(p(UseVM))
    sup.rocc := usingRoCC

    val del = Wire(init=sup)
    del.msip := false
    del.mtip := false
    del.meip := false

    (sup.toBits, del.toBits)
  }
  val delegable_exceptions = UInt(Seq(
    Causes.misaligned_fetch,
    Causes.fault_fetch,
    Causes.breakpoint,
    Causes.fault_load,
    Causes.fault_store,
    Causes.user_ecall).map(1 << _).sum)

  val exception = io.exception || io.csr_xcpt
  val reg_debug = Reg(init=Bool(false))
  val reg_dpc = Reg(UInt(width = vaddrBitsExtended))
  val reg_dscratch = Reg(UInt(width = xLen))

  val reg_singleStepped = Reg(Bool())
  when (io.retire(0) || exception) { reg_singleStepped := true }
  when (!io.singleStep) { reg_singleStepped := false }
  assert(!io.singleStep || io.retire <= UInt(1))
  assert(!reg_singleStepped || io.retire === UInt(0))

  val reg_tdrselect = Reg(new TDRSelect)
  val reg_bp = Reg(Vec(1 << log2Up(p(NBreakpoints)), new BP))

  val reg_mie = Reg(init=UInt(0, xLen))
  val reg_mideleg = Reg(init=UInt(0, xLen))
  val reg_medeleg = Reg(init=UInt(0, xLen))
  val reg_mip = Reg(new MIP)
  val reg_mepc = Reg(UInt(width = vaddrBitsExtended))
  val reg_mcause = Reg(Bits(width = xLen))
  val reg_mbadaddr = Reg(UInt(width = vaddrBitsExtended))
  val reg_mscratch = Reg(Bits(width = xLen))
  val reg_mtvec = Reg(init=UInt(p(MtvecInit), paddrBits min xLen))

  val reg_sepc = Reg(UInt(width = vaddrBitsExtended))
  val reg_scause = Reg(Bits(width = xLen))
  val reg_sbadaddr = Reg(UInt(width = vaddrBitsExtended))
  val reg_sscratch = Reg(Bits(width = xLen))
  val reg_stvec = Reg(UInt(width = vaddrBits))
  val reg_sptbr = Reg(new PTBR)
  val reg_wfi = Reg(init=Bool(false))

  val reg_fflags = Reg(UInt(width = 5))
  val reg_frm = Reg(UInt(width = 3))

  val reg_instret = WideCounter(64, io.retire)
  val reg_cycle: UInt = if (enableCommitLog) { reg_instret } else { WideCounter(64) }

  val mip = Wire(init=reg_mip)
  mip.rocc := io.rocc.interrupt
  val read_mip = mip.toBits & supported_interrupts

  val pending_interrupts = read_mip & reg_mie
  val m_interrupts = Mux(!reg_debug && (reg_mstatus.prv < PRV.M || (reg_mstatus.prv === PRV.M && reg_mstatus.mie)), pending_interrupts & ~reg_mideleg, UInt(0))
  val s_interrupts = Mux(!reg_debug && (reg_mstatus.prv < PRV.S || (reg_mstatus.prv === PRV.S && reg_mstatus.sie)), pending_interrupts & reg_mideleg, UInt(0))
  val all_interrupts = m_interrupts | s_interrupts
  val interruptMSB = BigInt(1) << (xLen-1)
  val interruptCause = interruptMSB + PriorityEncoder(all_interrupts)
  io.interrupt := all_interrupts.orR && !io.singleStep || reg_singleStepped
  io.interrupt_cause := interruptCause
  io.bp := reg_bp take p(NBreakpoints)

  val debugIntCause = reg_mip.getWidth
  // debug interrupts are only masked by being in debug mode
  when (Bool(usingDebug) && reg_dcsr.debugint && !reg_debug) {
    io.interrupt := true
    io.interrupt_cause := interruptMSB + debugIntCause
  }

  val system_insn = io.rw.cmd === CSR.I
  val cpu_ren = io.rw.cmd =/= CSR.N && !system_insn

  val isa_string = "IM" +
    (if (usingVM) "S" else "") +
    (if (usingUser) "U" else "") +
    (if (usingAtomics) "A" else "") +
    (if (usingFPU) "FD" else "") +
    (if (usingRoCC) "X" else "")
  val isa = (BigInt(log2Ceil(xLen) - 4) << (xLen-2)) |
    isa_string.map(x => 1 << (x - 'A')).reduce(_|_)
  val read_mstatus = io.status.toBits()(xLen-1,0)

  val read_mapping = collection.mutable.LinkedHashMap[Int,Bits](
    CSRs.tdrselect -> reg_tdrselect.toBits,
    CSRs.tdrdata1 -> reg_bp(reg_tdrselect.tdrindex).control.toBits,
    CSRs.tdrdata2 -> reg_bp(reg_tdrselect.tdrindex).address,
    CSRs.mimpid -> UInt(0),
    CSRs.marchid -> UInt(0),
    CSRs.mvendorid -> UInt(0),
    CSRs.mcycle -> reg_cycle,
    CSRs.minstret -> reg_instret,
    CSRs.mucounteren -> UInt(0),
    CSRs.mutime_delta -> UInt(0),
    CSRs.mucycle_delta -> UInt(0),
    CSRs.muinstret_delta -> UInt(0),
    CSRs.misa -> UInt(isa),
    CSRs.mstatus -> read_mstatus,
    CSRs.mtvec -> reg_mtvec,
    CSRs.mip -> read_mip,
    CSRs.mie -> reg_mie,
    CSRs.mideleg -> reg_mideleg,
    CSRs.medeleg -> reg_medeleg,
    CSRs.mscratch -> reg_mscratch,
    CSRs.mepc -> reg_mepc.sextTo(xLen),
    CSRs.mbadaddr -> reg_mbadaddr.sextTo(xLen),
    CSRs.mcause -> reg_mcause,
    CSRs.mhartid -> io.prci.id)

  if (usingDebug) {
    read_mapping += CSRs.dcsr -> reg_dcsr.toBits
    read_mapping += CSRs.dpc -> reg_dpc.toBits
    read_mapping += CSRs.dscratch -> reg_dscratch.toBits
  }

  if (usingFPU) {
    read_mapping += CSRs.fflags -> reg_fflags
    read_mapping += CSRs.frm -> reg_frm
    read_mapping += CSRs.fcsr -> Cat(reg_frm, reg_fflags)
  }

  if (usingVM) {
    val read_sie = reg_mie & reg_mideleg
    val read_sip = read_mip & reg_mideleg
    val read_sstatus = Wire(init=io.status)
    read_sstatus.vm := 0
    read_sstatus.mprv := 0
    read_sstatus.mpp := 0
    read_sstatus.hpp := 0
    read_sstatus.mpie := 0
    read_sstatus.hpie := 0
    read_sstatus.mie := 0
    read_sstatus.hie := 0

    read_mapping += CSRs.sstatus -> (read_sstatus.toBits())(xLen-1,0)
    read_mapping += CSRs.sip -> read_sip.toBits
    read_mapping += CSRs.sie -> read_sie.toBits
    read_mapping += CSRs.sscratch -> reg_sscratch
    read_mapping += CSRs.scause -> reg_scause
    read_mapping += CSRs.sbadaddr -> reg_sbadaddr.sextTo(xLen)
    read_mapping += CSRs.sptbr -> reg_sptbr.toBits
    read_mapping += CSRs.sepc -> reg_sepc.sextTo(xLen)
    read_mapping += CSRs.stvec -> reg_stvec.sextTo(xLen)
    read_mapping += CSRs.mscounteren -> UInt(0)
    read_mapping += CSRs.mstime_delta -> UInt(0)
    read_mapping += CSRs.mscycle_delta -> UInt(0)
    read_mapping += CSRs.msinstret_delta -> UInt(0)
  }

  if (xLen == 32) {
    read_mapping += CSRs.mcycleh -> (reg_cycle >> 32)
    read_mapping += CSRs.minstreth -> (reg_instret >> 32)
    read_mapping += CSRs.mutime_deltah -> UInt(0)
    read_mapping += CSRs.mucycle_deltah -> UInt(0)
    read_mapping += CSRs.muinstret_deltah -> UInt(0)
    if (usingVM) {
      read_mapping += CSRs.mstime_deltah -> UInt(0)
      read_mapping += CSRs.mscycle_deltah -> UInt(0)
      read_mapping += CSRs.msinstret_deltah -> UInt(0)
    }
  }

  for (i <- 0 until nCustomMrwCsrs) {
    val addr = 0xff0 + i
    require(addr < (1 << CSR.ADDRSZ))
    require(!read_mapping.contains(addr), "custom MRW CSR address " + i + " is already in use")
    read_mapping += addr -> io.custom_mrw_csrs(i)
  }

  for ((addr, i) <- roccCsrs.zipWithIndex) {
    require(!read_mapping.contains(addr), "RoCC: CSR address " + addr + " is already in use")
    read_mapping += addr -> io.rocc.csr.rdata(i)
  }

  val decoded_addr = read_mapping map { case (k, v) => k -> (io.rw.addr === k) }

  val addr_valid = decoded_addr.values.reduce(_||_)
  val fp_csr =
    if (usingFPU) decoded_addr(CSRs.fflags) || decoded_addr(CSRs.frm) || decoded_addr(CSRs.fcsr)
    else Bool(false)
  val csr_debug = Bool(usingDebug) && io.rw.addr(5)
  val csr_addr_priv = Cat(io.rw.addr(6,5).andR, io.rw.addr(9,8))
  val priv_sufficient = Cat(reg_debug, reg_mstatus.prv) >= csr_addr_priv
  val read_only = io.rw.addr(11,10).andR
  val cpu_wen = cpu_ren && io.rw.cmd =/= CSR.R && priv_sufficient
  val wen = cpu_wen && !read_only

  val wdata = (Mux((io.rw.cmd === CSR.S || io.rw.cmd === CSR.C), io.rw.rdata, UInt(0)) |
               Mux(io.rw.cmd =/= CSR.C, io.rw.wdata, UInt(0))) &
              ~Mux(io.rw.cmd === CSR.C, io.rw.wdata, UInt(0))

  val do_system_insn = priv_sufficient && system_insn
  val opcode = UInt(1) << io.rw.addr(2,0)
  val insn_call = do_system_insn && opcode(0)
  val insn_break = do_system_insn && opcode(1)
  val insn_ret = do_system_insn && opcode(2)
  val insn_sfence_vm = do_system_insn && opcode(4)
  val insn_wfi = do_system_insn && opcode(5)

  io.csr_xcpt := (cpu_wen && read_only) ||
    (cpu_ren && (!priv_sufficient || !addr_valid || fp_csr && !io.status.fs.orR)) ||
    (system_insn && !priv_sufficient) ||
    insn_call || insn_break

  when (insn_wfi) { reg_wfi := true }
  when (pending_interrupts.orR) { reg_wfi := false }

  val cause =
    Mux(!io.csr_xcpt, io.cause,
    Mux(insn_call, reg_mstatus.prv + Causes.user_ecall,
    Mux[UInt](insn_break, Causes.breakpoint, Causes.illegal_instruction)))
  val cause_lsbs = cause(log2Up(xLen)-1,0)
  val causeIsDebugInt = cause(xLen-1) && cause_lsbs === debugIntCause
  val causeIsDebugBreak = cause === Causes.breakpoint && Cat(reg_dcsr.ebreakm, reg_dcsr.ebreakh, reg_dcsr.ebreaks, reg_dcsr.ebreaku)(reg_mstatus.prv)
  val trapToDebug = Bool(usingDebug) && (reg_singleStepped || causeIsDebugInt || causeIsDebugBreak || reg_debug)
  val delegate = Bool(p(UseVM)) && reg_mstatus.prv < PRV.M && Mux(cause(xLen-1), reg_mideleg(cause_lsbs), reg_medeleg(cause_lsbs))
  val debugTVec = Mux(reg_debug, UInt(0x808), UInt(0x800))
  val tvec = Mux(trapToDebug, debugTVec, Mux(delegate, reg_stvec.sextTo(vaddrBitsExtended), reg_mtvec))
  val epc = Mux(csr_debug, reg_dpc, Mux(Bool(p(UseVM)) && !csr_addr_priv(1), reg_sepc, reg_mepc))
  io.fatc := insn_sfence_vm
  io.evec := Mux(exception, tvec, epc)
  io.ptbr := reg_sptbr
  io.eret := insn_ret
  io.singleStep := reg_dcsr.step && !reg_debug
  io.status := reg_mstatus
  io.status.sd := io.status.fs.andR || io.status.xs.andR
  io.status.debug := reg_debug
  if (xLen == 32)
    io.status.sd_rv32 := io.status.sd

  when (exception) {
    val epc = ~(~io.pc | (coreInstBytes-1))
    val pie = read_mstatus(reg_mstatus.prv)

    when (trapToDebug) {
      reg_debug := true
      reg_dpc := epc
      reg_dcsr.cause := Mux(reg_singleStepped, UInt(4), Mux(causeIsDebugInt, UInt(3), UInt(1)))
      reg_dcsr.prv := reg_mstatus.prv
    }.elsewhen (delegate) {
      reg_sepc := epc
      reg_scause := cause
      reg_sbadaddr := io.badaddr
      reg_mstatus.spie := pie
      reg_mstatus.spp := reg_mstatus.prv
      reg_mstatus.sie := false
      reg_mstatus.prv := PRV.S
    }.otherwise {
      reg_mepc := epc
      reg_mcause := cause
      reg_mbadaddr := io.badaddr
      reg_mstatus.mpie := pie
      reg_mstatus.mpp := reg_mstatus.prv
      reg_mstatus.mie := false
      reg_mstatus.prv := PRV.M
    }
  }
  
  when (insn_ret) {
    when (Bool(p(UseVM)) && !csr_addr_priv(1)) {
      when (reg_mstatus.spp.toBool) { reg_mstatus.sie := reg_mstatus.spie }
      reg_mstatus.spie := false
      reg_mstatus.spp := PRV.U
      reg_mstatus.prv := reg_mstatus.spp
    }.elsewhen (csr_debug) {
      reg_mstatus.prv := reg_dcsr.prv
      reg_debug := false
    }.otherwise {
      when (reg_mstatus.mpp(1)) { reg_mstatus.mie := reg_mstatus.mpie }
      .elsewhen (Bool(usingVM) && reg_mstatus.mpp(0)) { reg_mstatus.sie := reg_mstatus.mpie }
      reg_mstatus.mpie := false
      reg_mstatus.mpp := PRV.U
      reg_mstatus.prv := reg_mstatus.mpp
    }
  }

  assert(PopCount(insn_ret :: io.exception :: io.csr_xcpt :: Nil) <= 1, "these conditions must be mutually exclusive")

  io.time := reg_cycle
  io.csr_stall := reg_wfi

  io.rw.rdata := Mux1H(for ((k, v) <- read_mapping) yield decoded_addr(k) -> v)

  io.fcsr_rm := reg_frm
  when (io.fcsr_flags.valid) {
    reg_fflags := reg_fflags | io.fcsr_flags.bits
  }

  val supportedModes = Vec((PRV.M +: (if (usingUser) Some(PRV.U) else None) ++: (if (usingVM) Seq(PRV.S) else Nil)).map(UInt(_)))

  when (wen) {
    when (decoded_addr(CSRs.mstatus)) {
      val new_mstatus = new MStatus().fromBits(wdata)
      reg_mstatus.mie := new_mstatus.mie
      reg_mstatus.mpie := new_mstatus.mpie

      if (supportedModes.size > 1) {
        reg_mstatus.mprv := new_mstatus.mprv
        when (supportedModes contains new_mstatus.mpp) { reg_mstatus.mpp := new_mstatus.mpp }
        if (supportedModes.size > 2) {
          reg_mstatus.mxr := new_mstatus.mxr
          reg_mstatus.pum := new_mstatus.pum
          reg_mstatus.spp := new_mstatus.spp
          reg_mstatus.spie := new_mstatus.spie
          reg_mstatus.sie := new_mstatus.sie
        }
      }

      if (usingVM) {
        require(if (xLen == 32) pgLevels == 2 else pgLevels > 2 && pgLevels < 6)
        val vm_on = 6 + pgLevels // TODO Sv48 support should imply Sv39 support
        when (new_mstatus.vm === 0) { reg_mstatus.vm := 0 }
        when (new_mstatus.vm === vm_on) { reg_mstatus.vm := vm_on }
      }
      if (usingVM || usingFPU) reg_mstatus.fs := Fill(2, new_mstatus.fs.orR)
      if (usingRoCC) reg_mstatus.xs := Fill(2, new_mstatus.xs.orR)
    }
    when (decoded_addr(CSRs.mip)) {
      val new_mip = new MIP().fromBits(wdata)
      if (usingVM) {
        reg_mip.ssip := new_mip.ssip
        reg_mip.stip := new_mip.stip
      }
    }
    when (decoded_addr(CSRs.mie))      { reg_mie := wdata & supported_interrupts }
    when (decoded_addr(CSRs.mepc))     { reg_mepc := ~(~wdata | (coreInstBytes-1)) }
    when (decoded_addr(CSRs.mscratch)) { reg_mscratch := wdata }
    if (p(MtvecWritable))
      when (decoded_addr(CSRs.mtvec))  { reg_mtvec := wdata >> 2 << 2 }
    when (decoded_addr(CSRs.mcause))   { reg_mcause := wdata & UInt((BigInt(1) << (xLen-1)) + 31) /* only implement 5 LSBs and MSB */ }
    when (decoded_addr(CSRs.mbadaddr)) { reg_mbadaddr := wdata(vaddrBitsExtended-1,0) }
    if (usingFPU) {
      when (decoded_addr(CSRs.fflags)) { reg_fflags := wdata }
      when (decoded_addr(CSRs.frm))    { reg_frm := wdata }
      when (decoded_addr(CSRs.fcsr))   { reg_fflags := wdata; reg_frm := wdata >> reg_fflags.getWidth }
    }
    if (usingDebug) {
      when (decoded_addr(CSRs.dcsr)) {
        val new_dcsr = new DCSR().fromBits(wdata)
        reg_dcsr.halt := new_dcsr.halt
        reg_dcsr.step := new_dcsr.step
        reg_dcsr.ebreakm := new_dcsr.ebreakm
        if (usingVM) reg_dcsr.ebreaks := new_dcsr.ebreaks
        if (usingUser) reg_dcsr.ebreaku := new_dcsr.ebreaku
        if (supportedModes.size > 1) reg_dcsr.prv := new_dcsr.prv
      }
      when (decoded_addr(CSRs.dpc))      { reg_dpc := ~(~wdata | (coreInstBytes-1)) }
      when (decoded_addr(CSRs.dscratch)) { reg_dscratch := wdata }
    }
    if (usingVM) {
      when (decoded_addr(CSRs.sstatus)) {
        val new_sstatus = new MStatus().fromBits(wdata)
        reg_mstatus.sie := new_sstatus.sie
        reg_mstatus.spie := new_sstatus.spie
        reg_mstatus.spp := new_sstatus.spp
        reg_mstatus.pum := new_sstatus.pum
        reg_mstatus.fs := Fill(2, new_sstatus.fs.orR) // even without an FPU
        if (usingRoCC) reg_mstatus.xs := Fill(2, new_sstatus.xs.orR)
      }
      when (decoded_addr(CSRs.sip)) {
        val new_sip = new MIP().fromBits(wdata)
        reg_mip.ssip := new_sip.ssip
      }
      when (decoded_addr(CSRs.sie))      { reg_mie := (reg_mie & ~reg_mideleg) | (wdata & reg_mideleg) }
      when (decoded_addr(CSRs.sscratch)) { reg_sscratch := wdata }
      when (decoded_addr(CSRs.sptbr))    { reg_sptbr.ppn := wdata(ppnBits-1,0) }
      when (decoded_addr(CSRs.sepc))     { reg_sepc := ~(~wdata | (coreInstBytes-1)) }
      when (decoded_addr(CSRs.stvec))    { reg_stvec := wdata >> 2 << 2 }
      when (decoded_addr(CSRs.scause))   { reg_scause := wdata & UInt((BigInt(1) << (xLen-1)) + 31) /* only implement 5 LSBs and MSB */ }
      when (decoded_addr(CSRs.sbadaddr)) { reg_sbadaddr := wdata(vaddrBitsExtended-1,0) }
      when (decoded_addr(CSRs.mideleg))  { reg_mideleg := wdata & delegable_interrupts }
      when (decoded_addr(CSRs.medeleg))  { reg_medeleg := wdata & delegable_exceptions }
    }
    if (p(NBreakpoints) > 0) {
      val newTDR = new TDRSelect().fromBits(wdata)
      when (decoded_addr(CSRs.tdrselect)) { reg_tdrselect.tdrindex := newTDR.tdrindex }

      when (reg_tdrselect.tdrmode || reg_debug) {
        when (decoded_addr(CSRs.tdrdata1)) {
          val newBPC = new BPControl().fromBits(wdata)
          reg_bp(reg_tdrselect.tdrindex).control := newBPC
          reg_bp(reg_tdrselect.tdrindex).control.bpmatch := newBPC.bpmatch & 2 /* exact/NAPOT only */
        }
        when (decoded_addr(CSRs.tdrdata2)) { reg_bp(reg_tdrselect.tdrindex).address := wdata }
      }
    }
  }

  reg_mip := io.prci.interrupts
  reg_dcsr.debugint := io.prci.interrupts.debug
  reg_dcsr.hwbpcount := UInt(p(NBreakpoints))

  io.rocc.csr.waddr := io.rw.addr
  io.rocc.csr.wdata := wdata
  io.rocc.csr.wen := wen

  if (!usingUser) {
    reg_mstatus.mpp := PRV.M
    reg_mstatus.prv := PRV.M
    reg_mstatus.mprv := false
  }

  reg_sptbr.asid := 0
  reg_tdrselect.reserved := 0
  reg_tdrselect.tdrmode := true // TODO support D-mode breakpoint theft
  if (reg_bp.isEmpty) reg_tdrselect.tdrindex := 0
  for (bpc <- reg_bp map {_.control}) {
    bpc.tdrtype := bpc.tdrType
    bpc.bpamaskmax := bpc.bpaMaskMax
    bpc.reserved := 0
    bpc.bpaction := 0
    bpc.h := false
    if (!usingVM) bpc.s := false
    if (!usingUser) bpc.u := false
    if (!usingVM && !usingUser) bpc.m := true
    when (reset) {
      bpc.r := false
      bpc.w := false
      bpc.x := false
    }
  }
  for (bp <- reg_bp drop p(NBreakpoints))
    bp := new BP().fromBits(0)
}
