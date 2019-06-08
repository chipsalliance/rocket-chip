// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import scala.collection.mutable.LinkedHashMap
import Instructions._

class MStatus extends Bundle {
  // not truly part of mstatus, but convenient
  val debug = Bool()
  val cease = Bool()
  val isa = UInt(width = 32)

  val dprv = UInt(width = PRV.SZ) // effective privilege for data accesses
  val prv = UInt(width = PRV.SZ) // not truly part of mstatus, but convenient
  val sd = Bool()
  val zero2 = UInt(width = 27)
  val sxl = UInt(width = 2)
  val uxl = UInt(width = 2)
  val sd_rv32 = Bool()
  val zero1 = UInt(width = 8)
  val tsr = Bool()
  val tw = Bool()
  val tvm = Bool()
  val mxr = Bool()
  val sum = Bool()
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
  val zero4 = UInt(width=2)
  val zero3 = UInt(width = 12)
  val ebreakm = Bool()
  val ebreakh = Bool()
  val ebreaks = Bool()
  val ebreaku = Bool()
  val zero2 = Bool()
  val stopcycle = Bool()
  val stoptime = Bool()
  val cause = UInt(width = 3)
  val zero1 = UInt(width=3)
  val step = Bool()
  val prv = UInt(width = PRV.SZ)
}

class MIP(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreParameters {
  val lip = Vec(coreParams.nLocalInterrupts, Bool())
  val zero2 = Bool()
  val debug = Bool() // keep in sync with CSR.debugIntCause
  val zero1 = Bool()
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
  def additionalPgLevels = mode.extract(log2Ceil(pgLevels-minPgLevels+1)-1, 0)
  def pgLevelsToMode(i: Int) = (xLen, i) match {
    case (32, 2) => 1
    case (64, x) if x >= 3 && x <= 6 => x + 5
  }
  val (modeBits, maxASIdBits) = xLen match {
    case 32 => (1, 9)
    case 64 => (4, 16)
  }
  require(modeBits + maxASIdBits + maxPAddrBits - pgIdxBits == xLen)

  val mode = UInt(width = modeBits)
  val asid = UInt(width = maxASIdBits)
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
  def X = BitPat.dontCare(SZ)
  def N = UInt(0,SZ)
  def R = UInt(2,SZ)
  def I = UInt(4,SZ)
  def W = UInt(5,SZ)
  def S = UInt(6,SZ)
  def C = UInt(7,SZ)

  // mask a CSR cmd with a valid bit
  def maskCmd(valid: Bool, cmd: UInt): UInt = {
    // all commands less than CSR.I are treated by CSRFile as NOPs
    cmd & ~Mux(valid, 0.U, CSR.I)
  }

  val ADDRSZ = 12
  def busErrorIntCause = 128
  def debugIntCause = 14 // keep in sync with MIP.debug
  def debugTriggerCause = {
    val res = debugIntCause
    require(!(Causes.all contains res))
    res
  }

  val firstCtr = CSRs.cycle
  val firstCtrH = CSRs.cycleh
  val firstHPC = CSRs.hpmcounter3
  val firstHPCH = CSRs.hpmcounter3h
  val firstHPE = CSRs.mhpmevent3
  val firstMHPC = CSRs.mhpmcounter3
  val firstMHPCH = CSRs.mhpmcounter3h
  val firstHPM = 3
  val nCtr = 32
  val nHPM = nCtr - firstHPM
  val hpmWidth = 40

  val maxPMPs = 16
}

class PerfCounterIO(implicit p: Parameters) extends CoreBundle
    with HasCoreParameters {
  val eventSel = UInt(OUTPUT, xLen)
  val inc = UInt(INPUT, log2Ceil(1+retireWidth))
}

class TracedInstruction(implicit p: Parameters) extends CoreBundle with Clocked {
  val valid = Bool()
  val iaddr = UInt(width = coreMaxAddrBits)
  val insn = UInt(width = iLen)
  val priv = UInt(width = 3)
  val exception = Bool()
  val interrupt = Bool()
  val cause = UInt(width = log2Ceil(1 + CSR.busErrorIntCause))
  val tval = UInt(width = coreMaxAddrBits max iLen)
}

class CSRDecodeIO extends Bundle {
  val csr = UInt(INPUT, CSR.ADDRSZ)
  val fp_illegal = Bool(OUTPUT)
  val fp_csr = Bool(OUTPUT)
  val rocc_illegal = Bool(OUTPUT)
  val read_illegal = Bool(OUTPUT)
  val write_illegal = Bool(OUTPUT)
  val write_flush = Bool(OUTPUT)
  val system_illegal = Bool(OUTPUT)
}

class CSRFileIO(implicit p: Parameters) extends CoreBundle
    with HasCoreParameters {
  val ungated_clock = Clock().asInput
  val interrupts = new CoreInterrupts().asInput
  val hartid = UInt(INPUT, hartIdLen)
  val rw = new Bundle {
    val addr = UInt(INPUT, CSR.ADDRSZ)
    val cmd = Bits(INPUT, CSR.SZ)
    val rdata = Bits(OUTPUT, xLen)
    val wdata = Bits(INPUT, xLen)
  }

  val decode = Vec(decodeWidth, new CSRDecodeIO)

  val csr_stall = Bool(OUTPUT)
  val eret = Bool(OUTPUT)
  val singleStep = Bool(OUTPUT)

  val status = new MStatus().asOutput
  val ptbr = new PTBR().asOutput
  val evec = UInt(OUTPUT, vaddrBitsExtended)
  val exception = Bool(INPUT)
  val retire = UInt(INPUT, log2Up(1+retireWidth))
  val cause = UInt(INPUT, xLen)
  val pc = UInt(INPUT, vaddrBitsExtended)
  val tval = UInt(INPUT, vaddrBitsExtended)
  val time = UInt(OUTPUT, xLen)
  val fcsr_rm = Bits(OUTPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ)).flip
  val set_fs_dirty = coreParams.haveFSDirty.option(Bool(INPUT))
  val rocc_interrupt = Bool(INPUT)
  val interrupt = Bool(OUTPUT)
  val interrupt_cause = UInt(OUTPUT, xLen)
  val bp = Vec(nBreakpoints, new BP).asOutput
  val pmp = Vec(nPMPs, new PMP).asOutput
  val counters = Vec(nPerfCounters, new PerfCounterIO)
  val csrw_counter = UInt(OUTPUT, CSR.nCtr)
  val inst = Vec(retireWidth, UInt(width = iLen)).asInput
  val trace = Vec(retireWidth, new TracedInstruction).asOutput
}

class CSRFile(
  perfEventSets: EventSets = new EventSets(Seq()),
  customCSRs: Seq[CustomCSR] = Nil)(implicit p: Parameters)
    extends CoreModule()(p)
    with HasCoreParameters {
  val io = new CSRFileIO {
    val customCSRs = Vec(CSRFile.this.customCSRs.size, new CustomCSRIO).asOutput
  }

  val reset_mstatus = Wire(init=new MStatus().fromBits(0))
  reset_mstatus.mpp := PRV.M
  reset_mstatus.prv := PRV.M
  val reg_mstatus = Reg(init=reset_mstatus)

  val new_prv = Wire(init = reg_mstatus.prv)
  reg_mstatus.prv := legalizePrivilege(new_prv)

  val reset_dcsr = Wire(init=new DCSR().fromBits(0))
  reset_dcsr.xdebugver := 1
  reset_dcsr.prv := PRV.M
  val reg_dcsr = Reg(init=reset_dcsr)

  val (supported_interrupts, delegable_interrupts) = {
    val sup = Wire(new MIP)
    sup.usip := false
    sup.ssip := Bool(usingVM)
    sup.hsip := false
    sup.msip := true
    sup.utip := false
    sup.stip := Bool(usingVM)
    sup.htip := false
    sup.mtip := true
    sup.ueip := false
    sup.seip := Bool(usingVM)
    sup.heip := false
    sup.meip := true
    sup.rocc := usingRoCC
    sup.zero1 := false
    sup.debug := false
    sup.zero2 := false
    sup.lip foreach { _ := true }
    val supported_high_interrupts = if (io.interrupts.buserror.nonEmpty) UInt(BigInt(1) << CSR.busErrorIntCause) else 0.U

    val del = Wire(init=sup)
    del.msip := false
    del.mtip := false
    del.meip := false

    (sup.asUInt | supported_high_interrupts, del.asUInt)
  }
  val delegable_exceptions = UInt(Seq(
    Causes.misaligned_fetch,
    Causes.fetch_page_fault,
    Causes.breakpoint,
    Causes.load_page_fault,
    Causes.store_page_fault,
    Causes.misaligned_load,
    Causes.misaligned_store,
    Causes.illegal_instruction,
    Causes.user_ecall).map(1 << _).sum)

  val reg_debug = Reg(init=Bool(false))
  val reg_dpc = Reg(UInt(width = vaddrBitsExtended))
  val reg_dscratch = Reg(UInt(width = xLen))
  val reg_singleStepped = Reg(Bool())

  val reg_tselect = Reg(UInt(width = log2Up(nBreakpoints)))
  val reg_bp = Reg(Vec(1 << log2Up(nBreakpoints), new BP))
  val reg_pmp = Reg(Vec(nPMPs, new PMPReg))

  val reg_mie = Reg(UInt(width = xLen))
  val (reg_mideleg, read_mideleg) = {
    val reg = Reg(UInt(xLen.W))
    (reg, Mux(usingVM, reg & delegable_interrupts, 0.U))
  }
  val (reg_medeleg, read_medeleg) = {
    val reg = Reg(UInt(xLen.W))
    (reg, Mux(usingVM, reg & delegable_exceptions, 0.U))
  }
  val reg_mip = Reg(new MIP)
  val reg_mepc = Reg(UInt(width = vaddrBitsExtended))
  val reg_mcause = Reg(Bits(width = xLen))
  val reg_mtval = Reg(UInt(width = vaddrBitsExtended))
  val reg_mscratch = Reg(Bits(width = xLen))
  val mtvecWidth = paddrBits min xLen
  val reg_mtvec = mtvecInit match {
    case Some(addr) => Reg(init=UInt(addr, mtvecWidth))
    case None => Reg(UInt(width = mtvecWidth))
  }

  val delegable_counters = ((BigInt(1) << (nPerfCounters + CSR.firstHPM)) - 1).U
  val (reg_mcounteren, read_mcounteren) = {
    val reg = Reg(UInt(32.W))
    (reg, Mux(usingUser, reg & delegable_counters, 0.U))
  }
  val (reg_scounteren, read_scounteren) = {
    val reg = Reg(UInt(32.W))
    (reg, Mux(usingVM, reg & delegable_counters, 0.U))
  }

  val reg_sepc = Reg(UInt(width = vaddrBitsExtended))
  val reg_scause = Reg(Bits(width = xLen))
  val reg_stval = Reg(UInt(width = vaddrBitsExtended))
  val reg_sscratch = Reg(Bits(width = xLen))
  val reg_stvec = Reg(UInt(width = vaddrBits))
  val reg_satp = Reg(new PTBR)
  val reg_wfi = withClock(io.ungated_clock) { Reg(init=Bool(false)) }

  val reg_fflags = Reg(UInt(width = 5))
  val reg_frm = Reg(UInt(width = 3))

  val reg_instret = WideCounter(64, io.retire)
  val reg_cycle = if (enableCommitLog) reg_instret else withClock(io.ungated_clock) { WideCounter(64, !io.csr_stall) }
  val reg_hpmevent = io.counters.map(c => Reg(init = UInt(0, xLen)))
  (io.counters zip reg_hpmevent) foreach { case (c, e) => c.eventSel := e }
  val reg_hpmcounter = io.counters.map(c => WideCounter(CSR.hpmWidth, c.inc, reset = false))

  val mip = Wire(init=reg_mip)
  mip.lip := (io.interrupts.lip: Seq[Bool])
  mip.mtip := io.interrupts.mtip
  mip.msip := io.interrupts.msip
  mip.meip := io.interrupts.meip
  // seip is the OR of reg_mip.seip and the actual line from the PLIC
  io.interrupts.seip.foreach { mip.seip := reg_mip.seip || _ }
  mip.rocc := io.rocc_interrupt
  val read_mip = mip.asUInt & supported_interrupts
  val high_interrupts = io.interrupts.buserror.map(_ << CSR.busErrorIntCause).getOrElse(0.U)

  val pending_interrupts = high_interrupts | (read_mip & reg_mie)
  val d_interrupts = io.interrupts.debug << CSR.debugIntCause
  val m_interrupts = Mux(reg_mstatus.prv <= PRV.S || reg_mstatus.mie, ~(~pending_interrupts | read_mideleg), UInt(0))
  val s_interrupts = Mux(reg_mstatus.prv < PRV.S || (reg_mstatus.prv === PRV.S && reg_mstatus.sie), pending_interrupts & read_mideleg, UInt(0))
  val (anyInterrupt, whichInterrupt) = chooseInterrupt(Seq(s_interrupts, m_interrupts, d_interrupts))
  val interruptMSB = BigInt(1) << (xLen-1)
  val interruptCause = UInt(interruptMSB) + whichInterrupt
  io.interrupt := (anyInterrupt && !io.singleStep || reg_singleStepped) && !(reg_debug || io.status.cease)
  io.interrupt_cause := interruptCause
  io.bp := reg_bp take nBreakpoints
  io.pmp := reg_pmp.map(PMP(_))

  val isaMaskString =
    (if (usingMulDiv) "M" else "") +
    (if (usingAtomics) "A" else "") +
    (if (fLen >= 32) "F" else "") +
    (if (fLen >= 64) "D" else "") +
    (if (usingCompressed) "C" else "")
  val isaString = (if (coreParams.useRVE) "E" else "I") +
    isaMaskString +
    "X" + // Custom extensions always present (e.g. CEASE instruction)
    (if (usingVM) "S" else "") +
    (if (usingUser) "U" else "")
  val isaMax = (BigInt(log2Ceil(xLen) - 4) << (xLen-2)) | isaStringToMask(isaString)
  val reg_misa = Reg(init=UInt(isaMax))
  val read_mstatus = io.status.asUInt()(xLen-1,0)
  val read_mtvec = formTVec(reg_mtvec).padTo(xLen)
  val read_stvec = formTVec(reg_stvec).sextTo(xLen)

  val read_mapping = LinkedHashMap[Int,Bits](
    CSRs.tselect -> reg_tselect,
    CSRs.tdata1 -> reg_bp(reg_tselect).control.asUInt,
    CSRs.tdata2 -> reg_bp(reg_tselect).address.sextTo(xLen),
    CSRs.misa -> reg_misa,
    CSRs.mstatus -> read_mstatus,
    CSRs.mtvec -> read_mtvec,
    CSRs.mip -> read_mip,
    CSRs.mie -> reg_mie,
    CSRs.mscratch -> reg_mscratch,
    CSRs.mepc -> readEPC(reg_mepc).sextTo(xLen),
    CSRs.mtval -> reg_mtval.sextTo(xLen),
    CSRs.mcause -> reg_mcause,
    CSRs.mhartid -> io.hartid)

  val debug_csrs = LinkedHashMap[Int,Bits](
    CSRs.dcsr -> reg_dcsr.asUInt,
    CSRs.dpc -> readEPC(reg_dpc).sextTo(xLen),
    CSRs.dscratch -> reg_dscratch.asUInt)

  val fp_csrs = LinkedHashMap[Int,Bits](
    CSRs.fflags -> reg_fflags,
    CSRs.frm -> reg_frm,
    CSRs.fcsr -> Cat(reg_frm, reg_fflags))

  if (usingDebug)
    read_mapping ++= debug_csrs

  if (usingFPU)
    read_mapping ++= fp_csrs

  if (coreParams.haveBasicCounters) {
    read_mapping += CSRs.mcycle -> reg_cycle
    read_mapping += CSRs.minstret -> reg_instret

    for (((e, c), i) <- (reg_hpmevent.padTo(CSR.nHPM, UInt(0))
                         zip reg_hpmcounter.map(x => x: UInt).padTo(CSR.nHPM, UInt(0))) zipWithIndex) {
      read_mapping += (i + CSR.firstHPE) -> e // mhpmeventN
      read_mapping += (i + CSR.firstMHPC) -> c // mhpmcounterN
      if (usingUser) read_mapping += (i + CSR.firstHPC) -> c // hpmcounterN
      if (xLen == 32) {
        read_mapping += (i + CSR.firstMHPCH) -> (c >> 32) // mhpmcounterNh
        if (usingUser) read_mapping += (i + CSR.firstHPCH) -> (c >> 32) // hpmcounterNh
      }
    }

    if (usingUser) {
      read_mapping += CSRs.mcounteren -> read_mcounteren
      read_mapping += CSRs.cycle -> reg_cycle
      read_mapping += CSRs.instret -> reg_instret
    }

    if (xLen == 32) {
      read_mapping += CSRs.mcycleh -> (reg_cycle >> 32)
      read_mapping += CSRs.minstreth -> (reg_instret >> 32)
      if (usingUser) {
        read_mapping += CSRs.cycleh -> (reg_cycle >> 32)
        read_mapping += CSRs.instreth -> (reg_instret >> 32)
      }
    }
  }

  if (usingVM) {
    val read_sie = reg_mie & read_mideleg
    val read_sip = read_mip & read_mideleg
    val read_sstatus = Wire(init = 0.U.asTypeOf(new MStatus))
    read_sstatus.sd := io.status.sd
    read_sstatus.uxl := io.status.uxl
    read_sstatus.sd_rv32 := io.status.sd_rv32
    read_sstatus.mxr := io.status.mxr
    read_sstatus.sum := io.status.sum
    read_sstatus.xs := io.status.xs
    read_sstatus.fs := io.status.fs
    read_sstatus.spp := io.status.spp
    read_sstatus.spie := io.status.spie
    read_sstatus.sie := io.status.sie

    read_mapping += CSRs.sstatus -> (read_sstatus.asUInt())(xLen-1,0)
    read_mapping += CSRs.sip -> read_sip.asUInt
    read_mapping += CSRs.sie -> read_sie.asUInt
    read_mapping += CSRs.sscratch -> reg_sscratch
    read_mapping += CSRs.scause -> reg_scause
    read_mapping += CSRs.stval -> reg_stval.sextTo(xLen)
    read_mapping += CSRs.satp -> reg_satp.asUInt
    read_mapping += CSRs.sepc -> readEPC(reg_sepc).sextTo(xLen)
    read_mapping += CSRs.stvec -> read_stvec
    read_mapping += CSRs.scounteren -> read_scounteren
    read_mapping += CSRs.mideleg -> read_mideleg
    read_mapping += CSRs.medeleg -> read_medeleg
  }

  val pmpCfgPerCSR = xLen / new PMPConfig().getWidth
  def pmpCfgIndex(i: Int) = (xLen / 32) * (i / pmpCfgPerCSR)
  if (reg_pmp.nonEmpty) {
    require(reg_pmp.size <= CSR.maxPMPs)
    val read_pmp = reg_pmp.padTo(CSR.maxPMPs, 0.U.asTypeOf(new PMP))
    for (i <- 0 until read_pmp.size by pmpCfgPerCSR)
      read_mapping += (CSRs.pmpcfg0 + pmpCfgIndex(i)) -> read_pmp.map(_.cfg).slice(i, i + pmpCfgPerCSR).asUInt
    for ((pmp, i) <- read_pmp zipWithIndex)
      read_mapping += (CSRs.pmpaddr0 + i) -> pmp.readAddr
  }

  // implementation-defined CSRs
  val reg_custom = customCSRs.map { csr =>
    require(csr.mask >= 0 && csr.mask.bitLength <= xLen)
    require(!read_mapping.contains(csr.id))
    val reg = csr.init.map(init => RegInit(init.U(xLen.W))).getOrElse(Reg(UInt(xLen.W)))
    read_mapping += csr.id -> reg
    reg
  }

  // mimpid, marchid, and mvendorid are 0 unless overridden by customCSRs
  Seq(CSRs.mimpid, CSRs.marchid, CSRs.mvendorid).foreach(id => read_mapping.getOrElseUpdate(id, 0.U))

  val decoded_addr = read_mapping map { case (k, v) => k -> (io.rw.addr === k) }
  val wdata = readModifyWriteCSR(io.rw.cmd, io.rw.rdata, io.rw.wdata)

  val system_insn = io.rw.cmd === CSR.I
  val decode_table = Seq(        SCALL->       List(Y,N,N,N,N,N),
                                 SBREAK->      List(N,Y,N,N,N,N),
                                 MRET->        List(N,N,Y,N,N,N),
                                 CEASE->       List(N,N,N,Y,N,N),
                                 WFI->         List(N,N,N,N,Y,N)) ++
    usingDebug.option(           DRET->        List(N,N,Y,N,N,N)) ++
    coreParams.haveCFlush.option(CFLUSH_D_L1-> List(N,N,N,N,N,N)) ++
    usingVM.option(              SRET->        List(N,N,Y,N,N,N)) ++
    usingVM.option(              SFENCE_VMA->  List(N,N,N,N,N,Y))

  val insn_call :: insn_break :: insn_ret :: insn_cease :: insn_wfi :: insn_sfence :: Nil =
    DecodeLogic(io.rw.addr << 20, decode_table(0)._2.map(x=>X), decode_table).map(system_insn && _.asBool)

  for (io_dec <- io.decode) {
    val _ :: is_break :: is_ret :: _ :: is_wfi :: is_sfence :: Nil =
      DecodeLogic(io_dec.csr << 20, decode_table(0)._2.map(x=>X), decode_table).map(_.asBool)
    def decodeAny(m: LinkedHashMap[Int,Bits]): Bool = m.map { case(k: Int, _: Bits) => io_dec.csr === k }.reduce(_||_)
    val allow_wfi = Bool(!usingVM) || reg_mstatus.prv > PRV.S || !reg_mstatus.tw
    val allow_sfence_vma = Bool(!usingVM) || reg_mstatus.prv > PRV.S || !reg_mstatus.tvm
    val allow_sret = Bool(!usingVM) || reg_mstatus.prv > PRV.S || !reg_mstatus.tsr
    val counter_addr = io_dec.csr(log2Ceil(read_mcounteren.getWidth)-1, 0)
    val allow_counter = (reg_mstatus.prv > PRV.S || read_mcounteren(counter_addr)) &&
      (!usingVM || reg_mstatus.prv >= PRV.S || read_scounteren(counter_addr))
    io_dec.fp_illegal := io.status.fs === 0 || !reg_misa('f'-'a')
    io_dec.fp_csr := Bool(usingFPU) && DecodeLogic(io_dec.csr, fp_csrs.keys.toList.map(_.U), (read_mapping -- fp_csrs.keys.toList).keys.toList.map(_.U))
    io_dec.rocc_illegal := io.status.xs === 0 || !reg_misa('x'-'a')
    io_dec.read_illegal := reg_mstatus.prv < io_dec.csr(9,8) ||
      !decodeAny(read_mapping) ||
      io_dec.csr === CSRs.satp && !allow_sfence_vma ||
      (io_dec.csr.inRange(CSR.firstCtr, CSR.firstCtr + CSR.nCtr) || io_dec.csr.inRange(CSR.firstCtrH, CSR.firstCtrH + CSR.nCtr)) && !allow_counter ||
      Bool(usingDebug) && decodeAny(debug_csrs) && !reg_debug ||
      io_dec.fp_csr && io_dec.fp_illegal
    io_dec.write_illegal := io_dec.csr(11,10).andR
    io_dec.write_flush := !(io_dec.csr >= CSRs.mscratch && io_dec.csr <= CSRs.mtval || io_dec.csr >= CSRs.sscratch && io_dec.csr <= CSRs.stval)
    io_dec.system_illegal := reg_mstatus.prv < io_dec.csr(9,8) ||
      is_wfi && !allow_wfi ||
      is_ret && !allow_sret ||
      is_sfence && !allow_sfence_vma
  }

  val cause =
    Mux(insn_call, reg_mstatus.prv + Causes.user_ecall,
    Mux[UInt](insn_break, Causes.breakpoint, io.cause))
  val cause_lsbs = cause(io.trace.head.cause.getWidth-1, 0)
  val causeIsDebugInt = cause(xLen-1) && cause_lsbs === CSR.debugIntCause
  val causeIsDebugTrigger = !cause(xLen-1) && cause_lsbs === CSR.debugTriggerCause
  val causeIsDebugBreak = !cause(xLen-1) && insn_break && Cat(reg_dcsr.ebreakm, reg_dcsr.ebreakh, reg_dcsr.ebreaks, reg_dcsr.ebreaku)(reg_mstatus.prv)
  val trapToDebug = Bool(usingDebug) && (reg_singleStepped || causeIsDebugInt || causeIsDebugTrigger || causeIsDebugBreak || reg_debug)
  val debugTVec = Mux(reg_debug, Mux(insn_break, UInt(0x800), UInt(0x808)), UInt(0x800))
  val delegate = Bool(usingVM) && reg_mstatus.prv <= PRV.S && Mux(cause(xLen-1), read_mideleg(cause_lsbs), read_medeleg(cause_lsbs))
  def mtvecBaseAlign = 2
  def mtvecInterruptAlign = {
    require(reg_mip.getWidth <= xLen)
    log2Ceil(xLen)
  }
  val notDebugTVec = {
    val base = Mux(delegate, read_stvec, read_mtvec)
    val interruptOffset = cause(mtvecInterruptAlign-1, 0) << mtvecBaseAlign
    val interruptVec = Cat(base >> (mtvecInterruptAlign + mtvecBaseAlign), interruptOffset)
    val doVector = base(0) && cause(cause.getWidth-1) && (cause_lsbs >> mtvecInterruptAlign) === 0
    Mux(doVector, interruptVec, base >> mtvecBaseAlign << mtvecBaseAlign)
  }
  val tvec = Mux(trapToDebug, debugTVec, notDebugTVec)
  io.evec := tvec
  io.ptbr := reg_satp
  io.eret := insn_call || insn_break || insn_ret
  io.singleStep := reg_dcsr.step && !reg_debug
  io.status := reg_mstatus
  io.status.sd := io.status.fs.andR || io.status.xs.andR
  io.status.debug := reg_debug
  io.status.isa := reg_misa
  io.status.uxl := (if (usingUser) log2Ceil(xLen) - 4 else 0)
  io.status.sxl := (if (usingVM) log2Ceil(xLen) - 4 else 0)
  io.status.dprv := Reg(next = Mux(reg_mstatus.mprv && !reg_debug, reg_mstatus.mpp, reg_mstatus.prv))
  if (xLen == 32)
    io.status.sd_rv32 := io.status.sd

  val exception = insn_call || insn_break || io.exception
  assert(PopCount(insn_ret :: insn_call :: insn_break :: io.exception :: Nil) <= 1, "these conditions must be mutually exclusive")

  when (insn_wfi && !io.singleStep && !reg_debug) { reg_wfi := true }
  when (pending_interrupts.orR || io.interrupts.debug || exception) { reg_wfi := false }

  when (io.retire(0) || exception) { reg_singleStepped := true }
  when (!io.singleStep) { reg_singleStepped := false }
  assert(!io.singleStep || io.retire <= UInt(1))
  assert(!reg_singleStepped || io.retire === UInt(0))

  val epc = formEPC(io.pc)
  val noCause :: mCause :: hCause :: sCause :: uCause :: Nil = Enum(5)
  val xcause_dest = Wire(init = noCause)

  when (exception) {
    when (trapToDebug) {
      when (!reg_debug) {
        reg_debug := true
        reg_dpc := epc
        reg_dcsr.cause := Mux(reg_singleStepped, 4, Mux(causeIsDebugInt, 3, Mux[UInt](causeIsDebugTrigger, 2, 1)))
        reg_dcsr.prv := trimPrivilege(reg_mstatus.prv)
        new_prv := PRV.M
      }
    }.elsewhen (delegate) {
      reg_sepc := epc
      reg_scause := cause
      xcause_dest := sCause
      reg_stval := io.tval
      reg_mstatus.spie := reg_mstatus.sie
      reg_mstatus.spp := reg_mstatus.prv
      reg_mstatus.sie := false
      new_prv := PRV.S
    }.otherwise {
      reg_mepc := epc
      reg_mcause := cause
      xcause_dest := mCause
      reg_mtval := io.tval
      reg_mstatus.mpie := reg_mstatus.mie
      reg_mstatus.mpp := trimPrivilege(reg_mstatus.prv)
      reg_mstatus.mie := false
      new_prv := PRV.M
    }
  }

  for (i <- 0 until supported_interrupts.getWidth) {
    val en = exception && (supported_interrupts & (BigInt(1) << i).U) =/= 0 && cause === (BigInt(1) << (xLen - 1)).U + i
    val delegable = (delegable_interrupts & (BigInt(1) << i).U) =/= 0
    cover(en && !delegate, s"INTERRUPT_M_$i")
    cover(en && delegable && delegate, s"INTERRUPT_S_$i")
  }
  for (i <- 0 until xLen) {
    val supported_exceptions: BigInt = 0x8fe |
      (if (usingCompressed && !coreParams.misaWritable) 0 else 1) |
      (if (usingUser) 0x100 else 0) |
      (if (usingVM) 0xb200 else 0)
    if (((supported_exceptions >> i) & 1) != 0) {
      val en = exception && cause === i
      val delegable = (delegable_exceptions & (BigInt(1) << i).U) =/= 0
      cover(en && !delegate, s"EXCEPTION_M_$i")
      cover(en && delegable && delegate, s"EXCEPTION_S_$i")
    }
  }

  when (insn_ret) {
    when (Bool(usingVM) && !io.rw.addr(9)) {
      reg_mstatus.sie := reg_mstatus.spie
      reg_mstatus.spie := true
      reg_mstatus.spp := PRV.U
      new_prv := reg_mstatus.spp
      io.evec := readEPC(reg_sepc)
    }.elsewhen (Bool(usingDebug) && io.rw.addr(10)) {
      new_prv := reg_dcsr.prv
      reg_debug := false
      io.evec := readEPC(reg_dpc)
    }.otherwise {
      reg_mstatus.mie := reg_mstatus.mpie
      reg_mstatus.mpie := true
      reg_mstatus.mpp := legalizePrivilege(PRV.U)
      new_prv := reg_mstatus.mpp
      io.evec := readEPC(reg_mepc)
    }
  }

  io.time := reg_cycle
  io.csr_stall := reg_wfi || io.status.cease
  io.status.cease := RegEnable(true.B, false.B, insn_cease)

  for ((io, reg) <- io.customCSRs zip reg_custom) {
    io.wen := false
    io.wdata := wdata
    io.value := reg
  }

  io.rw.rdata := Mux1H(for ((k, v) <- read_mapping) yield decoded_addr(k) -> v)

  // cover access to register
  read_mapping.foreach( {case (k, v) => {
    when (!k(11,10).andR) {  // Cover points for RW CSR registers
      cover(io.rw.cmd.isOneOf(CSR.W, CSR.S, CSR.C) && io.rw.addr===k, "CSR_access_"+k.toString, "Cover Accessing Core CSR field")
    } .otherwise { // Cover points for RO CSR registers
      cover(io.rw.cmd===CSR.R && io.rw.addr===k, "CSR_access_"+k.toString, "Cover Accessing Core CSR field")
    }
  }})

  val set_fs_dirty = Wire(init = io.set_fs_dirty.getOrElse(false.B))
  if (coreParams.haveFSDirty) {
    when (set_fs_dirty) {
      assert(reg_mstatus.fs > 0)
      reg_mstatus.fs := 3
    }
  }

  io.fcsr_rm := reg_frm
  when (io.fcsr_flags.valid) {
    reg_fflags := reg_fflags | io.fcsr_flags.bits
    set_fs_dirty := true
  }

  val csr_wen = io.rw.cmd.isOneOf(CSR.S, CSR.C, CSR.W)
  io.csrw_counter := Mux(coreParams.haveBasicCounters && csr_wen && (io.rw.addr.inRange(CSRs.mcycle, CSRs.mcycle + CSR.nCtr) || io.rw.addr.inRange(CSRs.mcycleh, CSRs.mcycleh + CSR.nCtr)), UIntToOH(io.rw.addr(log2Ceil(CSR.nCtr+nPerfCounters)-1, 0)), 0.U)
  when (csr_wen) {
    when (decoded_addr(CSRs.mstatus)) {
      val new_mstatus = new MStatus().fromBits(wdata)
      reg_mstatus.mie := new_mstatus.mie
      reg_mstatus.mpie := new_mstatus.mpie

      if (usingUser) {
        reg_mstatus.mprv := new_mstatus.mprv
        reg_mstatus.mpp := legalizePrivilege(new_mstatus.mpp)
        if (usingVM) {
          reg_mstatus.mxr := new_mstatus.mxr
          reg_mstatus.sum := new_mstatus.sum
          reg_mstatus.spp := new_mstatus.spp
          reg_mstatus.spie := new_mstatus.spie
          reg_mstatus.sie := new_mstatus.sie
          reg_mstatus.tw := new_mstatus.tw
          reg_mstatus.tvm := new_mstatus.tvm
          reg_mstatus.tsr := new_mstatus.tsr
        }
      }

      if (usingVM || usingFPU) reg_mstatus.fs := formFS(new_mstatus.fs)
      if (usingRoCC) reg_mstatus.xs := Fill(2, new_mstatus.xs.orR)
    }
    when (decoded_addr(CSRs.misa)) {
      val mask = UInt(isaStringToMask(isaMaskString), xLen)
      val f = wdata('f' - 'a')
      // suppress write if it would cause the next fetch to be misaligned
      when (!usingCompressed || !io.pc(1) || wdata('c' - 'a')) {
        if (coreParams.misaWritable)
          reg_misa := ~(~wdata | (!f << ('d' - 'a'))) & mask | reg_misa & ~mask
      }
    }
    when (decoded_addr(CSRs.mip)) {
      // MIP should be modified based on the value in reg_mip, not the value
      // in read_mip, since read_mip.seip is the OR of reg_mip.seip and
      // io.interrupts.seip.  We don't want the value on the PLIC line to
      // inadvertently be OR'd into read_mip.seip.
      val new_mip = readModifyWriteCSR(io.rw.cmd, reg_mip.asUInt, io.rw.wdata).asTypeOf(new MIP)
      if (usingVM) {
        reg_mip.ssip := new_mip.ssip
        reg_mip.stip := new_mip.stip
        reg_mip.seip := new_mip.seip
      }
    }
    when (decoded_addr(CSRs.mie))      { reg_mie := wdata & supported_interrupts }
    when (decoded_addr(CSRs.mepc))     { reg_mepc := formEPC(wdata) }
    when (decoded_addr(CSRs.mscratch)) { reg_mscratch := wdata }
    if (mtvecWritable)
      when (decoded_addr(CSRs.mtvec))  { reg_mtvec := wdata }
    when (decoded_addr(CSRs.mcause))   { reg_mcause := wdata & UInt((BigInt(1) << (xLen-1)) + (BigInt(1) << whichInterrupt.getWidth) - 1) }
    when (decoded_addr(CSRs.mtval))    { reg_mtval := wdata(vaddrBitsExtended-1,0) }

    for (((e, c), i) <- (reg_hpmevent zip reg_hpmcounter) zipWithIndex) {
      writeCounter(i + CSR.firstMHPC, c, wdata)
      when (decoded_addr(i + CSR.firstHPE)) { e := perfEventSets.maskEventSelector(wdata) }
    }
    if (coreParams.haveBasicCounters) {
      writeCounter(CSRs.mcycle, reg_cycle, wdata)
      writeCounter(CSRs.minstret, reg_instret, wdata)
    }

    if (usingFPU) {
      when (decoded_addr(CSRs.fflags)) { set_fs_dirty := true; reg_fflags := wdata }
      when (decoded_addr(CSRs.frm))    { set_fs_dirty := true; reg_frm := wdata }
      when (decoded_addr(CSRs.fcsr))   { set_fs_dirty := true; reg_fflags := wdata; reg_frm := wdata >> reg_fflags.getWidth }
    }
    if (usingDebug) {
      when (decoded_addr(CSRs.dcsr)) {
        val new_dcsr = new DCSR().fromBits(wdata)
        reg_dcsr.step := new_dcsr.step
        reg_dcsr.ebreakm := new_dcsr.ebreakm
        if (usingVM) reg_dcsr.ebreaks := new_dcsr.ebreaks
        if (usingUser) reg_dcsr.ebreaku := new_dcsr.ebreaku
        if (usingUser) reg_dcsr.prv := legalizePrivilege(new_dcsr.prv)
      }
      when (decoded_addr(CSRs.dpc))      { reg_dpc := formEPC(wdata) }
      when (decoded_addr(CSRs.dscratch)) { reg_dscratch := wdata }
    }
    if (usingVM) {
      when (decoded_addr(CSRs.sstatus)) {
        val new_sstatus = new MStatus().fromBits(wdata)
        reg_mstatus.sie := new_sstatus.sie
        reg_mstatus.spie := new_sstatus.spie
        reg_mstatus.spp := new_sstatus.spp
        reg_mstatus.mxr := new_sstatus.mxr
        reg_mstatus.sum := new_sstatus.sum
        reg_mstatus.fs := formFS(new_sstatus.fs)
        if (usingRoCC) reg_mstatus.xs := Fill(2, new_sstatus.xs.orR)
      }
      when (decoded_addr(CSRs.sip)) {
        val new_sip = new MIP().fromBits((read_mip & ~read_mideleg) | (wdata & read_mideleg))
        reg_mip.ssip := new_sip.ssip
      }
      when (decoded_addr(CSRs.satp)) {
        val new_satp = new PTBR().fromBits(wdata)
        val valid_modes = 0 +: (minPgLevels to pgLevels).map(new_satp.pgLevelsToMode(_))
        when (new_satp.mode.isOneOf(valid_modes.map(_.U))) {
          reg_satp.mode := new_satp.mode & valid_modes.reduce(_|_)
          reg_satp.ppn := new_satp.ppn(ppnBits-1,0)
          if (asIdBits > 0) reg_satp.asid := new_satp.asid(asIdBits-1,0)
        }
      }
      when (decoded_addr(CSRs.sie))      { reg_mie := (reg_mie & ~read_mideleg) | (wdata & read_mideleg) }
      when (decoded_addr(CSRs.sscratch)) { reg_sscratch := wdata }
      when (decoded_addr(CSRs.sepc))     { reg_sepc := formEPC(wdata) }
      when (decoded_addr(CSRs.stvec))    { reg_stvec := wdata }
      when (decoded_addr(CSRs.scause))   { reg_scause := wdata & UInt((BigInt(1) << (xLen-1)) + 31) /* only implement 5 LSBs and MSB */ }
      when (decoded_addr(CSRs.stval))    { reg_stval := wdata(vaddrBitsExtended-1,0) }
      when (decoded_addr(CSRs.mideleg))  { reg_mideleg := wdata }
      when (decoded_addr(CSRs.medeleg))  { reg_medeleg := wdata }
      when (decoded_addr(CSRs.scounteren)) { reg_scounteren := wdata }
    }
    if (usingUser) {
      when (decoded_addr(CSRs.mcounteren)) { reg_mcounteren := wdata }
    }
    if (nBreakpoints > 0) {
      when (decoded_addr(CSRs.tselect)) { reg_tselect := wdata }

      for ((bp, i) <- reg_bp.zipWithIndex) {
        when (i === reg_tselect && (!bp.control.dmode || reg_debug)) {
          when (decoded_addr(CSRs.tdata2)) { bp.address := wdata }
          when (decoded_addr(CSRs.tdata1)) {
            bp.control := wdata.asTypeOf(bp.control)

            val prevChain = if (i == 0) false.B else reg_bp(i-1).control.chain
            val prevDMode = if (i == 0) false.B else reg_bp(i-1).control.dmode
            val nextChain = if (i >= nBreakpoints-1) true.B else reg_bp(i+1).control.chain
            val nextDMode = if (i >= nBreakpoints-1) true.B else reg_bp(i+1).control.dmode
            val newBPC = readModifyWriteCSR(io.rw.cmd, bp.control.asUInt, io.rw.wdata).asTypeOf(bp.control)
            val dMode = newBPC.dmode && reg_debug && (prevDMode || !prevChain)
            bp.control.dmode := dMode
            when (dMode || (newBPC.action > 1.U)) { bp.control.action := newBPC.action }.otherwise { bp.control.action := 0.U }
            bp.control.chain := newBPC.chain && !(prevChain || nextChain) && (dMode || !nextDMode)
          }
        }
      }
    }
    if (reg_pmp.nonEmpty) for (((pmp, next), i) <- (reg_pmp zip (reg_pmp.tail :+ reg_pmp.last)) zipWithIndex) {
      require(xLen % pmp.cfg.getWidth == 0)
      when (decoded_addr(CSRs.pmpcfg0 + pmpCfgIndex(i)) && !pmp.cfgLocked) {
        val newCfg = new PMPConfig().fromBits(wdata >> ((i * pmp.cfg.getWidth) % xLen))
        pmp.cfg := newCfg
        // disallow unreadable but writable PMPs
        pmp.cfg.w := newCfg.w && newCfg.r
        // can't select a=NA4 with coarse-grained PMPs
        if (pmpGranularity.log2 > PMP.lgAlign)
          pmp.cfg.a := Cat(newCfg.a(1), newCfg.a.orR)
      }
      when (decoded_addr(CSRs.pmpaddr0 + i) && !pmp.addrLocked(next)) {
        pmp.addr := wdata
      }
    }
    for ((io, csr, reg) <- (io.customCSRs, customCSRs, reg_custom).zipped) {
      val mask = csr.mask.U(xLen.W)
      when (decoded_addr(csr.id)) {
        reg := (wdata & mask) | (reg & ~mask)
        io.wen := true
      }
    }
  }

  reg_satp.asid := 0
  if (nBreakpoints <= 1) reg_tselect := 0
  for (bpc <- reg_bp map {_.control}) {
    bpc.ttype := bpc.tType
    bpc.maskmax := bpc.maskMax
    bpc.reserved := 0
    bpc.zero := 0
    bpc.h := false
    if (!usingVM) bpc.s := false
    if (!usingUser) bpc.u := false
    if (!usingVM && !usingUser) bpc.m := true
    when (reset) {
      bpc.action := 0.U
      bpc.dmode := false
      bpc.chain := false
      bpc.r := false
      bpc.w := false
      bpc.x := false
    }
  }
  for (bp <- reg_bp drop nBreakpoints)
    bp := new BP().fromBits(0)
  for (pmp <- reg_pmp) {
    pmp.cfg.res := 0
    when (reset) { pmp.reset() }
  }

  for (((t, insn), i) <- (io.trace zip io.inst).zipWithIndex) {
    t.clock := clock
    t.reset := reset
    t.exception := io.retire >= i && exception
    t.valid := io.retire > i || t.exception
    t.insn := insn
    t.iaddr := io.pc
    t.priv := Cat(reg_debug, reg_mstatus.prv)
    t.cause := cause
    t.interrupt := cause(xLen-1)
    t.tval := io.tval
  }

  def chooseInterrupt(masksIn: Seq[UInt]): (Bool, UInt) = {
    val nonstandard = supported_interrupts.getWidth-1 to 12 by -1
    // MEI, MSI, MTI, SEI, SSI, STI, UEI, USI, UTI
    val standard = Seq(11, 3, 7, 9, 1, 5, 8, 0, 4)
    val priority = nonstandard ++ standard
    val masks = masksIn.reverse
    val any = masks.flatMap(m => priority.filter(_ < m.getWidth).map(i => m(i))).reduce(_||_)
    val which = PriorityMux(masks.flatMap(m => priority.filter(_ < m.getWidth).map(i => (m(i), i.U))))
    (any, which)
  }

  def readModifyWriteCSR(cmd: UInt, rdata: UInt, wdata: UInt) = {
    (Mux(cmd(1), rdata, UInt(0)) | wdata) & ~Mux(cmd(1,0).andR, wdata, UInt(0))
  }

  def legalizePrivilege(priv: UInt): UInt =
    if (usingVM) Mux(priv === PRV.H, PRV.U, priv)
    else if (usingUser) Fill(2, priv(0))
    else PRV.M

  def trimPrivilege(priv: UInt): UInt =
    if (usingVM) priv
    else legalizePrivilege(priv)

  def writeCounter(lo: Int, ctr: WideCounter, wdata: UInt) = {
    if (xLen == 32) {
      val hi = lo + CSRs.mcycleh - CSRs.mcycle
      when (decoded_addr(lo)) { ctr := Cat(ctr(ctr.getWidth-1, 32), wdata) }
      when (decoded_addr(hi)) { ctr := Cat(wdata(ctr.getWidth-33, 0), ctr(31, 0)) }
    } else {
      when (decoded_addr(lo)) { ctr := wdata(ctr.getWidth-1, 0) }
    }
  }
  def formEPC(x: UInt) = ~(~x | (if (usingCompressed) 1.U else 3.U))
  def readEPC(x: UInt) = ~(~x | Mux(reg_misa('c' - 'a'), 1.U, 3.U))
  def formTVec(x: UInt) = x andNot Mux(x(0), ((((BigInt(1) << mtvecInterruptAlign) - 1) << mtvecBaseAlign) | 2).U, 2)
  def isaStringToMask(s: String) = s.map(x => 1 << (x - 'A')).foldLeft(0)(_|_)
  def formFS(fs: UInt) = if (coreParams.haveFSDirty) fs else Fill(2, fs.orR)
}
