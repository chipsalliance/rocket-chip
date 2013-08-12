package rocket

import Chisel._
import Node._
import Instructions._
import hwacha.Constants._

class DpathVecInterfaceIO extends Bundle
{
  val vcmdq = Decoupled(Bits(width = SZ_VCMD))
  val vximm1q = Decoupled(Bits(width = SZ_VIMM))
  val vximm2q = Decoupled(Bits(width = SZ_VSTRIDE))
  val vcntq = Decoupled(Bits(width = SZ_VLEN+1))

  val vpfcmdq = Decoupled(Bits(width = SZ_VCMD))
  val vpfximm1q = Decoupled(Bits(width = SZ_VIMM))
  val vpfximm2q = Decoupled(Bits(width = SZ_VSTRIDE))
  val vpfcntq = Decoupled(Bits(width = SZ_VLEN))

  val evac_addr = Bits(OUTPUT, 64)
  val irq_aux = Bits(INPUT, 64)
}

class DpathVecIO extends Bundle
{
  val ctrl = (new CtrlDpathVecIO).flip
  val iface = new DpathVecInterfaceIO
  val valid = Bool(INPUT)
  val inst = Bits(INPUT, 32)
  val vecbank = Bits(INPUT, 8)
  val vecbankcnt = UInt(INPUT, 4)
  val wdata = Bits(INPUT, 64)
  val rs2 = Bits(INPUT, 64)
  val wen = Bool(OUTPUT)
  val irq_aux = Bits(OUTPUT, 64)
  val appvl = UInt(OUTPUT, 12)
  val nxregs = UInt(OUTPUT, 6)
  val nfregs = UInt(OUTPUT, 6)
}

class rocketDpathVec extends Module
{
  val io = new DpathVecIO

  val nxregs_stage = Mux(io.ctrl.fn === VEC_CFG, io.wdata(5,0), io.inst(15,10))
  val nfregs_stage = Mux(io.ctrl.fn === VEC_CFG, io.rs2(5,0), io.inst(21,16))
  val nxregs = Mux(nxregs_stage(5), Bits(32), Mux(nxregs_stage === Bits(0), Bits(1), nxregs_stage)) + UInt(0,7)
  val nfregs = Mux(nfregs_stage(5), Bits(32), nfregs_stage) + UInt(0,7)
  val nregs = nxregs + nfregs

  //val uts_per_bank = UInt(4,9)

  val nreg_mod_bank = MuxLookup(
    nregs, UInt(4,9), Array(
      UInt(0,7) -> UInt(256,9),
      UInt(1,7) -> UInt(256,9),
      UInt(2,7) -> UInt(256,9),
      UInt(3,7) -> UInt(128,9),
      UInt(4,7) -> UInt(85,9),
      UInt(5,7) -> UInt(64,9),
      UInt(6,7) -> UInt(51,9),
      UInt(7,7) -> UInt(42,9),
      UInt(8,7) -> UInt(36,9),
      UInt(9,7) -> UInt(32,9),
      UInt(10,7) -> UInt(28,9),
      UInt(11,7) -> UInt(25,9),
      UInt(12,7) -> UInt(23,9),
      UInt(13,7) -> UInt(21,9),
      UInt(14,7) -> UInt(19,9),
      UInt(15,7) -> UInt(18,9),
      UInt(16,7) -> UInt(17,9),
      UInt(17,7) -> UInt(16,9),
      UInt(18,7) -> UInt(15,9),
      UInt(19,7) -> UInt(14,9),
      UInt(20,7) -> UInt(13,9),
      UInt(21,7) -> UInt(12,9),
      UInt(22,7) -> UInt(12,9),
      UInt(23,7) -> UInt(11,9),
      UInt(24,7) -> UInt(11,9),
      UInt(25,7) -> UInt(10,9),
      UInt(26,7) -> UInt(10,9),
      UInt(27,7) -> UInt(9,9),
      UInt(28,7) -> UInt(9,9),
      UInt(29,7) -> UInt(9,9),
      UInt(30,7) -> UInt(8,9),
      UInt(31,7) -> UInt(8,9),
      UInt(32,7) -> UInt(8,9),
      UInt(33,7) -> UInt(8,9),
      UInt(34,7) -> UInt(7,9),
      UInt(35,7) -> UInt(7,9),
      UInt(36,7) -> UInt(7,9),
      UInt(37,7) -> UInt(7,9),
      UInt(38,7) -> UInt(6,9),
      UInt(39,7) -> UInt(6,9),
      UInt(40,7) -> UInt(6,9),
      UInt(41,7) -> UInt(6,9),
      UInt(42,7) -> UInt(6,9),
      UInt(43,7) -> UInt(6,9),
      UInt(44,7) -> UInt(5,9),
      UInt(45,7) -> UInt(5,9),
      UInt(46,7) -> UInt(5,9),
      UInt(47,7) -> UInt(5,9),
      UInt(48,7) -> UInt(5,9),
      UInt(49,7) -> UInt(5,9),
      UInt(50,7) -> UInt(5,9),
      UInt(51,7) -> UInt(5,9),
      UInt(52,7) -> UInt(5,9)
    ))

  val max_threads = UInt(WIDTH_BMASK)
  val uts_per_bank = Mux(Bool(HAVE_PVFB) & nreg_mod_bank > max_threads, max_threads, nreg_mod_bank)

  val reg_hwvl = RegReset(UInt(32, 12))
  val reg_appvl0 = RegReset(Bool(true))
  val hwvl_vcfg = (uts_per_bank * io.vecbankcnt)(11,0)

  val hwvl =
    Mux(io.ctrl.fn === VEC_CFG || io.ctrl.fn === VEC_CFGVL, hwvl_vcfg,
        reg_hwvl)

  val appvl =
    Mux(io.ctrl.fn === VEC_CFG, UInt(0),
    Mux(io.wdata(11,0) < hwvl, io.wdata(11,0).toUInt,
        hwvl.toUInt))

  val reg_nxregs = RegReset(UInt(32, 6))
  val reg_nfregs = RegReset(UInt(32, 6))
  val reg_appvl = RegReset(UInt(0, 12))

  when (io.valid)
  {
    when (io.ctrl.fn === VEC_CFG || io.ctrl.fn === VEC_CFGVL)
    {
      reg_hwvl := hwvl_vcfg
      reg_nxregs := nxregs
      reg_nfregs := nfregs
    }
    when (io.ctrl.fn === VEC_VL || io.ctrl.fn === VEC_CFGVL)
    {
      reg_appvl0 := !(appvl.orR())
      reg_appvl := appvl
    }
  }

  io.wen := io.valid && io.ctrl.wen
  io.irq_aux := io.iface.irq_aux
  io.appvl := Mux(io.ctrl.fn === VEC_VL || io.ctrl.fn === VEC_CFGVL, appvl, reg_appvl)
  io.nxregs := reg_nxregs
  io.nfregs := reg_nfregs

  val appvlm1 = appvl - UInt(1)
  val waddr = io.inst(31,27)
  val raddr1 = io.inst(26,22)

  io.iface.vcmdq.bits :=
    Mux(io.ctrl.sel_vcmd === VCMD_I, Cat(Bits(0,2), Bits(0,4), io.inst(9,8), Bits(0,6), Bits(0,6)),
    Mux(io.ctrl.sel_vcmd === VCMD_F, Cat(Bits(0,2), Bits(1,3), io.inst(9,7), Bits(0,6), Bits(0,6)),
    Mux(io.ctrl.sel_vcmd === VCMD_TX, Cat(Bits(1,2), io.inst(13,8), Bits(0,1), waddr, Bits(0,1), raddr1),
    Mux(io.ctrl.sel_vcmd === VCMD_TF, Cat(Bits(1,2), io.inst(13,8), Bits(1,1), waddr, Bits(1,1), raddr1),
    Mux(io.ctrl.sel_vcmd === VCMD_MX, Cat(Bits(1,1), io.inst(13,12), io.inst(2), io.inst(10,7), Bits(0,1), waddr, Bits(0,1), waddr),
    Mux(io.ctrl.sel_vcmd === VCMD_MF, Cat(Bits(1,1), io.inst(13,12), io.inst(2), io.inst(10,7), Bits(1,1), waddr, Bits(1,1), waddr),
    Mux(io.ctrl.sel_vcmd === VCMD_A, io.wdata(SZ_VCMD-1, 0),
        Bits(0,20))))))))

  io.iface.vximm1q.bits :=
    Mux(io.ctrl.sel_vimm === VIMM_VLEN, Cat(Bits(0,29), io.vecbankcnt, io.vecbank, nfregs(5,0), nxregs(5,0), appvlm1(10,0)),
        io.wdata) // VIMM_ALU

  io.iface.vximm2q.bits :=
    Mux(io.ctrl.sel_vimm2 === VIMM2_RS2, io.rs2,
        io.wdata) // VIMM2_ALU

  val last = io.rs2(1)
  io.iface.vcntq.bits := Cat(last, io.iface.vpfcntq.bits)

  io.iface.vpfcmdq.bits := io.iface.vcmdq.bits
  io.iface.vpfximm1q.bits := io.iface.vximm1q.bits
  io.iface.vpfximm2q.bits := io.iface.vximm2q.bits
  io.iface.vpfcntq.bits := io.wdata(SZ_VLEN-1, 0)

  io.iface.evac_addr := io.wdata

  io.ctrl.inst := io.inst
  io.ctrl.appvl0 := reg_appvl0
  io.ctrl.pfq := io.rs2(0)
}
