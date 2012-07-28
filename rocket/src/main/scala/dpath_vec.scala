package rocket

import Chisel._
import Node._
import Constants._
import Instructions._
import hwacha.Constants._

class ioDpathVecInterface extends Bundle
{
  val vcmdq_bits = Bits(OUTPUT, SZ_VCMD)
  val vximm1q_bits = Bits(OUTPUT, SZ_VIMM)
  val vximm2q_bits = Bits(OUTPUT, SZ_VSTRIDE)
  val vcntq_bits = Bits(OUTPUT, SZ_VLEN)
  val vcntq_last = Bool(OUTPUT)
  val evac_addr = Bits(OUTPUT, 64)
  val irq_aux = Bits(INPUT, 64)
}

class ioDpathVec extends Bundle
{
  val ctrl = new ioCtrlDpathVec().flip
  val iface = new ioDpathVecInterface()
  val valid = Bool(INPUT)
  val inst = Bits(INPUT, 32)
  val waddr = UFix(INPUT, 5)
  val raddr1 = UFix(INPUT, 5)
  val vecbank = Bits(INPUT, 8)
  val vecbankcnt = UFix(INPUT, 4)
  val wdata = Bits(INPUT, 64)
  val rs2 = Bits(INPUT, 64)
  val wen = Bool(OUTPUT)
  val irq_aux = Bits(OUTPUT, 64)
  val appvl = UFix(OUTPUT, 12)
  val nxregs = UFix(OUTPUT, 6)
  val nfregs = UFix(OUTPUT, 6)
}

class rocketDpathVec extends Component
{
  val io = new ioDpathVec()

  val nxregs_stage = Mux(io.ctrl.fn === VEC_CFG, io.wdata(5,0), io.inst(15,10))
  val nfregs_stage = Mux(io.ctrl.fn === VEC_CFG, io.rs2(5,0), io.inst(21,16))
  val nxregs = Mux(nxregs_stage(5), Bits(32), Mux(nxregs_stage === Bits(0), Bits(1), nxregs_stage)) + UFix(0,7)
  val nfregs = Mux(nfregs_stage(5), Bits(32), nfregs_stage) + UFix(0,7)
  val nregs = nxregs + nfregs

  //val uts_per_bank = UFix(4,9)

  val nreg_mod_bank = MuxLookup(
    nregs, UFix(4,9), Array(
      UFix(0,7) -> UFix(256,9),
      UFix(1,7) -> UFix(256,9),
      UFix(2,7) -> UFix(256,9),
      UFix(3,7) -> UFix(128,9),
      UFix(4,7) -> UFix(85,9),
      UFix(5,7) -> UFix(64,9),
      UFix(6,7) -> UFix(51,9),
      UFix(7,7) -> UFix(42,9),
      UFix(8,7) -> UFix(36,9),
      UFix(9,7) -> UFix(32,9),
      UFix(10,7) -> UFix(28,9),
      UFix(11,7) -> UFix(25,9),
      UFix(12,7) -> UFix(23,9),
      UFix(13,7) -> UFix(21,9),
      UFix(14,7) -> UFix(19,9),
      UFix(15,7) -> UFix(18,9),
      UFix(16,7) -> UFix(17,9),
      UFix(17,7) -> UFix(16,9),
      UFix(18,7) -> UFix(15,9),
      UFix(19,7) -> UFix(14,9),
      UFix(20,7) -> UFix(13,9),
      UFix(21,7) -> UFix(12,9),
      UFix(22,7) -> UFix(12,9),
      UFix(23,7) -> UFix(11,9),
      UFix(24,7) -> UFix(11,9),
      UFix(25,7) -> UFix(10,9),
      UFix(26,7) -> UFix(10,9),
      UFix(27,7) -> UFix(9,9),
      UFix(28,7) -> UFix(9,9),
      UFix(29,7) -> UFix(9,9),
      UFix(30,7) -> UFix(8,9),
      UFix(31,7) -> UFix(8,9),
      UFix(32,7) -> UFix(8,9),
      UFix(33,7) -> UFix(8,9),
      UFix(34,7) -> UFix(7,9),
      UFix(35,7) -> UFix(7,9),
      UFix(36,7) -> UFix(7,9),
      UFix(37,7) -> UFix(7,9),
      UFix(38,7) -> UFix(6,9),
      UFix(39,7) -> UFix(6,9),
      UFix(40,7) -> UFix(6,9),
      UFix(41,7) -> UFix(6,9),
      UFix(42,7) -> UFix(6,9),
      UFix(43,7) -> UFix(6,9),
      UFix(44,7) -> UFix(5,9),
      UFix(45,7) -> UFix(5,9),
      UFix(46,7) -> UFix(5,9),
      UFix(47,7) -> UFix(5,9),
      UFix(48,7) -> UFix(5,9),
      UFix(49,7) -> UFix(5,9),
      UFix(50,7) -> UFix(5,9),
      UFix(51,7) -> UFix(5,9),
      UFix(52,7) -> UFix(5,9)
    ))

  val uts_per_bank = Mux(Bool(hwacha.Constants.HAVE_PVFB) & nreg_mod_bank > UFix(MAX_THREADS,9), UFix(MAX_THREADS, 9), nreg_mod_bank)

  val reg_hwvl = Reg(resetVal = UFix(32, 12))
  val reg_appvl0 = Reg(resetVal = Bool(true))
  val hwvl_vcfg = (uts_per_bank * io.vecbankcnt)(11,0)

  val hwvl =
    Mux(io.ctrl.fn === VEC_CFG || io.ctrl.fn === VEC_CFGVL, hwvl_vcfg,
        reg_hwvl)

  val appvl =
    Mux(io.ctrl.fn === VEC_CFG, UFix(0),
    Mux(io.wdata(11,0) < hwvl, io.wdata(11,0).toUFix,
        hwvl.toUFix))

  val reg_nxregs = Reg(resetVal = UFix(32, 6))
  val reg_nfregs = Reg(resetVal = UFix(32, 6))
  val reg_appvl = Reg(resetVal = UFix(0, 12))

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

  val appvlm1 = appvl - UFix(1)

  io.iface.vcmdq_bits :=
    Mux(io.ctrl.sel_vcmd === VCMD_I, Cat(Bits(0,2), Bits(0,4), io.inst(9,8), Bits(0,6), Bits(0,6)),
    Mux(io.ctrl.sel_vcmd === VCMD_F, Cat(Bits(0,2), Bits(1,3), io.inst(9,7), Bits(0,6), Bits(0,6)),
    Mux(io.ctrl.sel_vcmd === VCMD_TX, Cat(Bits(1,2), io.inst(13,8), Bits(0,1), io.waddr, Bits(0,1), io.raddr1),
    Mux(io.ctrl.sel_vcmd === VCMD_TF, Cat(Bits(1,2), io.inst(13,8), Bits(1,1), io.waddr, Bits(1,1), io.raddr1),
    Mux(io.ctrl.sel_vcmd === VCMD_MX, Cat(Bits(1,1), io.inst(13,12), io.inst(2), io.inst(10,7), Bits(0,1), io.waddr, Bits(0,1), io.waddr),
    Mux(io.ctrl.sel_vcmd === VCMD_MF, Cat(Bits(1,1), io.inst(13,12), io.inst(2), io.inst(10,7), Bits(1,1), io.waddr, Bits(1,1), io.waddr),
    Mux(io.ctrl.sel_vcmd === VCMD_A, io.wdata(SZ_VCMD-1, 0),
        Bits(0,20))))))))

  io.iface.vximm1q_bits :=
    Mux(io.ctrl.sel_vimm === VIMM_VLEN, Cat(Bits(0,29), io.vecbankcnt, io.vecbank, nfregs(5,0), nxregs(5,0), appvlm1(10,0)),
        io.wdata) // VIMM_ALU

  io.iface.vximm2q_bits :=
    Mux(io.ctrl.sel_vimm2 === VIMM2_RS2, io.rs2,
        io.wdata) // VIMM2_ALU

  io.iface.vcntq_bits := io.wdata(SZ_VLEN-1, 0)
  io.iface.vcntq_last := io.rs2(1)

  io.iface.evac_addr := io.wdata

  io.ctrl.inst := io.inst
  io.ctrl.appvl0 := reg_appvl0
  io.ctrl.pfq := io.rs2(0)
}
