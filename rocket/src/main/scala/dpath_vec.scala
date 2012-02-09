package Top

import Chisel._
import Node._
import Constants._
import Instructions._
import hwacha._

class ioDpathVec extends Bundle
{
  val valid = Bool(INPUT)
  val sr_ev = Bool(INPUT)
  val inst = Bits(32, INPUT)
  val waddr = UFix(5, INPUT)
  val raddr1 = UFix(5, INPUT)
  val vecbank = Bits(8, INPUT)
  val vecbankcnt = UFix(4, INPUT)
  val wdata = Bits(64, INPUT)
  val rs2 = Bits(64, INPUT)
  val wen = Bool(OUTPUT)
  val appvl = UFix(12, OUTPUT)
  val vcmdq = new io_vec_cmdq()
  val vximm1q = new io_vec_ximm1q()
  val vximm2q = new io_vec_ximm2q()
}

class rocketDpathVec extends Component
{
  val io = new ioDpathVec()

  val veccs =
  ListLookup(io.inst,
                //                                 appvlmask
                //                                 | vcmdq
                //                       wen       | | vximm1q
                // val vcmd    vimm      | fn      | | | vximm2q
                //   | |       |         | |       | | | |
                List(N,VCMD_X, VIMM_X,   N,VEC_X  ,N,N,N,N),Array(
    VVCFGIVL->  List(Y,VCMD_I, VIMM_VLEN,Y,VEC_CFG,N,Y,Y,N),
    VSETVL->    List(Y,VCMD_I, VIMM_VLEN,Y,VEC_VL ,N,Y,Y,N),
    VF->        List(Y,VCMD_I, VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VMVV->      List(Y,VCMD_TX,VIMM_X,   N,VEC_X  ,Y,Y,N,N),
    VMSV->      List(Y,VCMD_TX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VFMVV->     List(Y,VCMD_TF,VIMM_X,   N,VEC_X  ,Y,Y,N,N),
    FENCE_L_V-> List(Y,VCMD_F, VIMM_X,   N,VEC_X  ,N,Y,N,N),
    FENCE_G_V-> List(Y,VCMD_F, VIMM_X,   N,VEC_X  ,N,Y,N,N),
    FENCE_L_CV->List(Y,VCMD_F, VIMM_X,   N,VEC_X  ,N,Y,N,N),
    FENCE_G_CV->List(Y,VCMD_F, VIMM_X,   N,VEC_X  ,N,Y,N,N),
    VLD->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VLW->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VLWU->      List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VLH->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VLHU->      List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VLB->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VLBU->      List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VSD->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VSW->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VSH->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VSB->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VFLD->      List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VFLW->      List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VFSD->      List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VFSW->      List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N),
    VLSTD->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VLSTW->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VLSTWU->    List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VLSTH->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VLSTHU->    List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VLSTB->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VLSTBU->    List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VSSTD->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VSSTW->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VSSTH->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VSSTB->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VFLSTD->    List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VFLSTW->    List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VFSSTD->    List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y),
    VFSSTW->    List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y)
  ))

  val wb_vec_val :: wb_sel_vcmd :: wb_sel_vimm :: wb_vec_wen :: wb_vec_fn :: wb_vec_appvlmask :: veccs0 = veccs
  val wb_vec_cmdq_val :: wb_vec_ximm1q_val :: wb_vec_ximm2q_val :: Nil = veccs0

  val nxregs = Cat(UFix(0,1),io.inst(15,10).toUFix) // FIXME: to make the nregs width 7 bits
  val nfregs = io.inst(21,16).toUFix
  val nregs = nxregs + nfregs

  val uts_per_bank = MuxLookup(
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

  val reg_hwvl = Reg(resetVal = UFix(32, 12))
  val reg_appvl0 = Reg(resetVal = Bool(true))
  val hwvl_vcfg = (uts_per_bank * io.vecbankcnt)(11,0)
  val hwvl = Mux(wb_vec_fn.toBool, hwvl_vcfg, reg_hwvl)
  val appvl = Mux(io.wdata(11,0) < hwvl, io.wdata(11,0), hwvl).toUFix

  when (io.valid && wb_vec_wen.toBool && wb_vec_fn.toBool)
  {
    reg_hwvl <== hwvl_vcfg
    reg_appvl0 <== !(appvl.orR())
  }

  io.wen := io.valid && wb_vec_wen.toBool
  io.appvl := appvl
  val vlenm1 = appvl - Bits(1,1)

  val valid_common = io.valid && io.sr_ev && wb_vec_val.toBool && !(wb_vec_appvlmask.toBool && reg_appvl0)

  io.vcmdq.valid := valid_common && wb_vec_cmdq_val
  io.vximm1q.valid := valid_common && wb_vec_ximm1q_val
  io.vximm2q.valid := valid_common && wb_vec_ximm2q_val

  io.vcmdq.bits :=
    Mux(wb_sel_vcmd === VCMD_I, Cat(Bits(0,2), Bits(0,4), io.inst(9,8), Bits(0,6), Bits(0,6)),
    Mux(wb_sel_vcmd === VCMD_F, Cat(Bits(0,2), Bits(1,3), io.inst(9,7), Bits(0,6), Bits(0,6)),
    Mux(wb_sel_vcmd === VCMD_TX, Cat(Bits(1,2), io.inst(13,8), Bits(0,1), io.waddr, Bits(0,1), io.raddr1),
    Mux(wb_sel_vcmd === VCMD_TF, Cat(Bits(1,2), io.inst(13,8), Bits(1,1), io.waddr, Bits(1,1), io.raddr1),
    Mux(wb_sel_vcmd === VCMD_MX, Cat(Bits(1,1), io.inst(13,12), io.inst(2), io.inst(10,7), Bits(0,1), io.waddr, Bits(0,1), io.waddr),
    Mux(wb_sel_vcmd === VCMD_MF, Cat(Bits(1,1), io.inst(13,12), io.inst(2), io.inst(10,7), Bits(1,1), io.waddr, Bits(1,1), io.waddr),
    Bits(0,20)))))))

  io.vximm1q.bits :=
    Mux(wb_sel_vimm === VIMM_VLEN, Cat(Bits(0,29), io.vecbankcnt, io.vecbank, io.inst(21,10), vlenm1),
    io.wdata) // VIMM_ALU

  io.vximm2q.bits := io.rs2
}
