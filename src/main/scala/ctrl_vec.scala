package Top

import Chisel._
import Node._
import Constants._
import Instructions._

class ioCtrlDpathVec extends Bundle
{
  val valid = Bool(INPUT)
  val inst = Bits(32, INPUT)
  val appvl0 = Bool(INPUT)
  val wen = Bool(OUTPUT)
  val fn = Bits(1, OUTPUT)
  val sel_vcmd = Bits(3, OUTPUT)
  val sel_vimm = Bits(1, OUTPUT)
}

class ioCtrlVecInterface extends Bundle
{
  val vcmdq_valid = Bool(OUTPUT)
  val vcmdq_ready = Bool(INPUT)
  val vximm1q_valid = Bool(OUTPUT)
  val vximm1q_ready = Bool(INPUT)
  val vximm2q_valid = Bool(OUTPUT)
  val vximm2q_ready = Bool(INPUT)

  val vpfcmdq_valid = Bool(OUTPUT)
  val vpfcmdq_ready = Bool(INPUT)
  val vpfximm1q_valid = Bool(OUTPUT)
  val vpfximm1q_ready = Bool(INPUT)
  val vpfximm2q_valid = Bool(OUTPUT)
  val vpfximm2q_ready = Bool(INPUT)

  val vackq_valid = Bool(INPUT)
  val vackq_ready = Bool(OUTPUT)
}

class ioCtrlVec extends Bundle
{
  val dpath = new ioCtrlDpathVec()
  val iface = new ioCtrlVecInterface()
  val sr_ev = Bool(INPUT)
  val replay = Bool(OUTPUT)
  val cpfence = Bool(OUTPUT)
}

class rocketCtrlVec extends Component
{
  val io = new ioCtrlVec()

  val veccs =
  ListLookup(io.dpath.inst,
                //                                 appvlmask
                //                                 | vcmdq
                //                                 | | vximm1q
                //                                 | | | vximm2q
                //                                 | | | | vpfcmdq
                //                       wen       | | | | | vpximm1q
                // val vcmd    vimm      | fn      | | | | | | vpximm2q
                //   | |       |         | |       | | | | | | | cpfence
                //   | |       |         | |       | | | | | | | |
                List(N,VCMD_X, VIMM_X,   N,VEC_X  ,N,N,N,N,N,N,N,N),Array(
    VVCFGIVL->  List(Y,VCMD_I, VIMM_VLEN,Y,VEC_CFG,N,Y,Y,N,N,N,N,N),
    VSETVL->    List(Y,VCMD_I, VIMM_VLEN,Y,VEC_VL ,N,Y,Y,N,N,N,N,N),
    VF->        List(Y,VCMD_I, VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,N,N,N,N),
    VMVV->      List(Y,VCMD_TX,VIMM_X,   N,VEC_X  ,Y,Y,N,N,N,N,N,N),
    VMSV->      List(Y,VCMD_TX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,N,N,N,N),
    VFMVV->     List(Y,VCMD_TF,VIMM_X,   N,VEC_X  ,Y,Y,N,N,N,N,N,N),
    FENCE_L_V-> List(Y,VCMD_F, VIMM_X,   N,VEC_X  ,N,Y,N,N,N,N,N,N),
    FENCE_G_V-> List(Y,VCMD_F, VIMM_X,   N,VEC_X  ,N,Y,N,N,N,N,N,N),
    FENCE_L_CV->List(Y,VCMD_F, VIMM_X,   N,VEC_X  ,N,Y,N,N,N,N,N,Y),
    FENCE_G_CV->List(Y,VCMD_F, VIMM_X,   N,VEC_X  ,N,Y,N,N,N,N,N,Y),
    VLD->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VLW->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VLWU->      List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VLH->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VLHU->      List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VLB->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VLBU->      List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VSD->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VSW->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VSH->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VSB->       List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VFLD->      List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VFLW->      List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VFSD->      List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VFSW->      List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,N,Y,Y,N,N),
    VLSTD->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VLSTW->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VLSTWU->    List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VLSTH->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VLSTHU->    List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VLSTB->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VLSTBU->    List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VSSTD->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VSSTW->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VSSTH->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VSSTB->     List(Y,VCMD_MX,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VFLSTD->    List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VFLSTW->    List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VFSSTD->    List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N),
    VFSSTW->    List(Y,VCMD_MF,VIMM_ALU, N,VEC_X  ,Y,Y,Y,Y,Y,Y,Y,N)
  ))

  val wb_vec_val :: wb_sel_vcmd :: wb_sel_vimm :: wb_vec_wen :: wb_vec_fn :: wb_vec_appvlmask :: veccs0 = veccs
  val wb_vec_cmdq_enq :: wb_vec_ximm1q_enq :: wb_vec_ximm2q_enq :: veecs1 = veecs0
  val wb_vec_pfcmdq_enq :: wb_vec_pfximm1q_enq :: wb_vec_pfximm2q_enq :: wb_vec_cpfence :: Nil = veccs1

  val valid_common = io.dpath.valid && io.sr_ev && wb_vec_val.toBool && !(wb_vec_appvlmask.toBool && io.dpath.appvl0)

  val mask_wb_vec_cmdq_ready = !wb_vec_cmdq_enq || io.iface.vcmdq_ready
  val mask_wb_vec_ximm1q_ready = !wb_vec_ximm1q_enq || io.iface.vximm1q_ready
  val mask_wb_vec_ximm2q_ready = !wb_vec_ximm2q_enq || io.iface.vximm2q_ready
  val mask_wb_vec_pfcmdq_ready = !wb_vec_pfcmdq_enq || io.iface.vpfcmdq_ready
  val mask_wb_vec_pfximm1q_ready = !wb_vec_pfximm1q_enq || io.iface.vpfximm1q_ready
  val mask_wb_vec_pfximm2q_ready = !wb_vec_pfximm2q_enq || io.iface.vpfximm2q_ready

  io.dpath.wen := wb_vec_wen.toBool
  io.dpath.fn := wb_vec_fn
  io.dpath.sel_vcmd := wb_sel_vcmd
  io.dpath.sel_vimm := wb_sel_vimm

  io.iface.vcmdq_valid :=
    valid_common &&
    wb_vec_cmdq_enq && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready &&
    mask_wb_vec_pfcmdq_enq && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready

  io.iface.vximm1q_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && wb_vec_ximm1q_enq && mask_wb_vec_ximm2q_ready &&
    mask_wb_vec_pfcmdq_enq && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready

  io.iface.vximm2q_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && wb_vec_ximm2q_enq &&
    mask_wb_vec_pfcmdq_enq && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready

  io.iface.vpfcmdq_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready &&
    wb_vec_pfcmdq_enq && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready

  io.iface.vpfximm1q_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready &&
    mask_wb_vec_pfcmdq_enq && wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready

  io.iface.vpfximm2q_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready &&
    mask_wb_vec_pfcmdq_enq && mask_wb_vec_pfximm1q_ready && wb_vec_pfximm2q_ready

  io.replay := valid_common && (
    wb_vec_cmdq_enq && !io.iface.vcmdq_ready ||
    wb_vec_ximm1q_enq && !io.iface.vximm1q_ready ||
    wb_vec_ximm2q_enq && !io.iface.vximm2q_ready ||
    wb_vec_pfcmdq_enq && !io.iface.vpfcmdq_ready ||
    wb_vec_pfximm1q_enq && !io.iface.vpfximm1q_ready ||
    wb_vec_pfximm2q_enq && !io.iface.vpfximm2q_ready ||
  )
  io.cpfence := valid_common && wb_vec_cpfence && !io.replay
}
