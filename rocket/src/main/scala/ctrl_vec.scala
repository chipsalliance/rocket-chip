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
}

class ioCtrlVec extends Bundle
{
  val dpath = new ioCtrlDpathVec()
  val iface = new ioCtrlVecInterface()
  val sr_ev = Bool(INPUT)
  val replay = Bool(OUTPUT)
}

class rocketCtrlVec extends Component
{
  val io = new ioCtrlVec()

  val veccs =
  ListLookup(io.dpath.inst,
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
  val wb_vec_cmdq_enq :: wb_vec_ximm1q_enq :: wb_vec_ximm2q_enq :: Nil = veccs0

  val valid_common = io.dpath.valid && io.sr_ev && wb_vec_val.toBool && !(wb_vec_appvlmask.toBool && io.dpath.appvl0)

  val mask_wb_vec_cmdq_ready = !wb_vec_cmdq_enq || io.iface.vcmdq_ready
  val mask_wb_vec_ximm1q_ready = !wb_vec_ximm1q_enq || io.iface.vximm1q_ready
  val mask_wb_vec_ximm2q_ready = !wb_vec_ximm2q_enq || io.iface.vximm2q_ready

  io.dpath.wen := wb_vec_wen.toBool
  io.dpath.fn := wb_vec_fn
  io.dpath.sel_vcmd := wb_sel_vcmd
  io.dpath.sel_vimm := wb_sel_vimm

  io.iface.vcmdq_valid := valid_common && wb_vec_cmdq_enq && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready
  io.iface.vximm1q_valid := valid_common && mask_wb_vec_cmdq_ready && wb_vec_ximm1q_enq && mask_wb_vec_ximm2q_ready
  io.iface.vximm2q_valid := valid_common && mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && wb_vec_ximm2q_enq

  io.replay := valid_common && (
    wb_vec_cmdq_enq && !io.iface.vcmdq_ready ||
    wb_vec_ximm1q_enq && !io.iface.vximm1q_ready ||
    wb_vec_ximm2q_enq && !io.iface.vximm2q_ready
  )
}
