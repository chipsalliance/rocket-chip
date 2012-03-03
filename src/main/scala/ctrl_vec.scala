package rocket

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
  val sel_vimm2 = Bits(1, OUTPUT)
}

class ioCtrlVecInterface extends Bundle
{
  val vcmdq_valid = Bool(OUTPUT)
  val vcmdq_ready = Bool(INPUT)
  val vximm1q_valid = Bool(OUTPUT)
  val vximm1q_ready = Bool(INPUT)
  val vximm2q_valid = Bool(OUTPUT)
  val vximm2q_ready = Bool(INPUT)
  val vcntq_valid = Bool(OUTPUT)
  val vcntq_ready = Bool(INPUT)

  val vpfcmdq_valid = Bool(OUTPUT)
  val vpfcmdq_ready = Bool(INPUT)
  val vpfximm1q_valid = Bool(OUTPUT)
  val vpfximm1q_ready = Bool(INPUT)
  val vpfximm2q_valid = Bool(OUTPUT)
  val vpfximm2q_ready = Bool(INPUT)
  val vpfcntq_valid = Bool(OUTPUT)
  val vpfcntq_ready = Bool(INPUT)

  val vackq_valid = Bool(INPUT)
  val vackq_ready = Bool(OUTPUT)

  val exception_ack_valid = Bool(INPUT)
  val exception_ack_ready = Bool(OUTPUT)

  val kill_ack_valid = Bool(INPUT)
  val kill_ack_ready = Bool(OUTPUT)
}

class ioCtrlVec extends Bundle
{
  val dpath = new ioCtrlDpathVec()
  val iface = new ioCtrlVecInterface()
  val sr_ev = Bool(INPUT)
  val exception = Bool(INPUT)
  val replay = Bool(OUTPUT)
  val stalld = Bool(OUTPUT)
}

class rocketCtrlVec extends Component
{
  val io = new ioCtrlVec()

  val veccs =
  ListLookup(io.dpath.inst,
                //                                           appvlmask
                //                                           | vcmdq
                //                                           | | vximm1q
                //                                           | | | vximm2q
                //                                           | | | | vcntq
                //                                           | | | | | vpfcmdq
                //                                           | | | | | | vpfximm1q
                //                                           | | | | | | | vpfximm2q
                //                                 wen       | | | | | | | | vpfcntq
                // val vcmd    vimm      vimm2     | fn      | | | | | | | | | stalld
                //   | |       |         |         | |       | | | | | | | | | | waitxcpt
                //   | |       |         |         | |       | | | | | | | | | | |
                List(N,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_X,  N,N,N,N,N,N,N,N,N,N,N),Array(
    VVCFGIVL->  List(Y,VCMD_I, VIMM_VLEN,VIMM2_X,  Y,VEC_CFG,N,Y,Y,N,N,Y,Y,N,N,N,N),
    VSETVL->    List(Y,VCMD_I, VIMM_VLEN,VIMM2_X,  Y,VEC_VL, N,Y,Y,N,N,Y,Y,N,N,N,N),
    VF->        List(Y,VCMD_I, VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,N,N,N,N,N,N),
    VMVV->      List(Y,VCMD_TX,VIMM_X,   VIMM2_X,  N,VEC_X,  Y,Y,N,N,N,N,N,N,N,N,N),
    VMSV->      List(Y,VCMD_TX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,N,N,N,N,N,N),
    VFMVV->     List(Y,VCMD_TF,VIMM_X,   VIMM2_X,  N,VEC_X,  Y,Y,N,N,N,N,N,N,N,N,N),
    FENCE_L_V-> List(Y,VCMD_F, VIMM_X,   VIMM2_X,  N,VEC_X,  N,Y,N,N,N,N,N,N,N,N,N),
    FENCE_G_V-> List(Y,VCMD_F, VIMM_X,   VIMM2_X,  N,VEC_X,  N,Y,N,N,N,N,N,N,N,N,N),
    FENCE_L_CV->List(Y,VCMD_F, VIMM_X,   VIMM2_X,  N,VEC_X,  N,Y,N,N,N,N,N,N,N,Y,N),
    FENCE_G_CV->List(Y,VCMD_F, VIMM_X,   VIMM2_X,  N,VEC_X,  N,Y,N,N,N,N,N,N,N,Y,N),
    VLD->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VLW->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VLWU->      List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VLH->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VLHU->      List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VLB->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VLBU->      List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VSD->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VSW->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VSH->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VSB->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VFLD->      List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VFLW->      List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VFSD->      List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VFSW->      List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,N,N,Y,Y,N,N,N,N),
    VLSTD->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VLSTW->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VLSTWU->    List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VLSTH->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VLSTHU->    List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VLSTB->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VLSTBU->    List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VSSTD->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VSSTW->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VSSTH->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VSSTB->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VFLSTD->    List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VFLSTW->    List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VFSSTD->    List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VFSSTW->    List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_X,  Y,Y,Y,Y,N,Y,Y,Y,N,N,N),
    VENQCMD->   List(Y,VCMD_A, VIMM_X,   VIMM2_X,  N,VEC_X,  N,Y,N,N,N,Y,N,N,N,N,N),
    VENQIMM1->  List(Y,VCMD_X, VIMM_ALU, VIMM2_X,  N,VEC_X,  N,N,Y,N,N,N,Y,N,N,N,N),
    VENQIMM2->  List(Y,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_X,  N,N,N,Y,N,N,N,Y,N,N,N),
    VENQCNT->   List(Y,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_X,  N,N,N,N,Y,N,N,N,Y,N,N),
    VWAITXCPT-> List(Y,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_X,  N,N,N,N,N,N,N,N,N,N,Y),
    VWAITKILL-> List(Y,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_X,  N,N,N,N,N,N,N,N,N,N,Y)
  ))

  val wb_vec_val :: wb_sel_vcmd :: wb_sel_vimm :: wb_sel_vimm2 :: wb_vec_wen :: wb_vec_fn :: wb_vec_appvlmask :: veccs0 = veccs
  val wb_vec_cmdq_enq :: wb_vec_ximm1q_enq :: wb_vec_ximm2q_enq :: wb_vec_cntq_enq :: veccs1 = veccs0
  val wb_vec_pfcmdq_enq :: wb_vec_pfximm1q_enq :: wb_vec_pfximm2q_enq :: wb_vec_pfcntq_enq :: veccs2 = veccs1
  val wb_vec_stalld :: wb_vec_waitxcpt :: Nil = veccs2

  val valid_common = io.dpath.valid && io.sr_ev && wb_vec_val && !(wb_vec_appvlmask && io.dpath.appvl0)

  val mask_wb_vec_cmdq_ready = !wb_vec_cmdq_enq || io.iface.vcmdq_ready
  val mask_wb_vec_ximm1q_ready = !wb_vec_ximm1q_enq || io.iface.vximm1q_ready
  val mask_wb_vec_ximm2q_ready = !wb_vec_ximm2q_enq || io.iface.vximm2q_ready
  val mask_wb_vec_cntq_ready = !wb_vec_cntq_enq || io.iface.vcntq_ready
  val mask_wb_vec_pfcmdq_ready = !wb_vec_pfcmdq_enq || io.iface.vpfcmdq_ready
  val mask_wb_vec_pfximm1q_ready = !wb_vec_pfximm1q_enq || io.iface.vpfximm1q_ready
  val mask_wb_vec_pfximm2q_ready = !wb_vec_pfximm2q_enq || io.iface.vpfximm2q_ready
  val mask_wb_vec_pfcntq_ready = !wb_vec_pfcntq_enq || io.iface.vpfcntq_ready

  io.dpath.wen := wb_vec_wen.toBool
  io.dpath.fn := wb_vec_fn
  io.dpath.sel_vcmd := wb_sel_vcmd
  io.dpath.sel_vimm := wb_sel_vimm
  io.dpath.sel_vimm2 := wb_sel_vimm2

  io.iface.vcmdq_valid :=
    valid_common &&
    wb_vec_cmdq_enq && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready && mask_wb_vec_cntq_ready &&
    mask_wb_vec_pfcmdq_ready && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready && mask_wb_vec_pfcntq_ready

  io.iface.vximm1q_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && wb_vec_ximm1q_enq && mask_wb_vec_ximm2q_ready && mask_wb_vec_cntq_ready &&
    mask_wb_vec_pfcmdq_ready && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready && mask_wb_vec_pfcntq_ready

  io.iface.vximm2q_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && wb_vec_ximm2q_enq && mask_wb_vec_cntq_ready &&
    mask_wb_vec_pfcmdq_ready && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready && mask_wb_vec_pfcntq_ready

  io.iface.vcntq_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready && wb_vec_cntq_enq &&
    mask_wb_vec_pfcmdq_ready && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready && mask_wb_vec_pfcntq_ready

  io.iface.vpfcmdq_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready && mask_wb_vec_cntq_ready &&
    wb_vec_pfcmdq_enq && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready && mask_wb_vec_pfcntq_ready

  io.iface.vpfximm1q_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready && mask_wb_vec_cntq_ready &&
    mask_wb_vec_pfcmdq_ready && wb_vec_pfximm1q_enq && mask_wb_vec_pfximm2q_ready && mask_wb_vec_pfcntq_ready

  io.iface.vpfximm2q_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready && mask_wb_vec_cntq_ready &&
    mask_wb_vec_pfcmdq_ready && mask_wb_vec_pfximm1q_ready && wb_vec_pfximm2q_enq && mask_wb_vec_pfcntq_ready

  io.iface.vpfcntq_valid :=
    valid_common &&
    mask_wb_vec_cmdq_ready && mask_wb_vec_ximm1q_ready && mask_wb_vec_ximm2q_ready && mask_wb_vec_cntq_ready &&
    mask_wb_vec_pfcmdq_ready && mask_wb_vec_pfximm1q_ready && mask_wb_vec_pfximm2q_ready && wb_vec_pfcntq_enq

  io.iface.vackq_ready := Bool(true)
  io.iface.exception_ack_ready := Bool(true)
  io.iface.kill_ack_ready := Bool(true)

  io.replay := valid_common && (
    wb_vec_cmdq_enq && !io.iface.vcmdq_ready ||
    wb_vec_ximm1q_enq && !io.iface.vximm1q_ready ||
    wb_vec_ximm2q_enq && !io.iface.vximm2q_ready ||
    wb_vec_cntq_enq && !io.iface.vcntq_ready ||
    wb_vec_pfcmdq_enq && !io.iface.vpfcmdq_ready ||
    wb_vec_pfximm1q_enq && !io.iface.vpfximm1q_ready ||
    wb_vec_pfximm2q_enq && !io.iface.vpfximm2q_ready ||
    wb_vec_pfcntq_enq && !io.iface.vpfcntq_ready
  )

  val reg_stalld = Reg(resetVal = Bool(false))
  val do_stalld = valid_common && wb_vec_stalld && !io.replay

  when (do_stalld) { reg_stalld := Bool(true) }
  when (io.iface.vackq_valid || io.exception) { reg_stalld := Bool(false) }

  val reg_waitxcpt = Reg(resetVal = Bool(false))
  val do_waitxcpt = valid_common && wb_vec_waitxcpt && !io.replay

  when (do_waitxcpt) { reg_waitxcpt := Bool(true) }
  when (io.iface.exception_ack_valid) { reg_waitxcpt := Bool(false) }
  when (io.iface.kill_ack_valid) { reg_waitxcpt := Bool(false) }

  io.stalld := reg_stalld || reg_waitxcpt
}
