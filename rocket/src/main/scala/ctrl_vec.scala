package rocket

import Chisel._
import Node._
import Constants._
import Instructions._
import hwacha.Constants._

class ioCtrlDpathVec extends Bundle
{
  val inst = Bits(INPUT, 32)
  val appvl0 = Bool(INPUT)
  val pfq = Bool(INPUT)
  val wen = Bool(OUTPUT)
  val fn = Bits(OUTPUT, 2)
  val sel_vcmd = Bits(OUTPUT, 3)
  val sel_vimm = Bits(OUTPUT, 1)
  val sel_vimm2 = Bits(OUTPUT, 1)
}

class ioCtrlVecInterface extends Bundle
{
  val vcmdq = new FIFOIO()(Bits(width = SZ_VCMD))
  val vximm1q = new FIFOIO()(Bits(width = SZ_VIMM))
  val vximm2q = new FIFOIO()(Bits(width = SZ_VSTRIDE))
  val vcntq = new FIFOIO()(Bits(width = SZ_VLEN+1))

  val vpfcmdq = new FIFOIO()(Bits(width = SZ_VCMD))
  val vpfximm1q = new FIFOIO()(Bits(width = SZ_VIMM))
  val vpfximm2q = new FIFOIO()(Bits(width = SZ_VSTRIDE))
  val vpfcntq = new FIFOIO()(Bits(width = SZ_VLEN))

  val vcmdq_user_ready = Bool(INPUT)
  val vximm1q_user_ready = Bool(INPUT)
  val vximm2q_user_ready = Bool(INPUT)
  val vfence_ready = Bool(INPUT)

  val irq = Bool(INPUT)
  val irq_cause = UFix(INPUT, 5)

  val exception = Bool(OUTPUT)

  val evac = Bool(OUTPUT)
  val kill = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
}

class ioCtrlVec extends Bundle
{
  val dpath = new ioCtrlDpathVec()
  val iface = new ioCtrlVecInterface()
  val valid = Bool(INPUT)
  val s = Bool(INPUT)
  val sr_ev = Bool(INPUT)
  val exception = Bool(INPUT)
  val eret = Bool(INPUT)
  val replay = Bool(OUTPUT)
  val vfence_ready = Bool(OUTPUT)
  val irq = Bool(OUTPUT)
  val irq_cause = UFix(OUTPUT, 5)
}

class rocketCtrlVecSigs extends Bundle
{
  val valid = Bool()
  val sel_vcmd = Bits(width = 3)
  val sel_vimm = Bits(width = 1)
  val sel_vimm2 = Bits(width = 1)
  val wen = Bool()
  val fn = Bits(width = 2)
  val appvlmask = Bool()
  val enq_cmdq = Bool()
  val enq_ximm1q = Bool()
  val enq_ximm2q = Bool()
  val enq_cntq = Bool()
  val enq_pfcmdq = Bool()
  val enq_pfximm1q = Bool()
  val enq_pfximm2q = Bool()
  val enq_pfcntq = Bool()
  val pfaq = Bool()
  val vfence = Bool()
  val xcptevac = Bool()
  val xcptkill = Bool()
  val xcpthold = Bool()
}

class rocketCtrlVecDecoder extends Component
{
  val io = new Bundle
  {
    val inst = Bits(INPUT, 32)
    val sigs = new rocketCtrlVecSigs().asOutput
  }

  val veccs =
  ListLookup(io.inst,
                //                                             appvlmask
                //                                             | vcmdq
                //                                             | | vximm1q
                //                                             | | | vximm2q
                //                                             | | | | vcntq
                //                                             | | | | | vpfcmdq
                //                                             | | | | | | vpfximm1q
                //                                             | | | | | | | vpfximm2q
                //                                             | | | | | | | | vpfcntq
                //                                             | | | | | | | | | pfq
                //                                             | | | | | | | | | | vfence 
                //                                             | | | | | | | | | | | xcptevac
                //                                             | | | | | | | | | | | | xcptkill
                //                                 wen         | | | | | | | | | | | | | xcpthold
                // val vcmd    vimm      vimm2     | fn        | | | | | | | | | | | | | |
                //   | |       |         |         | |         | | | | | | | | | | | | | |
                List(N,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_FN_N, N,N,N,N,N,N,N,N,N,N,N,N,N,N),Array(
    VVCFGIVL->  List(Y,VCMD_I, VIMM_VLEN,VIMM2_X,  Y,VEC_CFGVL,N,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,N),
    VVCFG->     List(Y,VCMD_I, VIMM_VLEN,VIMM2_X,  N,VEC_CFG,  N,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,N),
    VSETVL->    List(Y,VCMD_I, VIMM_VLEN,VIMM2_X,  Y,VEC_VL,   N,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VF->        List(Y,VCMD_I, VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,N,N,N,N,N,N,N,N,N),
    VMVV->      List(Y,VCMD_TX,VIMM_X,   VIMM2_X,  N,VEC_FN_N, Y,Y,N,N,N,N,N,N,N,N,N,N,N,N),
    VMSV->      List(Y,VCMD_TX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,N,N,N,N,N,N,N,N,N),
    VFMVV->     List(Y,VCMD_TF,VIMM_X,   VIMM2_X,  N,VEC_FN_N, Y,Y,N,N,N,N,N,N,N,N,N,N,N,N),
    FENCE_V_L-> List(Y,VCMD_F, VIMM_X,   VIMM2_X,  N,VEC_FN_N, N,N,N,N,N,N,N,N,N,N,Y,N,N,N),
    FENCE_V_G-> List(Y,VCMD_F, VIMM_X,   VIMM2_X,  N,VEC_FN_N, N,N,N,N,N,N,N,N,N,N,Y,N,N,N),
    VLD->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VLW->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VLWU->      List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VLH->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VLHU->      List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VLB->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VLBU->      List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VSD->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VSW->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VSH->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VSB->       List(Y,VCMD_MX,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VFLD->      List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VFLW->      List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VFSD->      List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VFSW->      List(Y,VCMD_MF,VIMM_ALU, VIMM2_X,  N,VEC_FN_N, Y,Y,Y,N,N,Y,Y,N,N,N,N,N,N,N),
    VLSTD->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VLSTW->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VLSTWU->    List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VLSTH->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VLSTHU->    List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VLSTB->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VLSTBU->    List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VSSTD->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VSSTW->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VSSTH->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VSSTB->     List(Y,VCMD_MX,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VFLSTD->    List(Y,VCMD_MF,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VFLSTW->    List(Y,VCMD_MF,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VFSSTD->    List(Y,VCMD_MF,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VFSSTW->    List(Y,VCMD_MF,VIMM_ALU, VIMM2_RS2,N,VEC_FN_N, Y,Y,Y,Y,N,Y,Y,Y,N,N,N,N,N,N),
    VENQCMD->   List(Y,VCMD_A, VIMM_X,   VIMM2_X,  N,VEC_FN_N, N,Y,N,N,N,Y,N,N,N,Y,N,N,N,N),
    VENQIMM1->  List(Y,VCMD_X, VIMM_ALU, VIMM2_X,  N,VEC_FN_N, N,N,Y,N,N,N,Y,N,N,Y,N,N,N,N),
    VENQIMM2->  List(Y,VCMD_X, VIMM_X,   VIMM2_ALU,N,VEC_FN_N, N,N,N,Y,N,N,N,Y,N,Y,N,N,N,N),
    VENQCNT->   List(Y,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_FN_N, N,N,N,N,Y,N,N,N,Y,Y,N,N,N,N),
    VXCPTEVAC-> List(Y,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_FN_N, N,N,N,N,N,N,N,N,N,N,N,Y,N,N),
    VXCPTKILL-> List(Y,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_FN_N, N,N,N,N,N,N,N,N,N,N,N,N,Y,N),
    VXCPTHOLD-> List(Y,VCMD_X, VIMM_X,   VIMM2_X,  N,VEC_FN_N, N,N,N,N,N,N,N,N,N,N,N,N,N,Y)
  ))

  val valid :: sel_vcmd :: sel_vimm :: sel_vimm2 :: wen :: fn :: appvlmask :: veccs0 = veccs
  val enq_cmdq :: enq_ximm1q :: enq_ximm2q :: enq_cntq :: veccs1 = veccs0
  val enq_pfcmdq :: enq_pfximm1q :: enq_pfximm2q :: enq_pfcntq :: veccs2 = veccs1
  val pfaq :: vfence :: xcptevac :: xcptkill :: xcpthold :: Nil = veccs2

  io.sigs.valid := valid.toBool
  io.sigs.sel_vcmd := sel_vcmd
  io.sigs.sel_vimm := sel_vimm
  io.sigs.sel_vimm2 := sel_vimm2
  io.sigs.wen := wen.toBool
  io.sigs.fn := fn
  io.sigs.appvlmask := appvlmask.toBool
  io.sigs.enq_cmdq := enq_cmdq.toBool
  io.sigs.enq_ximm1q := enq_ximm1q.toBool
  io.sigs.enq_ximm2q := enq_ximm2q.toBool
  io.sigs.enq_cntq := enq_cntq.toBool
  io.sigs.enq_pfcmdq := enq_pfcmdq.toBool
  io.sigs.enq_pfximm1q := enq_pfximm1q.toBool
  io.sigs.enq_pfximm2q := enq_pfximm2q.toBool
  io.sigs.enq_pfcntq := enq_pfcntq.toBool
  io.sigs.pfaq := pfaq.toBool
  io.sigs.vfence := vfence.toBool
  io.sigs.xcptevac := xcptevac.toBool
  io.sigs.xcptkill := xcptkill.toBool
  io.sigs.xcpthold := xcpthold.toBool
}

class rocketCtrlVec extends Component
{
  val io = new ioCtrlVec()

  val dec = new rocketCtrlVecDecoder()
  dec.io.inst := io.dpath.inst

  val valid_common = io.valid && io.sr_ev && dec.io.sigs.valid && !(dec.io.sigs.appvlmask && io.dpath.appvl0)

  val enq_pfcmdq_mask_pfq = dec.io.sigs.enq_pfcmdq && (!dec.io.sigs.pfaq || io.dpath.pfq)
  val enq_pfximm1q_mask_pfq = dec.io.sigs.enq_pfximm1q && (!dec.io.sigs.pfaq || io.dpath.pfq)
  val enq_pfximm2q_mask_pfq = dec.io.sigs.enq_pfximm2q && (!dec.io.sigs.pfaq || io.dpath.pfq)
  val enq_pfcntq_mask_pfq = dec.io.sigs.enq_pfcntq && (!dec.io.sigs.pfaq || io.dpath.pfq)

  val mask_cmdq_ready = !dec.io.sigs.enq_cmdq || io.s && io.iface.vcmdq.ready || !io.s && io.iface.vcmdq_user_ready
  val mask_ximm1q_ready = !dec.io.sigs.enq_ximm1q || io.s && io.iface.vximm1q.ready || !io.s && io.iface.vximm1q_user_ready
  val mask_ximm2q_ready = !dec.io.sigs.enq_ximm2q || io.s && io.iface.vximm2q.ready || !io.s && io.iface.vximm2q_user_ready
  val mask_cntq_ready = !dec.io.sigs.enq_cntq || io.iface.vcntq.ready
  val mask_pfcmdq_ready = !enq_pfcmdq_mask_pfq || io.iface.vpfcmdq.ready
  val mask_pfximm1q_ready = !enq_pfximm1q_mask_pfq || io.iface.vpfximm1q.ready
  val mask_pfximm2q_ready = !enq_pfximm2q_mask_pfq || io.iface.vpfximm2q.ready
  val mask_pfcntq_ready = !enq_pfcntq_mask_pfq || io.iface.vpfcntq.ready

  io.dpath.wen := dec.io.sigs.wen
  io.dpath.fn := dec.io.sigs.fn
  io.dpath.sel_vcmd := dec.io.sigs.sel_vcmd
  io.dpath.sel_vimm := dec.io.sigs.sel_vimm
  io.dpath.sel_vimm2 := dec.io.sigs.sel_vimm2

  io.iface.vcmdq.valid :=
    valid_common &&
    dec.io.sigs.enq_cmdq && mask_ximm1q_ready && mask_ximm2q_ready && mask_cntq_ready &&
    mask_pfcmdq_ready && mask_pfximm1q_ready && mask_pfximm2q_ready && mask_pfcntq_ready

  io.iface.vximm1q.valid :=
    valid_common &&
    mask_cmdq_ready && dec.io.sigs.enq_ximm1q && mask_ximm2q_ready && mask_cntq_ready &&
    mask_pfcmdq_ready && mask_pfximm1q_ready && mask_pfximm2q_ready && mask_pfcntq_ready

  io.iface.vximm2q.valid :=
    valid_common &&
    mask_cmdq_ready && mask_ximm1q_ready && dec.io.sigs.enq_ximm2q && mask_cntq_ready &&
    mask_pfcmdq_ready && mask_pfximm1q_ready && mask_pfximm2q_ready && mask_pfcntq_ready

  io.iface.vcntq.valid :=
    valid_common &&
    mask_cmdq_ready && mask_ximm1q_ready && mask_ximm2q_ready && dec.io.sigs.enq_cntq &&
    mask_pfcmdq_ready && mask_pfximm1q_ready && mask_pfximm2q_ready && mask_pfcntq_ready

  io.iface.vpfcmdq.valid :=
    valid_common &&
    mask_cmdq_ready && mask_ximm1q_ready && mask_ximm2q_ready && mask_cntq_ready &&
    enq_pfcmdq_mask_pfq && mask_pfximm1q_ready && mask_pfximm2q_ready && mask_pfcntq_ready

  io.iface.vpfximm1q.valid :=
    valid_common &&
    mask_cmdq_ready && mask_ximm1q_ready && mask_ximm2q_ready && mask_cntq_ready &&
    mask_pfcmdq_ready && enq_pfximm1q_mask_pfq && mask_pfximm2q_ready && mask_pfcntq_ready

  io.iface.vpfximm2q.valid :=
    valid_common &&
    mask_cmdq_ready && mask_ximm1q_ready && mask_ximm2q_ready && mask_cntq_ready &&
    mask_pfcmdq_ready && mask_pfximm1q_ready && enq_pfximm2q_mask_pfq && mask_pfcntq_ready

  io.iface.vpfcntq.valid :=
    valid_common &&
    mask_cmdq_ready && mask_ximm1q_ready && mask_ximm2q_ready && mask_cntq_ready &&
    mask_pfcmdq_ready && mask_pfximm1q_ready && mask_pfximm2q_ready && enq_pfcntq_mask_pfq

  io.replay := valid_common && (
    !mask_cmdq_ready || !mask_ximm1q_ready || !mask_ximm2q_ready || !mask_cntq_ready ||
    !mask_pfcmdq_ready || !mask_pfximm1q_ready || !mask_pfximm2q_ready || !mask_pfcntq_ready ||
    dec.io.sigs.vfence && !io.iface.vfence_ready
  )

  io.iface.exception := io.exception && io.sr_ev

  val reg_hold = Reg(resetVal = Bool(false))

  when (valid_common && dec.io.sigs.xcpthold) { reg_hold := Bool(true) }
  when (io.eret) { reg_hold := Bool(false) }

  io.iface.evac := valid_common && dec.io.sigs.xcptevac
  io.iface.kill := valid_common && dec.io.sigs.xcptkill
  io.iface.hold := reg_hold

  io.vfence_ready := !io.sr_ev || io.iface.vfence_ready
  io.irq := io.iface.irq
  io.irq_cause := io.iface.irq_cause
}
