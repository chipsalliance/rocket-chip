package rocket

import Chisel._
import Node._
import Constants._
import hwacha._

class ioRocket(implicit conf: RocketConfiguration) extends Bundle
{
  val host    = new ioHTIF(conf.ntiles)
  val imem    = new IOCPUFrontend()(conf.icache)
  val vimem   = new IOCPUFrontend()(conf.icache)
  val dmem    = new ioHellaCache()(conf.dcache)
}

class Core(implicit conf: RocketConfiguration) extends Component
{
  val io    = new ioRocket
   
  val ctrl  = new Control
  val dpath = new Datapath

  ctrl.io.dpath <> dpath.io.ctrl
  dpath.io.host <> io.host

  ctrl.io.imem <> io.imem
  dpath.io.imem <> io.imem

  val dmemArb = new HellaCacheArbiter(if (HAVE_VEC) 3 else 2)
  dmemArb.io.mem <> io.dmem
  val dmem = dmemArb.io.requestor
  dmem(1) <> ctrl.io.dmem
  dmem(1) <> dpath.io.dmem

  val ptw = collection.mutable.ArrayBuffer(io.imem.ptw, io.dmem.ptw)

  val fpu: FPU = if (HAVE_FPU) {
    val fpu = new FPU(4,6)
    dpath.io.fpu <> fpu.io.dpath
    ctrl.io.fpu <> fpu.io.ctrl
    fpu
  } else null

  if (HAVE_VEC) {
    val vu = new vu()

    val vdtlb = new rocketTLB(8)
    ptw += vdtlb.io.ptw
    vdtlb.io.cpu_req <> vu.io.vec_tlb_req
    vu.io.vec_tlb_resp := vdtlb.io.cpu_resp
    vu.io.vec_tlb_resp.xcpt_pf := Bool(false)

    val pftlb = new rocketTLB(2)
    pftlb.io.cpu_req <> vu.io.vec_pftlb_req
    ptw += pftlb.io.ptw
    vu.io.vec_pftlb_resp := pftlb.io.cpu_resp
    vu.io.vec_pftlb_resp.xcpt_ld := Bool(false)
    vu.io.vec_pftlb_resp.xcpt_st := Bool(false)

    dpath.io.vec_ctrl <> ctrl.io.vec_dpath

    // hooking up vector I$
    ptw += io.vimem.ptw
    io.vimem.req.bits.pc := vu.io.imem_req.bits
    io.vimem.req.valid := vu.io.imem_req.valid
    io.vimem.req.bits.invalidate := ctrl.io.dpath.flush_inst
    vu.io.imem_resp.valid := io.vimem.resp.valid
    vu.io.imem_resp.bits.pc := io.vimem.resp.bits.pc
    vu.io.imem_resp.bits.data := io.vimem.resp.bits.data
    vu.io.imem_resp.bits.xcpt_ma := io.vimem.resp.bits.xcpt_ma
    vu.io.imem_resp.bits.xcpt_if := io.vimem.resp.bits.xcpt_if
    io.vimem.resp.ready := vu.io.imem_resp.ready
    io.vimem.req.bits.mispredict := Bool(false)
    io.vimem.req.bits.taken := Bool(false)

    // hooking up vector command queues
    vu.io.vec_cmdq.valid := ctrl.io.vec_iface.vcmdq_valid
    vu.io.vec_cmdq.bits := dpath.io.vec_iface.vcmdq_bits
    vu.io.vec_ximm1q.valid := ctrl.io.vec_iface.vximm1q_valid
    vu.io.vec_ximm1q.bits := dpath.io.vec_iface.vximm1q_bits
    vu.io.vec_ximm2q.valid := ctrl.io.vec_iface.vximm2q_valid
    vu.io.vec_ximm2q.bits := dpath.io.vec_iface.vximm2q_bits
    vu.io.vec_cntq.valid := ctrl.io.vec_iface.vcntq_valid
    vu.io.vec_cntq.bits := Cat(dpath.io.vec_iface.vcntq_last, dpath.io.vec_iface.vcntq_bits)

    // prefetch queues
    vu.io.vec_pfcmdq.valid := ctrl.io.vec_iface.vpfcmdq_valid
    vu.io.vec_pfcmdq.bits := dpath.io.vec_iface.vcmdq_bits
    vu.io.vec_pfximm1q.valid := ctrl.io.vec_iface.vpfximm1q_valid
    vu.io.vec_pfximm1q.bits := dpath.io.vec_iface.vximm1q_bits
    vu.io.vec_pfximm2q.valid := ctrl.io.vec_iface.vpfximm2q_valid
    vu.io.vec_pfximm2q.bits := dpath.io.vec_iface.vximm2q_bits
    vu.io.vec_pfcntq.valid := ctrl.io.vec_iface.vpfcntq_valid
    vu.io.vec_pfcntq.bits := dpath.io.vec_iface.vcntq_bits

    // don't have to use pf ready signals
    // if cmdq is not a load or store
    ctrl.io.vec_iface.vcmdq_ready := vu.io.vec_cmdq.ready
    ctrl.io.vec_iface.vximm1q_ready := vu.io.vec_ximm1q.ready
    ctrl.io.vec_iface.vximm2q_ready := vu.io.vec_ximm2q.ready
    ctrl.io.vec_iface.vcntq_ready := vu.io.vec_cntq.ready
    ctrl.io.vec_iface.vpfcmdq_ready := vu.io.vec_pfcmdq.ready
    ctrl.io.vec_iface.vpfximm1q_ready := vu.io.vec_pfximm1q.ready
    ctrl.io.vec_iface.vpfximm2q_ready := vu.io.vec_pfximm2q.ready
    ctrl.io.vec_iface.vpfcntq_ready := vu.io.vec_pfcntq.ready

    // user level vector command queue ready signals
    ctrl.io.vec_iface.vcmdq_user_ready := vu.io.vec_cmdq_user_ready
    ctrl.io.vec_iface.vximm1q_user_ready := vu.io.vec_ximm1q_user_ready
    ctrl.io.vec_iface.vximm2q_user_ready := vu.io.vec_ximm2q_user_ready

    // fences
    ctrl.io.vec_iface.vfence_ready := vu.io.vec_fence_ready

    // irqs
    ctrl.io.vec_iface.irq := vu.io.irq
    ctrl.io.vec_iface.irq_cause := vu.io.irq_cause
    dpath.io.vec_iface.irq_aux := vu.io.irq_aux

    // exceptions
    vu.io.xcpt.exception := ctrl.io.vec_iface.exception
    vu.io.xcpt.evac := ctrl.io.vec_iface.evac
    vu.io.xcpt.evac_addr := dpath.io.vec_iface.evac_addr.toUFix
    vu.io.xcpt.kill := ctrl.io.vec_iface.kill
    vu.io.xcpt.hold := ctrl.io.vec_iface.hold

    // hooking up vector memory interface
    dmem(2).req.valid := vu.io.dmem_req.valid
    dmem(2).req.bits := vu.io.dmem_req.bits
    dmem(2).req.bits.data := Reg(StoreGen(vu.io.dmem_req.bits.typ, Bits(0), vu.io.dmem_req.bits.data).data)

    vu.io.dmem_req.ready := dmem(2).req.ready
    vu.io.dmem_resp.valid := Reg(dmem(2).resp.valid)
    vu.io.dmem_resp.bits.nack := dmem(2).resp.bits.nack
    vu.io.dmem_resp.bits.data := dmem(2).resp.bits.data_subword
    vu.io.dmem_resp.bits.tag := Reg(dmem(2).resp.bits.tag)
    vu.io.dmem_resp.bits.typ := Reg(dmem(2).resp.bits.typ)

    // share vector integer multiplier with rocket
    dpath.io.vec_imul_req <> vu.io.cp_imul_req
    dpath.io.vec_imul_resp <> vu.io.cp_imul_resp

    // share sfma and dfma pipelines with rocket
    fpu.io.sfma <> vu.io.cp_sfma
    fpu.io.dfma <> vu.io.cp_dfma
  } else if (fpu != null) {
    fpu.io.sfma.valid := Bool(false)
    fpu.io.dfma.valid := Bool(false)
  }

  val thePTW = new PTW(ptw.length)
  ptw zip thePTW.io.requestor map { case (a, b) => a <> b }
  thePTW.io.dpath <> dpath.io.ptw
  dmem(0) <> thePTW.io.mem
}
