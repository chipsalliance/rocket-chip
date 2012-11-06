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

class rocketProc(implicit conf: RocketConfiguration) extends Component
{
  val io    = new ioRocket
   
  val ctrl  = new Control
  val dpath = new Datapath

  val ptw = Vec(0) { new IOTLBPTW }
  val arb = new HellaCacheArbiter(DCACHE_PORTS)

  var vu: vu = null
  if (HAVE_VEC)
  {
    vu = new vu()

    val vdtlb = new rocketTLB(8)
    vdtlb.io.invalidate := dpath.io.ptbr_wen
    vdtlb.io.status := dpath.io.ctrl.status
    ptw += vdtlb.io.ptw

    vdtlb.io.cpu_req <> vu.io.vec_tlb_req
    vu.io.vec_tlb_resp := vdtlb.io.cpu_resp
    vu.io.vec_tlb_resp.xcpt_pf := Bool(false)

    val pftlb = new rocketTLB(2)
    pftlb.io.invalidate := dpath.io.ptbr_wen
    pftlb.io.status := dpath.io.ctrl.status
    pftlb.io.cpu_req <> vu.io.vec_pftlb_req
    ptw += pftlb.io.ptw

    vu.io.vec_pftlb_resp := pftlb.io.cpu_resp
    vu.io.vec_pftlb_resp.xcpt_ld := Bool(false)
    vu.io.vec_pftlb_resp.xcpt_st := Bool(false)
  }

  // connect DTLB to ctrl+dpath
  val dtlb = new rocketTLB(DTLB_ENTRIES)
  dtlb.io.invalidate := dpath.io.ptbr_wen
  dtlb.io.status := dpath.io.ctrl.status
  ptw += dtlb.io.ptw

  dtlb.io.cpu_req.valid := ctrl.io.dtlb_val
  dtlb.io.cpu_req.bits.kill := ctrl.io.dtlb_kill
  dtlb.io.cpu_req.bits.cmd := ctrl.io.dmem.req.bits.cmd
  dtlb.io.cpu_req.bits.asid := UFix(0)
  dtlb.io.cpu_req.bits.vpn := dpath.io.dtlb.vpn
  ctrl.io.xcpt_dtlb_ld := dtlb.io.cpu_resp.xcpt_ld
  ctrl.io.xcpt_dtlb_st := dtlb.io.cpu_resp.xcpt_st
  ctrl.io.dtlb_rdy := dtlb.io.cpu_req.ready
  ctrl.io.dtlb_miss := dtlb.io.cpu_resp.miss

  arb.io.requestor(DCACHE_CPU).req.bits.ppn := dtlb.io.cpu_resp.ppn

  ctrl.io.dpath <> dpath.io.ctrl
  dpath.io.host <> io.host

  ctrl.io.imem <> io.imem
  dpath.io.imem <> io.imem

  ctrl.io.dmem <> arb.io.requestor(DCACHE_CPU)
  dpath.io.dmem <> arb.io.requestor(DCACHE_CPU)

  var fpu: rocketFPU = null
  if (HAVE_FPU)
  {
    fpu = new rocketFPU(4,6)
    dpath.io.fpu <> fpu.io.dpath
    ctrl.io.fpu <> fpu.io.ctrl
  }

  if (HAVE_VEC)
  {
    dpath.io.vec_ctrl <> ctrl.io.vec_dpath

    // hooking up vector I$
    ptw += io.vimem.ptw
    io.vimem.req.bits.status := dpath.io.ctrl.status
    io.vimem.req.bits.pc := vu.io.imem_req.bits
    io.vimem.req.valid := vu.io.imem_req.valid
    io.vimem.req.bits.invalidate := ctrl.io.dpath.flush_inst
    io.vimem.req.bits.invalidateTLB := dpath.io.ptbr_wen
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
    arb.io.requestor(DCACHE_VU).req.valid := vu.io.dmem_req.valid
    arb.io.requestor(DCACHE_VU).req.bits.kill := vu.io.dmem_req.bits.kill
    arb.io.requestor(DCACHE_VU).req.bits.cmd := vu.io.dmem_req.bits.cmd
    arb.io.requestor(DCACHE_VU).req.bits.typ := vu.io.dmem_req.bits.typ
    arb.io.requestor(DCACHE_VU).req.bits.idx := vu.io.dmem_req.bits.idx
    arb.io.requestor(DCACHE_VU).req.bits.ppn := Reg(vu.io.dmem_req.bits.ppn)
    arb.io.requestor(DCACHE_VU).req.bits.data := Reg(StoreGen(vu.io.dmem_req.bits.typ, Bits(0), vu.io.dmem_req.bits.data).data)
    arb.io.requestor(DCACHE_VU).req.bits.tag := vu.io.dmem_req.bits.tag

    vu.io.dmem_req.ready := arb.io.requestor(DCACHE_VU).req.ready
    vu.io.dmem_resp.valid := Reg(arb.io.requestor(DCACHE_VU).resp.valid)
    vu.io.dmem_resp.bits.nack := arb.io.requestor(DCACHE_VU).resp.bits.nack
    vu.io.dmem_resp.bits.data := arb.io.requestor(DCACHE_VU).resp.bits.data_subword
    vu.io.dmem_resp.bits.tag := Reg(arb.io.requestor(DCACHE_VU).resp.bits.tag)
    vu.io.dmem_resp.bits.typ := Reg(arb.io.requestor(DCACHE_VU).resp.bits.typ)

    // share vector integer multiplier with rocket
    dpath.io.vec_imul_req <> vu.io.cp_imul_req
    dpath.io.vec_imul_resp <> vu.io.cp_imul_resp

    // share sfma and dfma pipelines with rocket
    fpu.io.sfma <> vu.io.cp_sfma
    fpu.io.dfma <> vu.io.cp_dfma
  }
  else
  {
    arb.io.requestor(DCACHE_VU).req.valid := Bool(false)
    if (HAVE_FPU)
    {
      fpu.io.sfma.valid := Bool(false)
      fpu.io.dfma.valid := Bool(false)
    }
  }

  ptw += io.imem.ptw
  val thePTW = new PTW(ptw.length)
  thePTW.io.requestor <> ptw
  thePTW.io.ptbr := dpath.io.ptbr;
  arb.io.requestor(DCACHE_PTW) <> thePTW.io.mem

  arb.io.mem <> io.dmem
}
