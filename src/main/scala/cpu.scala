package rocket

import Chisel._
import Node._
import Constants._
import hwacha._

class ioRocket(implicit conf: RocketConfiguration) extends Bundle
{
  val host    = new ioHTIF
  val imem    = new IOCPUFrontend
  val vimem   = new IOCPUFrontend
  val dmem    = new ioHellaCache
}

class rocketProc(implicit conf: RocketConfiguration) extends Component
{
  val io    = new ioRocket
   
  val ctrl  = new rocketCtrl
  val dpath = new rocketDpath

  val dtlb  = new rocketTLB(DTLB_ENTRIES);
  val ptw   = new rocketPTW(if (HAVE_VEC) 3 else 2)
  val arb   = new rocketHellaCacheArbiter(DCACHE_PORTS)

  var vu: vu = null
  if (HAVE_VEC)
  {
    vu = new vu()
    // cpu, vector prefetch, and vector use the DTLB
    val dtlbarb = new RRArbiter(DTLB_PORTS)({new ioDTLB_CPU_req_bundle()})
    val dtlbchosen = Reg(resetVal=Bits(DTLB_CPU,log2Up(DTLB_PORTS)))
    when( dtlb.io.cpu_req.ready && dtlbarb.io.out.valid ) { dtlbchosen := dtlbarb.io.chosen }

    // tlb respones come out a cycle later
    val chosen_vec = dtlbchosen === Bits(DTLB_VEC)
    val chosen_pf = dtlbchosen === Bits(DTLB_VPF)
    val chosen_cpu = dtlbchosen === Bits(DTLB_CPU)

    dtlbarb.io.in(DTLB_VEC) <> vu.io.vec_tlb_req

    vu.io.vec_tlb_resp.xcpt_ld := chosen_vec && dtlb.io.cpu_resp.xcpt_ld
    vu.io.vec_tlb_resp.xcpt_st := chosen_vec && dtlb.io.cpu_resp.xcpt_st
    vu.io.vec_tlb_resp.xcpt_pf := Bool(false)
    vu.io.vec_tlb_resp.miss := chosen_vec && dtlb.io.cpu_resp.miss
    vu.io.vec_tlb_resp.ppn := dtlb.io.cpu_resp.ppn

    dtlbarb.io.in(DTLB_VPF) <> vu.io.vec_pftlb_req

    vu.io.vec_pftlb_resp.xcpt_ld := Bool(false)
    vu.io.vec_pftlb_resp.xcpt_st := Bool(false)
    vu.io.vec_pftlb_resp.xcpt_pf := chosen_pf && dtlb.io.cpu_resp.xcpt_pf
    vu.io.vec_pftlb_resp.miss := chosen_pf && dtlb.io.cpu_resp.miss
    vu.io.vec_pftlb_resp.ppn := dtlb.io.cpu_resp.ppn

    // connect DTLB to ctrl+dpath
    dtlbarb.io.in(DTLB_CPU).valid := ctrl.io.dtlb_val
    dtlbarb.io.in(DTLB_CPU).bits.kill := ctrl.io.dtlb_kill
    dtlbarb.io.in(DTLB_CPU).bits.cmd := ctrl.io.dmem.req.bits.cmd
    dtlbarb.io.in(DTLB_CPU).bits.asid := UFix(0)
    dtlbarb.io.in(DTLB_CPU).bits.vpn := dpath.io.dtlb.vpn
    ctrl.io.dtlb_rdy := dtlbarb.io.in(DTLB_CPU).ready

    ctrl.io.xcpt_dtlb_ld := chosen_cpu && dtlb.io.cpu_resp.xcpt_ld
    ctrl.io.xcpt_dtlb_st := chosen_cpu && dtlb.io.cpu_resp.xcpt_st
    ctrl.io.dtlb_miss := chosen_cpu && dtlb.io.cpu_resp.miss

    dtlb.io.cpu_req <> dtlbarb.io.out
  }
  else
  {
    // connect DTLB to ctrl+dpath
    dtlb.io.cpu_req.valid := ctrl.io.dtlb_val
    dtlb.io.cpu_req.bits.kill := ctrl.io.dtlb_kill
    dtlb.io.cpu_req.bits.cmd := ctrl.io.dmem.req.bits.cmd
    dtlb.io.cpu_req.bits.asid := UFix(0)
    dtlb.io.cpu_req.bits.vpn := dpath.io.dtlb.vpn
    ctrl.io.xcpt_dtlb_ld := dtlb.io.cpu_resp.xcpt_ld
    ctrl.io.xcpt_dtlb_st := dtlb.io.cpu_resp.xcpt_st
    ctrl.io.dtlb_rdy := dtlb.io.cpu_req.ready
    ctrl.io.dtlb_miss := dtlb.io.cpu_resp.miss
  }

  dtlb.io.invalidate := dpath.io.ptbr_wen
  dtlb.io.status := dpath.io.ctrl.status

  arb.io.requestor(DCACHE_CPU).req.bits.ppn := dtlb.io.cpu_resp.ppn
  ctrl.io.dmem.req.ready := dtlb.io.cpu_req.ready && arb.io.requestor(DCACHE_CPU).req.ready

  // connect page table walker to TLBs, page table base register (from PCR)
  // and D$ arbiter (selects between requests from pipeline and PTW, PTW has priority)
  ptw.io.requestor(0)     <> io.imem.ptw
  ptw.io.requestor(1)     <> dtlb.io.ptw
  ptw.io.ptbr             := dpath.io.ptbr;
  arb.io.requestor(DCACHE_PTW) <> ptw.io.mem
  arb.io.mem             <> io.dmem

  ctrl.io.dpath             <> dpath.io.ctrl;
  dpath.io.host             <> io.host;

  // FIXME: try to make this more compact
  
  // connect I$
  ctrl.io.imem <> io.imem
  dpath.io.imem <> io.imem

  // connect arbiter to ctrl+dpath+DTLB
  //TODO: views on nested bundles?
  arb.io.requestor(DCACHE_CPU).resp <> ctrl.io.dmem.resp
  arb.io.requestor(DCACHE_CPU).xcpt <> ctrl.io.dmem.xcpt
  arb.io.requestor(DCACHE_CPU).resp <> dpath.io.dmem.resp
  arb.io.requestor(DCACHE_CPU).req.valid     := ctrl.io.dmem.req.valid
  ctrl.io.dmem.req.ready := arb.io.requestor(DCACHE_CPU).req.ready
  arb.io.requestor(DCACHE_CPU).req.bits.kill := ctrl.io.dmem.req.bits.kill
  arb.io.requestor(DCACHE_CPU).req.bits.cmd  := ctrl.io.dmem.req.bits.cmd
  arb.io.requestor(DCACHE_CPU).req.bits.typ  := ctrl.io.dmem.req.bits.typ
  arb.io.requestor(DCACHE_CPU).req.bits.idx  := dpath.io.dmem.req.bits.idx
  arb.io.requestor(DCACHE_CPU).req.bits.tag  := dpath.io.dmem.req.bits.tag
  arb.io.requestor(DCACHE_CPU).req.bits.data := dpath.io.dmem.req.bits.data

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
    ptw.io.requestor(2) <> io.vimem.ptw
    io.vimem.req.bits.status := dpath.io.ctrl.status
    io.vimem.req.bits.pc := vu.io.imem_req.bits.toUFix
    io.vimem.req.valid := vu.io.imem_req.valid
    io.vimem.req.bits.invalidate := ctrl.io.dpath.flush_inst
    io.vimem.req.bits.invalidateTLB := dpath.io.ptbr_wen
    vu.io.imem_req.ready := Bool(true)
    vu.io.imem_resp.valid := io.vimem.resp.valid
    vu.io.imem_resp.bits := io.vimem.resp.bits.data
    vu.io.vitlb_exception   := io.vimem.resp.bits.xcpt_if
    io.vimem.resp.ready := Bool(true)
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
    val storegen = new StoreDataGen
    storegen.io.typ := vu.io.dmem_req.bits.typ
    storegen.io.din := vu.io.dmem_req.bits.data

    arb.io.requestor(DCACHE_VU).req.valid := vu.io.dmem_req.valid
    arb.io.requestor(DCACHE_VU).req.bits.kill := vu.io.dmem_req.bits.kill
    arb.io.requestor(DCACHE_VU).req.bits.cmd := vu.io.dmem_req.bits.cmd
    arb.io.requestor(DCACHE_VU).req.bits.typ := vu.io.dmem_req.bits.typ
    arb.io.requestor(DCACHE_VU).req.bits.idx := vu.io.dmem_req.bits.idx
    arb.io.requestor(DCACHE_VU).req.bits.ppn := Reg(vu.io.dmem_req.bits.ppn)
    arb.io.requestor(DCACHE_VU).req.bits.data := Reg(storegen.io.dout)
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
}
