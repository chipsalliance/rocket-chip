package rocket

import Chisel._;
import Node._;
import Constants._;
import hwacha._

class ioDebug(view: List[String] = null) extends Bundle(view)
{
  val error_mode  = Bool(OUTPUT);
}

class ioRocket extends Bundle()
{
  val debug   = new ioDebug();
  val host    = new ioHTIF();
  val imem    = new ioImem().flip
  val vimem   = new ioImem().flip
  val dmem    = new ioDmem().flip
}

class rocketProc(resetSignal: Bool = null) extends Component(resetSignal)
{
  val io    = new ioRocket();
   
  val ctrl  = new rocketCtrl();      
  val dpath = new rocketDpath();

  val dtlb  = new rocketDTLB(DTLB_ENTRIES);
  val itlb  = new rocketITLB(ITLB_ENTRIES);
  val vitlb = new rocketITLB(ITLB_ENTRIES);
  val ptw   = new rocketPTW();
  val arb   = new rocketDmemArbiter(DCACHE_PORTS)

  var vu: vu = null
  if (HAVE_VEC)
  {
    vu = new vu()
    // cpu, vector prefetch, and vector use the DTLB
    val dtlbarb = new hwacha.Arbiter(3)({new ioDTLB_CPU_req()})
    val dtlbchosen = Reg(resetVal=Bits(DTLB_CPU,log2up(3)))
    when( dtlb.io.cpu_req.ready && dtlbarb.io.out.valid ) { dtlbchosen := dtlbarb.io.chosen }

    // tlb respones come out a cycle later
    val chosen_vec = dtlbchosen === Bits(DTLB_VEC)
    val chosen_pf = dtlbchosen === Bits(DTLB_VPF)
    val chosen_cpu = dtlbchosen === Bits(DTLB_CPU)

    dtlbarb.io.in(DTLB_VEC) <> vu.io.vec_tlb_req

    vu.io.vec_tlb_resp.xcpt_ld := chosen_vec && dtlb.io.cpu_resp.xcpt_ld
    vu.io.vec_tlb_resp.xcpt_st := chosen_vec && dtlb.io.cpu_resp.xcpt_st
    vu.io.vec_tlb_resp.miss := chosen_vec && dtlb.io.cpu_resp.miss
    vu.io.vec_tlb_resp.ppn := dtlb.io.cpu_resp.ppn

    // vector prefetch doesn't care about exceptions
    // and shouldn't cause any anyways
    dtlbarb.io.in(DTLB_VPF) <> vu.io.vec_pftlb_req

    vu.io.vec_pftlb_resp.xcpt_ld := Bool(false)
    vu.io.vec_pftlb_resp.xcpt_st := Bool(false)
    vu.io.vec_pftlb_resp.miss := chosen_pf && dtlb.io.cpu_resp.miss
    vu.io.vec_pftlb_resp.ppn := dtlb.io.cpu_resp.ppn

    // connect DTLB to ctrl+dpath
    dtlbarb.io.in(DTLB_CPU).valid := ctrl.io.dtlb_val
    dtlbarb.io.in(DTLB_CPU).bits.kill := ctrl.io.dtlb_kill
    dtlbarb.io.in(DTLB_CPU).bits.cmd := ctrl.io.dmem.req_cmd
    dtlbarb.io.in(DTLB_CPU).bits.asid := Bits(0,ASID_BITS); // FIXME: connect to PCR
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
    dtlb.io.cpu_req.bits.cmd := ctrl.io.dmem.req_cmd
    dtlb.io.cpu_req.bits.asid := Bits(0,ASID_BITS); // FIXME: connect to PCR
    dtlb.io.cpu_req.bits.vpn := dpath.io.dtlb.vpn
    ctrl.io.xcpt_dtlb_ld := dtlb.io.cpu_resp.xcpt_ld
    ctrl.io.xcpt_dtlb_st := dtlb.io.cpu_resp.xcpt_st
    ctrl.io.dtlb_rdy := dtlb.io.cpu_req.ready
    ctrl.io.dtlb_miss := dtlb.io.cpu_resp.miss
  }

  dtlb.io.invalidate := dpath.io.ptbr_wen
  dtlb.io.status := dpath.io.ctrl.status

  arb.io.requestor(DMEM_CPU).req_ppn := dtlb.io.cpu_resp.ppn
  ctrl.io.dmem.req_rdy := dtlb.io.cpu_req.ready && arb.io.requestor(DMEM_CPU).req_rdy

  // connect page table walker to TLBs, page table base register (from PCR)
  // and D$ arbiter (selects between requests from pipeline and PTW, PTW has priority)
  ptw.io.dtlb             <> dtlb.io.ptw;
  ptw.io.itlb             <> itlb.io.ptw;
  ptw.io.ptbr             := dpath.io.ptbr;
  arb.io.requestor(DMEM_PTW) <> ptw.io.dmem
  arb.io.dmem             <> io.dmem

  ctrl.io.dpath             <> dpath.io.ctrl;
  dpath.io.host             <> io.host;
  dpath.io.debug            <> io.debug;

  // FIXME: try to make this more compact
  
  // connect ITLB to I$, ctrl, dpath
  itlb.io.cpu.invalidate  := dpath.io.ptbr_wen;
  itlb.io.cpu.status      := dpath.io.ctrl.status;
  itlb.io.cpu.req_val     := ctrl.io.imem.req_val;  
  itlb.io.cpu.req_asid    := Bits(0,ASID_BITS); // FIXME: connect to PCR
  itlb.io.cpu.req_vpn     := dpath.io.imem.req_addr(VADDR_BITS,PGIDX_BITS);
  io.imem.req_idx         := dpath.io.imem.req_addr(PGIDX_BITS-1,0);
  io.imem.req_ppn         := itlb.io.cpu.resp_ppn;
  io.imem.req_val         := ctrl.io.imem.req_val;
  io.imem.invalidate      := ctrl.io.dpath.flush_inst;
  ctrl.io.imem.resp_val   := io.imem.resp_val;
  dpath.io.imem.resp_data := io.imem.resp_data;
  ctrl.io.xcpt_itlb       := itlb.io.cpu.exception;
  io.imem.itlb_miss       := itlb.io.cpu.resp_miss;

  // connect arbiter to ctrl+dpath+DTLB
  arb.io.requestor(DMEM_CPU) <> ctrl.io.dmem
  arb.io.requestor(DMEM_CPU) <> dpath.io.dmem

  var fpu: rocketFPU = null
  if (HAVE_FPU)
  {
    fpu = new rocketFPU(4,6)
    dpath.io.fpu <> fpu.io.dpath
    ctrl.io.fpu <> fpu.io.ctrl
  }
  else
  {
    ctrl.io.fpu.dec.valid := Bool(false)
    ctrl.io.fpu.dec.wen := Bool(false)
  }

  if (HAVE_VEC)
  {
    dpath.io.vec_ctrl <> ctrl.io.vec_dpath

    // hooking up vector I$
    vitlb.io.cpu.invalidate := dpath.io.ptbr_wen
    vitlb.io.cpu.status     := dpath.io.ctrl.status
    vitlb.io.cpu.req_val    := vu.io.imem_req.valid  
    vitlb.io.cpu.req_asid   := Bits(0,ASID_BITS) // FIXME: connect to PCR
    vitlb.io.cpu.req_vpn    := vu.io.imem_req.bits(VADDR_BITS,PGIDX_BITS).toUFix
    io.vimem.req_idx        := vu.io.imem_req.bits(PGIDX_BITS-1,0)
    io.vimem.req_ppn        := vitlb.io.cpu.resp_ppn
    io.vimem.req_val        := vu.io.imem_req.valid
    io.vimem.invalidate     := ctrl.io.dpath.flush_inst
    vu.io.imem_req.ready    := Bool(true)
    vu.io.imem_resp.valid   := io.vimem.resp_val
    vu.io.imem_resp.bits    := io.vimem.resp_data
    // handle vitlb.io.cpu.exception
    io.vimem.itlb_miss      := vitlb.io.cpu.resp_miss

    // hooking up vector command queues
    vu.io.vec_cmdq.valid := ctrl.io.vec_iface.vcmdq_valid
    vu.io.vec_cmdq.bits := dpath.io.vec_iface.vcmdq_bits
    vu.io.vec_ximm1q.valid := ctrl.io.vec_iface.vximm1q_valid
    vu.io.vec_ximm1q.bits := dpath.io.vec_iface.vximm1q_bits
    vu.io.vec_ximm2q.valid := ctrl.io.vec_iface.vximm2q_valid
    vu.io.vec_ximm2q.bits := dpath.io.vec_iface.vximm2q_bits
    vu.io.vec_cntq.valid := ctrl.io.vec_iface.vcntq_valid
    vu.io.vec_cntq.bits := dpath.io.vec_iface.vcntq_bits

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
    ctrl.io.vec_iface.exception_ack_valid := vu.io.xcpt.exception_ack_valid
    vu.io.xcpt.exception_ack_ready := ctrl.io.vec_iface.exception_ack_ready
    vu.io.xcpt.evac := ctrl.io.vec_iface.evac
    vu.io.xcpt.evac_addr := dpath.io.vec_iface.evac_addr.toUFix
    vu.io.xcpt.kill := ctrl.io.vec_iface.kill
    vu.io.xcpt.hold := ctrl.io.vec_iface.hold

    // hooking up vector memory interface
    val storegen = new StoreDataGen
    storegen.io.typ := vu.io.dmem_req.bits.typ
    storegen.io.din := vu.io.dmem_req.bits.data

    arb.io.requestor(DMEM_VU).req_val := vu.io.dmem_req.valid
    arb.io.requestor(DMEM_VU).req_kill := Reg(vu.io.dmem_req.bits.kill)
    arb.io.requestor(DMEM_VU).req_cmd := vu.io.dmem_req.bits.cmd
    arb.io.requestor(DMEM_VU).req_type := vu.io.dmem_req.bits.typ
    arb.io.requestor(DMEM_VU).req_idx := vu.io.dmem_req.bits.idx
    arb.io.requestor(DMEM_VU).req_ppn := Reg(vu.io.dmem_req.bits.ppn)
    arb.io.requestor(DMEM_VU).req_data := Reg(storegen.io.dout)
    arb.io.requestor(DMEM_VU).req_tag := vu.io.dmem_req.bits.tag

    vu.io.dmem_resp.valid := Reg(arb.io.requestor(DMEM_VU).resp_val)
    // the vu doesn't look at the ready signal, it's simply a nack
    // but should be delayed one cycle to match the nack semantics
    vu.io.dmem_resp.bits.nack := arb.io.requestor(DMEM_VU).resp_nack || Reg(!arb.io.requestor(DMEM_VU).req_rdy)
    vu.io.dmem_resp.bits.data := arb.io.requestor(DMEM_VU).resp_data_subword
    vu.io.dmem_resp.bits.tag := Reg(arb.io.requestor(DMEM_VU).resp_tag)
    vu.io.dmem_resp.bits.typ := Reg(arb.io.requestor(DMEM_VU).resp_type)

    // share vector integer multiplier with rocket
    dpath.io.vec_imul_req <> vu.io.cp_imul_req
    dpath.io.vec_imul_resp <> vu.io.cp_imul_resp

    // share sfma and dfma pipelines with rocket
    fpu.io.sfma <> vu.io.cp_sfma
    fpu.io.dfma <> vu.io.cp_dfma
  }
  else
  {
    arb.io.requestor(DMEM_VU).req_val := Bool(false)
    if (HAVE_FPU)
    {
      fpu.io.sfma.valid := Bool(false)
      fpu.io.dfma.valid := Bool(false)
    }
  }
}
