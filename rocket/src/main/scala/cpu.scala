package Top

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
  val imem    = new ioImem().flip();
  val vimem   = new ioImem().flip();
  val dmem    = new ioDmem().flip();
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
  val arb   = new rocketDmemArbiter();

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

  // connect DTLB to D$ arbiter, ctrl+dpath
  dtlb.io.cpu.invalidate  := dpath.io.ptbr_wen;
  dtlb.io.cpu.status      := dpath.io.ctrl.status;
  dtlb.io.cpu.req_val     := ctrl.io.dtlb_val;
  dtlb.io.cpu.req_kill    := ctrl.io.dtlb_kill;
  dtlb.io.cpu.req_cmd     := ctrl.io.dmem.req_cmd;
  dtlb.io.cpu.req_asid    := Bits(0,ASID_BITS); // FIXME: connect to PCR
  dtlb.io.cpu.req_vpn     := dpath.io.dmem.req_addr(VADDR_BITS,PGIDX_BITS);
  ctrl.io.xcpt_dtlb_ld    := dtlb.io.cpu.xcpt_ld; 
  ctrl.io.xcpt_dtlb_st    := dtlb.io.cpu.xcpt_st; 
  ctrl.io.dtlb_rdy        := dtlb.io.cpu.req_rdy;
  ctrl.io.dtlb_miss       := dtlb.io.cpu.resp_miss;
  ctrl.io.xcpt_ma_ld      := io.dmem.xcpt_ma_ld;  
  ctrl.io.xcpt_ma_st      := io.dmem.xcpt_ma_st;
  
  // connect page table walker to TLBs, page table base register (from PCR)
  // and D$ arbiter (selects between requests from pipeline and PTW, PTW has priority)
  ptw.io.dtlb             <> dtlb.io.ptw;
  ptw.io.itlb             <> itlb.io.ptw;
  ptw.io.ptbr             := dpath.io.ptbr;
  arb.io.ptw              <> ptw.io.dmem;
  arb.io.mem              <> io.dmem
  
  // connect arbiter to ctrl+dpath+DTLB
  arb.io.cpu.req_val      := ctrl.io.dmem.req_val;
  arb.io.cpu.req_cmd      := ctrl.io.dmem.req_cmd;
  arb.io.cpu.req_type     := ctrl.io.dmem.req_type;
  arb.io.cpu.req_kill     := ctrl.io.dmem.req_kill;
  arb.io.cpu.req_idx      := dpath.io.dmem.req_addr(PGIDX_BITS-1,0);
  arb.io.cpu.req_ppn      := dtlb.io.cpu.resp_ppn;
  arb.io.cpu.req_data     := dpath.io.dmem.req_data;
  arb.io.cpu.req_tag      := dpath.io.dmem.req_tag;
  ctrl.io.dmem.req_rdy    := dtlb.io.cpu.req_rdy && arb.io.cpu.req_rdy;
  ctrl.io.dmem.resp_miss  := arb.io.cpu.resp_miss;
  ctrl.io.dmem.resp_replay:= arb.io.cpu.resp_replay;
  ctrl.io.dmem.resp_nack  := arb.io.cpu.resp_nack;
  dpath.io.dmem.resp_val  := arb.io.cpu.resp_val;
  dpath.io.dmem.resp_miss := arb.io.cpu.resp_miss;
  dpath.io.dmem.resp_replay := arb.io.cpu.resp_replay;
  dpath.io.dmem.resp_type := io.dmem.resp_type;
  dpath.io.dmem.resp_tag  := arb.io.cpu.resp_tag;
  dpath.io.dmem.resp_data := arb.io.cpu.resp_data;  
  dpath.io.dmem.resp_data_subword := io.dmem.resp_data_subword;

  var fpu: rocketFPU = null
  if (HAVE_FPU)
  {
    fpu = new rocketFPU(4,6)
    dpath.io.fpu <> fpu.io.dpath
    ctrl.io.fpu <> fpu.io.ctrl
  }
  else
    ctrl.io.fpu.dec.valid := Bool(false)

  if (HAVE_VEC)
  {
    dpath.io.vec_ctrl <> ctrl.io.vec_dpath

    val vu = new vu()

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

    ctrl.io.vec_iface.vcmdq_ready := vu.io.vec_cmdq.ready
    ctrl.io.vec_iface.vximm1q_ready := vu.io.vec_ximm1q.ready
    ctrl.io.vec_iface.vximm2q_ready := vu.io.vec_ximm2q.ready
    ctrl.io.vec_iface.vackq_valid := vu.io.vec_ackq.valid
    vu.io.vec_ackq.ready := ctrl.io.vec_iface.vackq_ready

    // hooking up vector memory interface
    ctrl.io.ext_mem.req_val := vu.io.dmem_req.valid
    ctrl.io.ext_mem.req_cmd := vu.io.dmem_req.bits.cmd
    ctrl.io.ext_mem.req_type := vu.io.dmem_req.bits.typ

    dpath.io.ext_mem.req_val := vu.io.dmem_req.valid
    dpath.io.ext_mem.req_idx := vu.io.dmem_req.bits.idx
    dpath.io.ext_mem.req_ppn := vu.io.dmem_req.bits.ppn
    dpath.io.ext_mem.req_data := vu.io.dmem_req.bits.data
    dpath.io.ext_mem.req_tag := vu.io.dmem_req.bits.tag

    vu.io.dmem_resp.valid := dpath.io.ext_mem.resp_val
    vu.io.dmem_resp.bits.nack := ctrl.io.ext_mem.resp_nack
    vu.io.dmem_resp.bits.data := dpath.io.ext_mem.resp_data
    vu.io.dmem_resp.bits.tag := dpath.io.ext_mem.resp_tag
    vu.io.dmem_resp.bits.typ := dpath.io.ext_mem.resp_type

    fpu.io.sfma.valid := Bool(false)
    fpu.io.dfma.valid := Bool(false)
  }
  else
  {
    ctrl.io.ext_mem.req_val := Bool(false)
    dpath.io.ext_mem.req_val := Bool(false)

    if (HAVE_FPU)
    {
      fpu.io.sfma.valid := Bool(false)
      fpu.io.dfma.valid := Bool(false)
    }
  }
}
