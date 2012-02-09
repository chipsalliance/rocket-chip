package Top

import Chisel._;
import Node._;
import Constants._;
import hwacha._

class ioDebug(view: List[String] = null) extends Bundle(view)
{
  val error_mode  = Bool(OUTPUT);
}

class ioHost(view: List[String] = null) extends Bundle(view)
{
  val from_wen   = Bool(INPUT);
  val from       = Bits(64, INPUT);
  val to         = Bits(64, OUTPUT);
}

class ioConsole(view: List[String] = null) extends Bundle(view)
{
  val rdy   = Bool(INPUT);
  val valid = Bool(OUTPUT);
  val bits  = Bits(8, OUTPUT);
}

class ioRocket extends Bundle()
{
  val debug   = new ioDebug();
  val console = new ioConsole();
  val host    = new ioHost();
  val imem    = new ioImem().flip();
  val dmem    = new ioDmem().flip();
}

class rocketProc extends Component
{
  val io    = new ioRocket();
   
  val ctrl  = new rocketCtrl();      
  val dpath = new rocketDpath();

  val dtlb  = new rocketDTLB(DTLB_ENTRIES);
  val itlb  = new rocketITLB(ITLB_ENTRIES);
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
  io.imem.invalidate      := ctrl.io.flush_inst;
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
  dpath.io.dmem.resp_tag  := arb.io.cpu.resp_tag;
  dpath.io.dmem.resp_data := arb.io.cpu.resp_data;  
  dpath.io.dmem.resp_data_subword := io.dmem.resp_data_subword;

  io.console.bits     := dpath.io.console.bits;
  io.console.valid    := dpath.io.console.valid;
  ctrl.io.console.rdy := io.console.rdy;

  if (HAVE_FPU)
  {
    val fpu = new rocketFPU
    fpu.io.dmem.resp_val := arb.io.cpu.resp_val;
    fpu.io.dmem.resp_tag := arb.io.cpu.resp_tag;
    fpu.io.dmem.resp_data := arb.io.cpu.resp_data;
    dpath.io.fpu <> fpu.io.dpath
  }

  if (HAVE_VEC)
  {
    val vu = new vu()

    vu.io.vec_cmdq <> ctrl.io.vcmdq
    vu.io.vec_cmdq <> dpath.io.vcmdq
    vu.io.vec_ximm1q <> ctrl.io.vximm1q
    vu.io.vec_ximm1q <> dpath.io.vximm1q
    vu.io.vec_ximm2q <> ctrl.io.vximm2q
    vu.io.vec_ximm2q <> dpath.io.vximm2q
  }
}
