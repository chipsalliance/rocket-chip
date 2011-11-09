package Top {

import Chisel._;
import Node._;
import Constants._;

class ioDebug extends Bundle()
{
  val error_mode  = Bool('output);
  val log_control = Bool('output);
  val id_valid    = Bool('output);
  val ex_valid    = Bool('output);
  val mem_valid   = Bool('output);
  val wb_valid    = Bool('output);
}

class ioHost(view: List[String] = null) extends Bundle(view)
{
  val start      = Bool('input);
  val from_wen   = Bool('input);
  val from       = Bits(32, 'input);
  val to         = Bits(32, 'output);
}

class ioConsole(view: List[String] = null) extends Bundle(view)
{
  val rdy   = Bool('input);
  val valid = Bool('output);
  val bits  = Bits(8, 'output);
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

  val itlb  = new rocketITLB(ITLB_ENTRIES);
  val ptw   = new rocketPTW();
  val arb   = new rocketDmemArbiter();

  ctrl.io.dpath             <> dpath.io.ctrl;
//  ctrl.io.dmem              ^^ io.dmem;
  ctrl.io.host.start        ^^ io.host.start;
//  ctrl.io.imem              ^^ io.imem;
  
//  dpath.io.dmem             ^^ io.dmem;
//  dpath.io.imem.req_addr    ^^ io.imem.req_addr;
  dpath.io.imem.resp_data   ^^ io.imem.resp_data;
  dpath.io.host             ^^ io.host;
  dpath.io.debug            ^^ io.debug;

  itlb.io.cpu.invalidate  := Bool(false);
  itlb.io.cpu.status      := dpath.io.ctrl.status;
  itlb.io.cpu.req_val     := ctrl.io.imem.req_val;
  ctrl.io.imem.req_rdy    := itlb.io.cpu.req_rdy && io.imem.req_rdy;
  
  itlb.io.cpu.req_asid    := Bits(0,ASID_BITS); // FIXME: connect to PCR
  itlb.io.cpu.req_vpn     := dpath.io.imem.req_addr(VADDR_BITS-1,PGIDX_BITS);
  
  io.imem.req_val         := itlb.io.cpu.resp_val;
  io.imem.req_addr        := Cat(itlb.io.cpu.resp_ppn, dpath.io.imem.req_addr(PGIDX_BITS-1,0)).toUFix;
  
  ctrl.io.imem.resp_val   := io.imem.resp_val;
  dpath.io.itlb_xcpt      := itlb.io.cpu.exception;
  
  ptw.io.itlb             <> itlb.io.ptw;
  ptw.io.ptbr             := dpath.io.ptbr;
  
  arb.io.ptw              <> ptw.io.dmem;
  arb.io.mem              ^^ io.dmem
  
  arb.io.cpu.req_val      := ctrl.io.dmem.req_val;
  arb.io.cpu.req_cmd      := ctrl.io.dmem.req_cmd;
  arb.io.cpu.req_type     := ctrl.io.dmem.req_type;
  arb.io.cpu.req_addr     := dpath.io.dmem.req_addr;
  arb.io.cpu.req_data     := dpath.io.dmem.req_data;
  arb.io.cpu.req_tag      := dpath.io.dmem.req_tag;
  
  ctrl.io.dmem.req_rdy    := arb.io.cpu.req_rdy;
  ctrl.io.dmem.resp_miss  := arb.io.cpu.resp_miss;
  ctrl.io.dmem.resp_val   := arb.io.cpu.resp_val;
  
  dpath.io.dmem.resp_val  := arb.io.cpu.resp_val;
  dpath.io.dmem.resp_tag  := arb.io.cpu.resp_tag;
  dpath.io.dmem.resp_data := arb.io.cpu.resp_data;

  // FIXME: console disconnected
//   io.console.bits     := dpath.io.dpath.rs1(7,0);
  io.console.bits     := Bits(0,8);
  io.console.valid    := ctrl.io.console.valid;
  ctrl.io.console.rdy := io.console.rdy;
}

}
