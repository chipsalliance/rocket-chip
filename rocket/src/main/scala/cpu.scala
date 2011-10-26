package Top {

import Chisel._
import Node._;
import Constants._;

class ioDebug extends Bundle()
{
  val error_mode  = Bool('output);
  val log_control = Bool('output);
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
  val mem   = new rocketMemory();    
  val wb    = new rocketWriteback(); 

  dpath.io.host             ^^ io.host;
  dpath.io.debug            ^^ io.debug;
  // dpath.io.wb            <> wb.io;
  dpath.io.wb.wen           <> wb.io.wb_wen;
  dpath.io.wb.waddr         <> wb.io.wb_waddr;
  dpath.io.wb.wdata         <> wb.io.wb_wdata;
  dpath.io.imem.req_addr    ^^ io.imem.req_addr;
  dpath.io.imem.resp_data   ^^ io.imem.resp_data;

  ctrl.io.ctrl              <> dpath.io.ctrl;
  ctrl.io.dpath             <> dpath.io.dpath;
  // ctrl.io.mem               <> mem.io;
  ctrl.io.mem.mrq_val       <> mem.io.mem_mrq_val;
  ctrl.io.mem.mrq_cmd       <> mem.io.mem_mrq_cmd;
  ctrl.io.mem.mrq_type      <> mem.io.mem_mrq_type;
  ctrl.io.mem.mrq_deq       <> mem.io.mem_mrq_deq;
  ctrl.io.mem.xsdq_rdy      <> mem.io.mem_xsdq_rdy;
  ctrl.io.mem.xsdq_val      <> mem.io.mem_xsdq_val;
  ctrl.io.mem.dc_busy       := !io.dmem.req_rdy;
  ctrl.io.host.start        ^^ io.host.start;
  ctrl.io.imem              ^^ io.imem;
//   ctrl.io.console           ^^ io.console;
  ctrl.io.wb.waddr          <> wb.io.wb_waddr;
  ctrl.io.wb.wen            <> wb.io.wb_wen;

  // TODO: SHOULD BE THE FOLLOWING BUT NEED BETTER INTERFACE CHUNKS
  // mem.io.dmem               >< io.dmem;

  mem.io.dmem_req_val       ^^ io.dmem.req_val;
  mem.io.dmem_req_rdy       ^^ io.dmem.req_rdy;
  mem.io.dmem_req_op        ^^ io.dmem.req_op;
  mem.io.dmem_req_addr      ^^ io.dmem.req_addr;
  mem.io.dmem_req_data      ^^ io.dmem.req_data;
  mem.io.dmem_req_wmask     ^^ io.dmem.req_wmask;
  mem.io.dmem_req_tag       ^^ io.dmem.req_tag;

  mem.io.dpath_rs2          <> dpath.io.dpath.rs2;
  mem.io.dpath_waddr        <> dpath.io.dpath.waddr;
  mem.io.dpath_alu_out      <> dpath.io.dpath.alu_out;

  wb.io.dmem_resp_val       ^^ io.dmem.resp_val;
  wb.io.dmem_resp_data      ^^ io.dmem.resp_data;
  wb.io.dmem_resp_tag       ^^ io.dmem.resp_tag;

  io.console.bits     := dpath.io.dpath.rs1(7,0);
  io.console.valid    := ctrl.io.console.valid;
  ctrl.io.console.rdy := io.console.rdy;
}

}
