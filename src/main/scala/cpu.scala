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

  ctrl.io.dpath             <> dpath.io.ctrl;
  ctrl.io.dmem              ^^ io.dmem;
  ctrl.io.host.start        ^^ io.host.start;
  ctrl.io.imem              ^^ io.imem;
  
  dpath.io.dmem             ^^ io.dmem;
  dpath.io.imem.req_addr    ^^ io.imem.req_addr;
  dpath.io.imem.resp_data   ^^ io.imem.resp_data;
  dpath.io.host             ^^ io.host;
  dpath.io.debug            ^^ io.debug;

  // FIXME: console disconnected
//   io.console.bits     := dpath.io.dpath.rs1(7,0);
  io.console.bits     := Bits(0,8);
  io.console.valid    := ctrl.io.console.valid;
  ctrl.io.console.rdy := io.console.rdy;
}

}
