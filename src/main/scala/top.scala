package Top

import Chisel._
import Node._;
import Constants._;

class ioTop extends Bundle  {
  val debug   = new ioDebug();
  val console = new ioConsole();
  val host    = new ioHost();
  val mem     = new ioMem();
}

class Top() extends Component {
  val io        = new ioTop();
  
  val cpu       = new rocketProc();
  val icache    = new rocketICache(128, 2); // 128 sets x 2 ways
  val icache_pf = new rocketIPrefetcher();
  val vicache   = new rocketICache(128, 2); // 128 sets x 2 ways
  val dcache    = new HellaCache();
  val arbiter   = new rocketMemArbiter();

  arbiter.io.mem    <> io.mem; 
  arbiter.io.dcache <> dcache.io.mem;
  arbiter.io.icache <> icache_pf.io.mem;
  arbiter.io.vicache <> vicache.io.mem

  cpu.io.host       <> io.host;
  cpu.io.debug      <> io.debug;
  cpu.io.console    <> io.console;

  icache.io.mem     <> icache_pf.io.icache;
  cpu.io.imem       <> icache.io.cpu;
  cpu.io.vimem      <> vicache.io.cpu;
  cpu.io.dmem       <> dcache.io.cpu;
  
}

object top_main {
  def main(args: Array[String]) = { 
     chiselMain(args, () => new Top());
  }
}
