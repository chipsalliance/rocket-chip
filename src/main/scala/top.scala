package Top {

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
  val icache    = new rocketICacheDM(128, 32); // lines, address bits
  val icache_pf = new rocketIPrefetcher();
  val dcache    = new rocketDCacheDM_flush(128, 32);
  val arbiter   = new rocketMemArbiter();

  arbiter.io.mem    ^^ io.mem; 
  arbiter.io.dcache <> dcache.io.mem;
  arbiter.io.icache <> icache_pf.io.mem;

  cpu.io.host       ^^ io.host;
  cpu.io.debug      ^^ io.debug;
  cpu.io.console    ^^ io.console;

  icache.io.mem     <> icache_pf.io.icache;
  cpu.io.imem       <> icache.io.cpu;
  cpu.io.dmem       <> dcache.io.cpu;
  
}

object top_main {
  def main(args: Array[String]) = { 
    // Can turn off --debug and --vcd when done with debugging to improve emulator performance
//    val cpu_args = args ++ Array("--target-dir", "generated-src","--debug","--vcd");
    val cpu_args = args ++ Array("--target-dir", "generated-src","--debug");
    // Set variables based off of command flags
//     for(a <- args) {
//         a match {
//               case "-bp" => isBranchPrediction = true;
//               case any =>
//         }
//     }

     chiselMain(cpu_args, () => new Top());
  }
}

}
