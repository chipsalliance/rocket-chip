package rocket

import Chisel._
import Node._;
import Constants._;

class ioTop(htif_width: Int) extends Bundle  {
  val debug   = new ioDebug();
  val host    = new ioHost(htif_width);
  val mem     = new ioMem();
}

class Top() extends Component {

  val htif_width = 16
  val io = new ioTop(htif_width);
  val htif = new rocketHTIF(htif_width, 1)
  
  val cpu       = new rocketProc(resetSignal = htif.io.cpu(0).reset);
  val icache    = new rocketICache(128, 2); // 128 sets x 2 ways
  val icache_pf = new rocketIPrefetcher();
  val dcache    = new HellaCacheUniproc();

  val arbiter   = new rocketMemArbiter(4);
  arbiter.io.requestor(0) <> dcache.io.mem
  arbiter.io.requestor(1) <> icache_pf.io.mem
  arbiter.io.requestor(3) <> htif.io.mem
  arbiter.io.mem <> io.mem

  if (HAVE_VEC)
  {
    val vicache = new rocketICache(128, 2); // 128 sets x 2 ways
    arbiter.io.requestor(2) <> vicache.io.mem
    cpu.io.vimem <> vicache.io.cpu;
  }
  else
    arbiter.io.requestor(2).req_val := Bool(false)

  htif.io.host <> io.host
  cpu.io.host       <> htif.io.cpu(0);
  cpu.io.debug      <> io.debug;

  icache_pf.io.invalidate := cpu.io.imem.invalidate
  icache.io.mem     <> icache_pf.io.icache;
  cpu.io.imem       <> icache.io.cpu;
  cpu.io.dmem       <> dcache.io.cpu;
  
}

object top_main {
  def main(args: Array[String]) = { 
     chiselMain(args, () => new Top());
  }
}
