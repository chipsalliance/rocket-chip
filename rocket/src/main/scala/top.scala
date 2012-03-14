package rocket

import Chisel._
import Node._;
import Constants._;

class ioTop(htif_width: Int) extends Bundle  {
  val debug   = new ioDebug();
  val host    = new ioHost(htif_width);
  val mem     = new ioMem
}

class Top() extends Component {

  val htif_width = 16
  val io = new ioTop(htif_width);
  val htif = new rocketHTIF(htif_width, 1)
  
  val cpu       = new rocketProc(resetSignal = htif.io.cpu(0).reset);
  val icache    = new rocketICache(128, 2); // 128 sets x 2 ways
  val icache_pf = new rocketIPrefetcher();
  val dcache    = new HellaCacheUniproc();

  val arbiter   = new rocketMemArbiter(2 + (if (HAVE_VEC) 1 else 0));
  arbiter.io.requestor(0) <> dcache.io.mem
  arbiter.io.requestor(1) <> icache_pf.io.mem

  val hub = new CoherenceHubBroadcast(2)
  // connect tile to hub
  hub.io.tiles(0).xact_init <> Queue(arbiter.io.mem.xact_init)
  hub.io.tiles(0).xact_init_data <> Queue(dcache.io.mem.xact_init_data)
  arbiter.io.mem.xact_abort <> Queue(hub.io.tiles(0).xact_abort)
  arbiter.io.mem.xact_rep <> Pipe(hub.io.tiles(0).xact_rep)
  hub.io.tiles(0).xact_finish <> Queue(arbiter.io.mem.xact_finish)
  dcache.io.mem.probe_req <> Queue(hub.io.tiles(0).probe_req)
  hub.io.tiles(0).probe_rep <> Queue(dcache.io.mem.probe_rep, 1)
  hub.io.tiles(0).probe_rep_data <> Queue(dcache.io.mem.probe_rep_data)
  // connect HTIF to hub
  hub.io.tiles(1) <> htif.io.mem
  // connect hub to memory
  io.mem.req_cmd <> Queue(hub.io.mem.req_cmd)
  io.mem.req_data <> Queue(hub.io.mem.req_data)
  hub.io.mem.resp <> Pipe(io.mem.resp)

  if (HAVE_VEC)
  {
    val vicache = new rocketICache(128, 2); // 128 sets x 2 ways
    arbiter.io.requestor(2) <> vicache.io.mem
    cpu.io.vimem <> vicache.io.cpu;
  }

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
