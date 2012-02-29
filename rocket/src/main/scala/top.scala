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

  val arbiter   = new rocketMemArbiter(3 + (if (HAVE_VEC) 1 else 0));
  arbiter.io.requestor(0) <> dcache.io.mem
  arbiter.io.requestor(1) <> icache_pf.io.mem
  arbiter.io.requestor(2) <> htif.io.mem

  val hub = new CoherenceHubNull
  // connect tile to hub (figure out how to do this more compactly)
  val xact_init_q = (new queue(2)) { new TransactionInit }
  xact_init_q.io.enq <> arbiter.io.mem.xact_init
  xact_init_q.io.deq <> hub.io.tile.xact_init
  val xact_init_data_q = (new queue(2)) { new TransactionInitData }
  xact_init_data_q.io.enq <> arbiter.io.mem.xact_init_data
  xact_init_data_q.io.deq <> hub.io.tile.xact_init_data
  val xact_rep_q = (new queue(1, pipe = true)) { new TransactionReply }
  xact_rep_q.io.enq <> hub.io.tile.xact_rep
  xact_rep_q.io.deq <> arbiter.io.mem.xact_rep
  // connect hub to memory
  val mem_req_q = (new queue(2)) { new MemReqCmd }
  mem_req_q.io.enq <> hub.io.mem.req_cmd
  mem_req_q.io.deq <> io.mem.req_cmd
  val mem_req_data_q = (new queue(2)) { new MemData }
  mem_req_data_q.io.enq <> hub.io.mem.req_data
  mem_req_data_q.io.deq <> io.mem.req_data
  hub.io.mem.resp.valid := Reg(io.mem.resp.valid, resetVal = Bool(false))
  hub.io.mem.resp.bits := Reg(io.mem.resp.bits)


  if (HAVE_VEC)
  {
    val vicache = new rocketICache(128, 2); // 128 sets x 2 ways
    arbiter.io.requestor(3) <> vicache.io.mem
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
