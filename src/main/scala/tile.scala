package rocket

import Chisel._
import Node._
import Constants._

class Tile(co: CoherencePolicyWithUncached, resetSignal: Bool = null) extends Component(resetSignal)
{
  val io = new Bundle {
    val tilelink = new ioTileLink
    val host = new ioHTIF
  }
  
  val cpu       = new rocketProc
  val icache    = new rocketICache(128, 4, co) // 128 sets x 4 ways (32KB)
  val dcache    = new HellaCache(co)

  val arbiter   = new rocketMemArbiter(2 + (if (HAVE_VEC) 1 else 0))
  arbiter.io.requestor(0) <> dcache.io.mem
  arbiter.io.requestor(1) <> icache.io.mem

  io.tilelink.xact_init <> arbiter.io.mem.xact_init
  io.tilelink.xact_init_data <> dcache.io.mem.xact_init_data
  arbiter.io.mem.xact_abort <> io.tilelink.xact_abort
  arbiter.io.mem.xact_rep <> io.tilelink.xact_rep
  io.tilelink.xact_finish <> arbiter.io.mem.xact_finish
  dcache.io.mem.probe_req <> io.tilelink.probe_req
  io.tilelink.probe_rep <> dcache.io.mem.probe_rep
  io.tilelink.probe_rep_data <> dcache.io.mem.probe_rep_data

  if (HAVE_VEC)
  {
    val vicache = new rocketICache(128, 1, co) // 128 sets x 1 ways (8KB)
    arbiter.io.requestor(2) <> vicache.io.mem
    cpu.io.vimem <> vicache.io.cpu
  }

  cpu.io.host       <> io.host

  cpu.io.imem       <> icache.io.cpu
  cpu.io.dmem       <> dcache.io.cpu
}
