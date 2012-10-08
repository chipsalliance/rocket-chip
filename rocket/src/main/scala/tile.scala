package rocket

import Chisel._
import Node._
import Constants._
import uncore._

class Tile(resetSignal: Bool = null)(implicit conf: Configuration) extends Component(resetSignal)
{
  val io = new Bundle {
    val tilelink = new ioTileLink
    val host = new ioHTIF
  }
  
  val cpu       = new rocketProc
  val icache    = new rocketICache(128, 4) // 128 sets x 4 ways (32KB)
  val dcache    = new HellaCache

  val arbiter   = new rocketMemArbiter(DMEM_PORTS)
  arbiter.io.requestor(DMEM_DCACHE) <> dcache.io.mem
  arbiter.io.requestor(DMEM_ICACHE) <> icache.io.mem

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
    val vicache = new rocketICache(128, 1) // 128 sets x 1 ways (8KB)
    arbiter.io.requestor(DMEM_VICACHE) <> vicache.io.mem
    cpu.io.vimem <> vicache.io.cpu
  }

  cpu.io.host       <> io.host

  cpu.io.imem       <> icache.io.cpu
  cpu.io.dmem       <> dcache.io.cpu
}
