package rocket

import Chisel._
import Node._
import Constants._
import uncore._

case class RocketConfiguration(ntiles: Int, co: CoherencePolicyWithUncached,
                               icache: ICacheConfig, dcache: DCacheConfig)
{
  val dcacheReqTagBits = 9 // enforce compliance with require()
}

class Tile(resetSignal: Bool = null)(confIn: RocketConfiguration) extends Component(resetSignal)
{
  implicit val dcConf = confIn.dcache.copy(reqtagbits = confIn.dcacheReqTagBits + log2Up(DMEM_PORTS))
  implicit val conf = confIn.copy(dcache = dcConf)

  val io = new Bundle {
    val tilelink = new ioTileLink
    val host = new ioHTIF(conf.ntiles)
  }

  val cpu       = new rocketProc
  val icache    = new Frontend()(confIn.icache)
  val dcache    = new HellaCache

  val arbiter   = new MemArbiter(DMEM_PORTS)
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
    val vicache = new Frontend()(ICacheConfig(128, 1, conf.co)) // 128 sets x 1 ways (8KB)
    arbiter.io.requestor(DMEM_VICACHE) <> vicache.io.mem
    cpu.io.vimem <> vicache.io.cpu
  }

  cpu.io.host       <> io.host
  cpu.io.imem       <> icache.io.cpu
  cpu.io.dmem       <> dcache.io.cpu
}
