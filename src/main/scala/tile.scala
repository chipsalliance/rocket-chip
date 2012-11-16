package rocket

import Chisel._
import Node._
import Constants._
import uncore._

case class RocketConfiguration(ntiles: Int, co: CoherencePolicyWithUncached,
                               icache: ICacheConfig, dcache: DCacheConfig,
                               fastLoadByte: Boolean = false)
{
  val dcacheReqTagBits = 9 // enforce compliance with require()
}

class Tile(resetSignal: Bool = null)(confIn: RocketConfiguration) extends Component(resetSignal)
{
  val memPorts = if (HAVE_VEC) 3 else 2
  implicit val dcConf = confIn.dcache.copy(reqtagbits = confIn.dcacheReqTagBits + log2Up(memPorts))
  implicit val conf = confIn.copy(dcache = dcConf)

  val io = new Bundle {
    val tilelink = new ioTileLink
    val host = new ioHTIF(conf.ntiles)
  }

  val core      = new Core
  val icache    = new Frontend()(confIn.icache)
  val dcache    = new HellaCache

  val arbiter   = new MemArbiter(memPorts)
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

  if (HAVE_VEC) {
    val vicache = new Frontend()(ICacheConfig(128, 1, conf.co)) // 128 sets x 1 ways (8KB)
    arbiter.io.requestor(2) <> vicache.io.mem
    core.io.vimem <> vicache.io.cpu
  }

  core.io.host <> io.host
  core.io.imem <> icache.io.cpu
  core.io.dmem <> dcache.io.cpu
}
