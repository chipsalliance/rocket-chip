package rocket

import Chisel._
import Node._
import Constants._
import uncore._
import Util._

case class RocketConfiguration(lnConf: LogicalNetworkConfiguration, co: CoherencePolicyWithUncached,
                               icache: ICacheConfig, dcache: DCacheConfig,
                               fpu: Boolean, vec: Boolean,
                               fastLoadWord: Boolean = true,
                               fastLoadByte: Boolean = false,
                               fastMulDiv: Boolean = true)
{
  val dcacheReqTagBits = 9 // enforce compliance with require()
  val xprlen = 64
  val nxpr = 32
  val nxprbits = log2Up(nxpr)
  val rvc = false
  if (fastLoadByte) require(fastLoadWord)
}

class Tile(resetSignal: Bool = null)(confIn: RocketConfiguration) extends Component(resetSignal) with ClientCoherenceAgent
{
  val memPorts = 2 + confIn.vec
  implicit val dcConf = confIn.dcache.copy(reqtagbits = confIn.dcacheReqTagBits + log2Up(memPorts), databits = confIn.xprlen)
  implicit val lnConf = confIn.lnConf
  implicit val conf = confIn.copy(dcache = dcConf)

  val io = new Bundle {
    val tilelink = new TileLinkIO
    val host = new HTIFIO(lnConf.nTiles)
  }

  val core      = new Core
  val icache    = new Frontend()(confIn.icache, lnConf)
  val dcache    = new HellaCache

  val arbiter   = new MemArbiter(memPorts)
  arbiter.io.requestor(0) <> dcache.io.mem
  arbiter.io.requestor(1) <> icache.io.mem

  io.tilelink.acquire.valid := arbiter.io.mem.acquire.valid
  io.tilelink.acquire.ready := arbiter.io.mem.acquire.ready
  io.tilelink.acquire.bits := arbiter.io.mem.acquire.bits
  io.tilelink.acquire_data.valid := dcache.io.mem.acquire_data.valid
  io.tilelink.acquire_data.ready := dcache.io.mem.acquire_data.ready
  io.tilelink.acquire_data.bits := dcache.io.mem.acquire_data.bits
  arbiter.io.mem.abort <> io.tilelink.abort
  arbiter.io.mem.grant <> io.tilelink.grant
  io.tilelink.grant_ack.valid := arbiter.io.mem.grant_ack.valid
  io.tilelink.grant_ack.ready := arbiter.io.mem.grant_ack.ready
  io.tilelink.grant_ack.bits := arbiter.io.mem.grant_ack.bits
  dcache.io.mem.probe <> io.tilelink.probe
  io.tilelink.release.valid := dcache.io.mem.release.valid
  io.tilelink.release.ready := dcache.io.mem.release.ready
  io.tilelink.release.bits := dcache.io.mem.release.bits
  io.tilelink.release_data.valid := dcache.io.mem.release_data.valid
  io.tilelink.release_data.ready := dcache.io.mem.release_data.ready
  io.tilelink.release_data.bits := dcache.io.mem.release_data.bits

  val ioSubBundles = io.tilelink.getClass.getMethods.filter( x => 
    classOf[ClientSourcedIO[Data]].isAssignableFrom(x.getReturnType)).map{ m =>
      m.invoke(io.tilelink).asInstanceOf[ClientSourcedIO[LogicalNetworkIO[Data]]] }
  ioSubBundles.foreach{ m => 
    m.bits.header.dst := UFix(0)
    m.bits.header.src := UFix(0)
  }

  if (conf.vec) {
    val vicache = new Frontend()(ICacheConfig(128, 1, conf.co), lnConf) // 128 sets x 1 ways (8KB)
    arbiter.io.requestor(2) <> vicache.io.mem
    core.io.vimem <> vicache.io.cpu
  }

  core.io.host <> io.host
  core.io.imem <> icache.io.cpu
  core.io.dmem <> dcache.io.cpu
}
