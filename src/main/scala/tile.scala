package rocket

import Chisel._
import uncore._
import Util._

case class RocketConfiguration(tl: TileLinkConfiguration,
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

class Tile(resetSignal: Bool = null)(confIn: RocketConfiguration) extends Module(reset = resetSignal) with ClientCoherenceAgent
{
  val memPorts = 2 + confIn.vec
  val dcachePortId = 0
  val icachePortId = 1
  val vicachePortId = 2
  implicit val tlConf = confIn.tl
  implicit val lnConf = confIn.tl.ln
  implicit val icConf = confIn.icache
  implicit val dcConf = confIn.dcache.copy(reqtagbits = confIn.dcacheReqTagBits + log2Up(memPorts), databits = confIn.xprlen)
  implicit val conf = confIn.copy(dcache = dcConf)

  val io = new Bundle {
    val tilelink = new TileLinkIO
    val host = new HTIFIO(lnConf.nClients)
  }

  val core = Module(new Core)
  val icache = Module(new Frontend)
  val dcache = Module(new HellaCache)

  val arbiter = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(memPorts))
  arbiter.io.in(dcachePortId) <> dcache.io.mem
  arbiter.io.in(icachePortId) <> icache.io.mem

  io.tilelink.acquire <> arbiter.io.out.acquire
  arbiter.io.out.grant <> io.tilelink.grant
  io.tilelink.grant_ack <> arbiter.io.out.grant_ack
  dcache.io.mem.probe <> io.tilelink.probe
  io.tilelink.release.data <> dcache.io.mem.release.data
  io.tilelink.release.meta.valid   := dcache.io.mem.release.meta.valid
  dcache.io.mem.release.meta.ready := io.tilelink.release.meta.ready
  io.tilelink.release.meta.bits := dcache.io.mem.release.meta.bits
  io.tilelink.release.meta.bits.payload.client_xact_id :=  Cat(dcache.io.mem.release.meta.bits.payload.client_xact_id, UInt(dcachePortId, log2Up(memPorts))) // Mimic client id extension done by UncachedTileLinkIOArbiter for Acquires from either client)

  /*val ioSubBundles = io.tilelink.getClass.getMethods.filter( x => 
    classOf[ClientSourcedIO[Data]].isAssignableFrom(x.getReturnType)).map{ m =>
      m.invoke(io.tilelink).asInstanceOf[ClientSourcedIO[LogicalNetworkIO[Data]]] }
  ioSubBundles.foreach{ m => 
    m.bits.header.dst := UInt(0)
    m.bits.header.src := UInt(0)
  }*/

  if (conf.vec) {
    val vicache = Module(new Frontend()(ICacheConfig(128, 1), tlConf)) // 128 sets x 1 ways (8KB)
    arbiter.io.in(vicachePortId) <> vicache.io.mem
    core.io.vimem <> vicache.io.cpu
  }

  core.io.host <> io.host
  core.io.imem <> icache.io.cpu
  core.io.dmem <> dcache.io.cpu
}
