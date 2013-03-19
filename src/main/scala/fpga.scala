package referencechip

import Chisel._
import Node._
import uncore._
import rocket._
import rocket.Constants._

class FPGAUncore(htif_width: Int)(implicit conf: CoherenceHubConfiguration) extends Component
{
  val io = new Bundle {
    val host = new HostIO(htif_width)
    val mem = new ioMem
    val tiles = Vec(conf.ln.nTiles) { new TileLinkIO()(conf.ln) }.flip
    val htif = Vec(conf.ln.nTiles) { new HTIFIO(conf.ln.nTiles) }.flip
    val incoherent = Vec(conf.ln.nTiles) { Bool() }.asInput
  }

  val htif = new rocketHTIF(htif_width)
  htif.io.cpu <> io.htif
  io.host <> htif.io.host

  val lnWithHtif = conf.ln.copy(nEndpoints = conf.ln.nEndpoints+1, nTiles = conf.ln.nTiles+1)
  val hub = new CoherenceHubBroadcast()(conf.copy(ln = lnWithHtif))
  for (i <- 0 until conf.ln.nTiles)
    hub.io.tiles(i) <> io.tiles(i)
  hub.io.tiles(conf.ln.nTiles) <> htif.io.mem
  hub.io.incoherent <> io.incoherent

  io.mem.req_cmd <> Queue(hub.io.mem.req_cmd)
  io.mem.req_data <> Queue(hub.io.mem.req_data, REFILL_CYCLES*2)
  hub.io.mem.resp <> Queue(io.mem.resp, REFILL_CYCLES*2)
}

class FPGATop extends Component {
  val htif_width = 16
  val io = new Bundle {
    val debug = new DebugIO
    val host = new HostIO(htif_width)
    val mem = new ioMem
  }
  val co = new MESICoherence
  implicit val lnConf = LogicalNetworkConfiguration(4, 3, 1, 3)
  implicit val uconf = CoherenceHubConfiguration(co, lnConf)
  val uncore = new FPGAUncore(htif_width = htif_width)

  val resetSigs = Vec(uconf.ln.nTiles){ Bool() }
  val ic = ICacheConfig(64, 1, co, ntlb = 4, nbtb = 4)
  val dc = DCacheConfig(64, 1, co, ntlb = 4,
                        nmshr = 2, nrpq = 16, nsdq = 17)
  val rc = RocketConfiguration(uconf.ln, co, ic, dc,
                               fastMulDiv = false,
                               fpu = false, vec = false)
  val tileList = (0 until uconf.ln.nTiles).map(r => new Tile(resetSignal = resetSigs(r))(rc))

  io.debug.error_mode := Bool(false)
  for (i <- 0 until uconf.ln.nTiles) {
    val hl = uncore.io.htif(i)
    val tl = uncore.io.tiles(i)
    val il = uncore.io.incoherent(i)

    resetSigs(i) := hl.reset
    val tile = tileList(i)

    tile.io.host <> hl
    when (tile.io.host.debug.error_mode) { io.debug.error_mode := Bool(true) }

    il := hl.reset
    tl.acquire <> Queue(tile.io.tilelink.acquire)
    tl.acquire_data <> Queue(tile.io.tilelink.acquire_data)
    tile.io.tilelink.grant <> Queue(tl.grant)
    tl.grant_ack <> Queue(tile.io.tilelink.grant_ack)
    tile.io.tilelink.probe <> Queue(tl.probe)
    tl.release <> Queue(tile.io.tilelink.release)
    tl.release_data <> Queue(tile.io.tilelink.release_data)
    //TODO: Set logcal network headers here
  }

  io.host <> uncore.io.host
  io.mem <> uncore.io.mem
}

abstract class AXISlave extends Component {
  val aw = 5
  val dw = 32
  val io = new Bundle {
    val in = new FIFOIO()(Bits(width = dw)).flip
    val out = new FIFOIO()(Bits(width = dw))
    val addr = Bits(INPUT, aw)
  }
}

class Slave extends AXISlave
{
  val top = new FPGATop

  val memw = top.io.mem.resp.bits.data.getWidth
  val htifw = top.io.host.in.bits.getWidth
  
  val n = 4 // htif, mem req/read data, mem write data, error mode
  def wen(i: Int) = io.in.valid && io.addr(log2Up(n)-1,0) === UFix(i)
  def ren(i: Int) = io.out.ready && io.addr(log2Up(n)-1,0) === UFix(i)
  val rdata = Vec(n){Bits(width = dw)}
  val rvalid = Vec(n){Bool()}
  val wready = Vec(n){Bool()}

  io.in.ready := wready(io.addr)
  io.out.valid := rvalid(io.addr)
  io.out.bits := rdata(io.addr)

  // write r0 -> htif.in (blocking)
  wready(0) := top.io.host.in.ready
  top.io.host.in.valid := wen(0)
  top.io.host.in.bits := io.in.bits

  // read cr0 -> htif.out (nonblocking)
  rdata(0) := Cat(top.io.host.out.bits, top.io.host.out.valid)
  rvalid(0) := Bool(true)
  top.io.host.out.ready := ren(0)
  require(dw >= htifw + 1)

  // read cr1 -> mem.req_cmd (nonblocking)
  // the memory system is FIFO from hereon out, so just remember the tags here
  val tagq = new Queue(NGLOBAL_XACTS)(top.io.mem.req_cmd.bits.tag.clone)
  tagq.io.enq.bits := top.io.mem.req_cmd.bits.tag
  tagq.io.enq.valid := ren(1) && top.io.mem.req_cmd.valid && !top.io.mem.req_cmd.bits.rw
  top.io.mem.req_cmd.ready := ren(1)
  rdata(1) := Cat(top.io.mem.req_cmd.bits.addr, top.io.mem.req_cmd.bits.rw, top.io.mem.req_cmd.valid)
  rvalid(1) := Bool(true)
  require(dw >= top.io.mem.req_cmd.bits.addr.getWidth + 1 + 1)

  // write cr1 -> mem.resp (nonblocking)
  val in_count = Reg(resetVal = UFix(0, log2Up(memw/dw)))
  val rf_count = Reg(resetVal = UFix(0, log2Up(REFILL_CYCLES)))
  require(memw % dw == 0 && isPow2(memw/dw))
  val in_reg = Reg{top.io.mem.resp.bits.data.clone}
  top.io.mem.resp.bits.data := Cat(io.in.bits, in_reg(in_reg.getWidth-1,dw))
  top.io.mem.resp.bits.tag := tagq.io.deq.bits
  top.io.mem.resp.valid := wen(1) && in_count.andR
  tagq.io.deq.ready := top.io.mem.resp.fire() && rf_count.andR
  wready(1) := top.io.mem.resp.ready
  when (wen(1) && wready(1)) {
    in_count := in_count + UFix(1)
    in_reg := top.io.mem.resp.bits.data
  }
  when (top.io.mem.resp.fire()) {
    rf_count := rf_count + UFix(1)
  }

  // read cr2 -> mem.req_data (blocking)
  val out_count = Reg(resetVal = UFix(0, log2Up(memw/dw)))
  top.io.mem.req_data.ready := ren(2) && out_count.andR
  rdata(2) := top.io.mem.req_data.bits.data >> (out_count * UFix(dw))
  rvalid(2) := top.io.mem.req_data.valid
  when (ren(2) && rvalid(2)) { out_count := out_count + UFix(1) }

  // read cr3 -> error mode (nonblocking)
  rdata(3) := top.io.debug.error_mode
  rvalid(3) := Bool(true)

  // writes to cr2, cr3 ignored
  wready(2) := Bool(true)
  wready(3) := Bool(true)
}
