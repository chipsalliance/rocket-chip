package referencechip

import Chisel._
import Node._
import uncore._
import rocket._
import rocket.Constants._

class FPGAOuterMemorySystem(htif_width: Int, clientEndpoints: Seq[ClientCoherenceAgent])(implicit conf: UncoreConfiguration) extends Component
{
  implicit val lnconf = conf.ln
  val io = new Bundle {
    val tiles = Vec(conf.ln.nClients) { new TileLinkIO }.flip
    val htif = (new TileLinkIO).flip
    val incoherent = Vec(conf.ln.nClients) { Bool() }.asInput
    val mem = new ioMem
  }

  import rocket.Constants._

  val lnWithHtifConf = conf.ln.copy(nEndpoints = conf.ln.nEndpoints+1, 
                                    idBits = log2Up(conf.ln.nEndpoints+1)+1,
                                    nClients = conf.ln.nClients+1)
  val ucWithHtifConf = conf.copy(ln = lnWithHtifConf)
  require(clientEndpoints.length == lnWithHtifConf.nClients)
  val masterEndpoints = (0 until lnWithHtifConf.nMasters).map(new L2CoherenceAgent(_)(ucWithHtifConf))

  val net = new ReferenceChipCrossbarNetwork(masterEndpoints++clientEndpoints)(lnWithHtifConf)
  net.io zip (masterEndpoints.map(_.io.client) ++ io.tiles :+ io.htif) map { case (net, end) => net <> end }
  masterEndpoints.map{ _.io.incoherent zip (io.incoherent ++ List(Bool(true))) map { case (m, c) => m := c } }

  val conv = new MemIOUncachedTileLinkIOConverter(2)(ucWithHtifConf)
  if(lnWithHtifConf.nMasters > 1) {
    val arb = new UncachedTileLinkIOArbiter(lnWithHtifConf.nMasters)(lnWithHtifConf)
    arb.io.in zip masterEndpoints.map(_.io.master) map { case (arb, cache) => arb <> cache }
    conv.io.uncached <> arb.io.out
  } else {
    conv.io.uncached <> masterEndpoints.head.io.master
  }
  io.mem.req_cmd <> Queue(conv.io.mem.req_cmd)
  io.mem.req_data <> Queue(conv.io.mem.req_data, REFILL_CYCLES)
  conv.io.mem.resp <> Queue(io.mem.resp)
}

class FPGAUncore(htif_width: Int, tileList: Seq[ClientCoherenceAgent])(implicit conf: UncoreConfiguration) extends Component
{
  implicit val lnconf = conf.ln
  val io = new Bundle {
    val debug = new DebugIO()
    val host = new HostIO(htif_width)
    val mem = new ioMem
    val tiles = Vec(conf.ln.nClients) { new TileLinkIO }.flip
    val htif = Vec(conf.ln.nClients) { new HTIFIO(conf.ln.nClients) }.flip
    val incoherent = Vec(conf.ln.nClients) { Bool() }.asInput
  }
  val nBanks = 1
  val bankIdLsb = 5

  val htif = new rocketHTIF(htif_width)
  val outmemsys = new FPGAOuterMemorySystem(htif_width, tileList :+ htif)
  htif.io.cpu <> io.htif
  outmemsys.io.incoherent <> io.incoherent
  io.mem <> outmemsys.io.mem

  // Add networking headers and endpoint queues
  (outmemsys.io.tiles :+ outmemsys.io.htif).zip(io.tiles :+ htif.io.mem).zipWithIndex.map { 
    case ((outer, client), i) => 
      val (acq_w_header, acq_data_w_header) = TileLinkHeaderAppender(client.acquire, client.acquire_data, i, nBanks, bankIdLsb)
      outer.acquire <> acq_w_header
      outer.acquire_data <> acq_data_w_header

      val (rel_w_header, rel_data_w_header) = TileLinkHeaderAppender(client.release, client.release_data, i, nBanks, bankIdLsb)
      outer.release <> rel_w_header
      outer.release_data <> rel_data_w_header

      val grant_ack_q = Queue(client.grant_ack)
      outer.grant_ack.valid := grant_ack_q.valid
      outer.grant_ack.bits := grant_ack_q.bits
      outer.grant_ack.bits.header.src := UFix(i)
      grant_ack_q.ready := outer.grant_ack.ready

      client.grant <> Queue(outer.grant, 1, pipe = true)
      client.probe <> Queue(outer.probe)
  }

  htif.io.host.out <> io.host.out
  htif.io.host.in <> io.host.in
}

class FPGATop extends Component {
  val htif_width = 16
  val io = new Bundle {
    val debug = new DebugIO
    val host = new HostIO(htif_width)
    val mem = new ioMem
  }
  val co = new MESICoherence
  val ntiles = 1
  val nbanks = 1
  implicit val lnConf = LogicalNetworkConfiguration(ntiles+nbanks, log2Up(ntiles)+1, nbanks, ntiles)
  implicit val uconf = UncoreConfiguration(co, lnConf)

  val resetSigs = Vec(uconf.ln.nClients){ Bool() }
  val ic = ICacheConfig(64, 1, co, ntlb = 4, nbtb = 4)
  val dc = DCacheConfig(64, 1, co, ntlb = 4,
                        nmshr = 2, nrpq = 16, nsdq = 17)
  val rc = RocketConfiguration(uconf.ln, co, ic, dc,
                               fastMulDiv = false,
                               fpu = false, vec = false)
  val tileList = (0 until uconf.ln.nClients).map(r => new Tile(resetSignal = resetSigs(r))(rc))
  val uncore = new FPGAUncore(htif_width = htif_width, tileList = tileList)

  io.debug.error_mode := Bool(false)
  for (i <- 0 until uconf.ln.nClients) {
    val hl = uncore.io.htif(i)
    val tl = uncore.io.tiles(i)
    val il = uncore.io.incoherent(i)

    resetSigs(i) := hl.reset
    val tile = tileList(i)

    tile.io.tilelink <> tl
    il := hl.reset
    tile.io.host.reset := Reg(Reg(hl.reset))
    tile.io.host.pcr_req <> Queue(hl.pcr_req)
    hl.pcr_rep <> Queue(tile.io.host.pcr_rep)
    hl.ipi_req <> Queue(tile.io.host.ipi_req)
    tile.io.host.ipi_rep <> Queue(hl.ipi_rep)

    when (tile.io.host.debug.error_mode) { io.debug.error_mode := Bool(true) }
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
  val tagq = new Queue(4)(top.io.mem.req_cmd.bits.tag.clone)
  tagq.io.enq.bits := top.io.mem.req_cmd.bits.tag
  tagq.io.enq.valid := ren(1) && top.io.mem.req_cmd.valid && !top.io.mem.req_cmd.bits.rw
  top.io.mem.req_cmd.ready := ren(1)
  rdata(1) := Cat(top.io.mem.req_cmd.bits.addr, top.io.mem.req_cmd.bits.rw, top.io.mem.req_cmd.valid && (tagq.io.enq.ready || top.io.mem.req_cmd.bits.rw))
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
  rdata(3) := Cat(top.io.mem.req_cmd.valid, tagq.io.enq.ready, top.io.debug.error_mode)
  rvalid(3) := Bool(true)

  // writes to cr2, cr3 ignored
  wready(2) := Bool(true)
  wready(3) := Bool(true)
}
