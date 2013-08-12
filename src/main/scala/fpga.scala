package referencechip

import Chisel._
import Node._
import uncore._
import rocket._

class FPGAOuterMemorySystem(htif_width: Int, clientEndpoints: Seq[ClientCoherenceAgent])(implicit conf: UncoreConfiguration) extends Module
{
  implicit val (tl, ln, l2) = (conf.tl, conf.tl.ln, conf.l2)
  val io = new Bundle {
    val tiles = Vec.fill(conf.nTiles){new TileLinkIO}.flip
    val htif = (new TileLinkIO).flip
    val incoherent = Vec.fill(ln.nClients){Bool()}.asInput
    val mem = new ioMem
  }

  require(clientEndpoints.length == ln.nClients)
  val masterEndpoints = (0 until ln.nMasters).map(i => Module(new L2CoherenceAgent(i)))

  val net = Module(new ReferenceChipCrossbarNetwork(masterEndpoints++clientEndpoints))
  net.io zip (masterEndpoints.map(_.io.client) ++ io.tiles :+ io.htif) map { case (net, end) => net <> end }
  masterEndpoints.map{ _.io.incoherent zip io.incoherent map { case (m, c) => m := c } }

  val conv = Module(new MemIOUncachedTileLinkIOConverter(2))
  if(ln.nMasters > 1) {
    val arb = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(ln.nMasters))
    arb.io.in zip masterEndpoints.map(_.io.master) map { case (arb, cache) => arb <> cache }
    conv.io.uncached <> arb.io.out
  } else {
    conv.io.uncached <> masterEndpoints.head.io.master
  }
  io.mem.req_cmd <> Queue(conv.io.mem.req_cmd)
  io.mem.req_data <> Queue(conv.io.mem.req_data, REFILL_CYCLES)
  conv.io.mem.resp <> Queue(io.mem.resp)
}

class FPGAUncore(htif_width: Int, tileList: Seq[ClientCoherenceAgent])(implicit conf: UncoreConfiguration) extends Module
{
  implicit val (tl, ln) = (conf.tl, conf.tl.ln)
  val io = new Bundle {
    val debug = new DebugIO()
    val host = new HostIO(htif_width)
    val mem = new ioMem
    val tiles = Vec.fill(conf.nTiles){new TileLinkIO}.flip
    val htif = Vec.fill(conf.nTiles){new HTIFIO(conf.nTiles)}.flip
    val incoherent = Vec.fill(conf.nTiles){Bool()}.asInput
  }
  val htif = Module(new RocketHTIF(htif_width))
  val outmemsys = Module(new FPGAOuterMemorySystem(htif_width, tileList :+ htif))
  htif.io.cpu <> io.htif
  outmemsys.io.mem <> io.mem

  // Add networking headers and endpoint queues
  def convertAddrToBank(addr: Bits): UInt = {
    require(conf.bankIdLsb + log2Up(conf.nBanks) < MEM_ADDR_BITS, {println("Invalid bits for bank multiplexing.")})
    addr(conf.bankIdLsb + log2Up(conf.nBanks) - 1, conf.bankIdLsb)
  }

  outmemsys.io.incoherent <> (io.incoherent :+ Bool(true))
  (outmemsys.io.tiles :+ outmemsys.io.htif).zip(io.tiles :+ htif.io.mem).zipWithIndex.map { 
    case ((outer, client), i) => 
      outer.acquire <> TileLinkHeaderAppender(client.acquire, i, conf.nBanks, convertAddrToBank _)
      outer.release <> TileLinkHeaderAppender(client.release, i, conf.nBanks, convertAddrToBank _)

      val grant_ack_q = Queue(client.grant_ack)
      outer.grant_ack.valid := grant_ack_q.valid
      outer.grant_ack.bits := grant_ack_q.bits
      outer.grant_ack.bits.header.src := UInt(i)
      grant_ack_q.ready := outer.grant_ack.ready

      client.grant <> Queue(outer.grant, 1, pipe = true)
      client.probe <> Queue(outer.probe)
  }

  htif.io.host.out <> io.host.out
  htif.io.host.in <> io.host.in
}

class FPGATopIO(htifWidth: Int) extends TopIO(htifWidth)

class FPGATop extends Module {
  val htif_width = 16
  val co = new MESICoherence
  val ntiles = 1
  val nbanks = 1
  val nmshrs = 2
  implicit val ln = LogicalNetworkConfiguration(ntiles+nbanks+1, log2Up(ntiles)+1, nbanks, ntiles+1)
  implicit val tl = TileLinkConfiguration(co, ln, log2Up(1+8), 2*log2Up(nmshrs*ntiles+1), MEM_DATA_BITS)
  implicit val l2 = L2CoherenceAgentConfiguration(tl, 1, 8)
  implicit val uc = UncoreConfiguration(l2, tl, ntiles, nbanks, bankIdLsb = 5)

  val ic = ICacheConfig(64, 1, ntlb = 4, nbtb = 4)
  val dc = DCacheConfig(64, 1, ntlb = 4, nmshr = 2, nrpq = 16, nsdq = 17, states = co.nClientStates)
  val rc = RocketConfiguration(tl, ic, dc,
                               fastMulDiv = false,
                               fpu = false, vec = false)

  val io = new FPGATopIO(htif_width)

  val resetSigs = Vec.fill(uc.nTiles){Bool()}
  val tileList = (0 until uc.nTiles).map(r => Module(new Tile(resetSignal = resetSigs(r))(rc)))
  val uncore = Module(new FPGAUncore(htif_width, tileList))

  io.debug.error_mode := Bool(false)
  for (i <- 0 until uc.nTiles) {
    val hl = uncore.io.htif(i)
    val tl = uncore.io.tiles(i)
    val il = uncore.io.incoherent(i)

    resetSigs(i) := hl.reset
    val tile = tileList(i)

    tile.io.tilelink <> tl
    il := hl.reset
    tile.io.host.reset := RegUpdate(RegUpdate(hl.reset))
    tile.io.host.pcr_req <> Queue(hl.pcr_req)
    hl.pcr_rep <> Queue(tile.io.host.pcr_rep)
    hl.ipi_req <> Queue(tile.io.host.ipi_req)
    tile.io.host.ipi_rep <> Queue(hl.ipi_rep)

    when (tile.io.host.debug.error_mode) { io.debug.error_mode := Bool(true) }
  }

  io.host <> uncore.io.host
  io.mem <> uncore.io.mem
}

abstract class AXISlave extends Module {
  val aw = 5
  val dw = 32
  val io = new Bundle {
    val in = Decoupled(Bits(width = dw)).flip
    val out = Decoupled(Bits(width = dw))
    val addr = Bits(INPUT, aw)
  }
}

class Slave extends AXISlave
{
  val top = new FPGATop

  val memw = top.io.mem.resp.bits.data.getWidth
  val htifw = top.io.host.in.bits.getWidth
  
  val n = 4 // htif, mem req/read data, mem write data, error mode
  def wen(i: Int) = io.in.valid && io.addr(log2Up(n)-1,0) === UInt(i)
  def ren(i: Int) = io.out.ready && io.addr(log2Up(n)-1,0) === UInt(i)
  val rdata = Vec.fill(n){Bits(width = dw)}
  val rvalid = Vec.fill(n){Bool()}
  val wready = Vec.fill(n){Bool()}

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
  val tagq = Module(new Queue(top.io.mem.req_cmd.bits.tag, 4))
  tagq.io.enq.bits := top.io.mem.req_cmd.bits.tag
  tagq.io.enq.valid := ren(1) && top.io.mem.req_cmd.valid && !top.io.mem.req_cmd.bits.rw
  top.io.mem.req_cmd.ready := ren(1)
  rdata(1) := Cat(top.io.mem.req_cmd.bits.addr, top.io.mem.req_cmd.bits.rw, top.io.mem.req_cmd.valid && (tagq.io.enq.ready || top.io.mem.req_cmd.bits.rw))
  rvalid(1) := Bool(true)
  require(dw >= top.io.mem.req_cmd.bits.addr.getWidth + 1 + 1)

  // write cr1 -> mem.resp (nonblocking)
  val in_count = RegReset(UInt(0, log2Up(memw/dw)))
  val rf_count = RegReset(UInt(0, log2Up(REFILL_CYCLES)))
  require(memw % dw == 0 && isPow2(memw/dw))
  val in_reg = Reg(top.io.mem.resp.bits.data)
  top.io.mem.resp.bits.data := Cat(io.in.bits, in_reg(in_reg.getWidth-1,dw))
  top.io.mem.resp.bits.tag := tagq.io.deq.bits
  top.io.mem.resp.valid := wen(1) && in_count.andR
  tagq.io.deq.ready := top.io.mem.resp.fire() && rf_count.andR
  wready(1) := top.io.mem.resp.ready
  when (wen(1) && wready(1)) {
    in_count := in_count + UInt(1)
    in_reg := top.io.mem.resp.bits.data
  }
  when (top.io.mem.resp.fire()) {
    rf_count := rf_count + UInt(1)
  }

  // read cr2 -> mem.req_data (blocking)
  val out_count = RegReset(UInt(0, log2Up(memw/dw)))
  top.io.mem.req_data.ready := ren(2) && out_count.andR
  rdata(2) := top.io.mem.req_data.bits.data >> (out_count * UInt(dw))
  rvalid(2) := top.io.mem.req_data.valid
  when (ren(2) && rvalid(2)) { out_count := out_count + UInt(1) }

  // read cr3 -> error mode (nonblocking)
  rdata(3) := Cat(top.io.mem.req_cmd.valid, tagq.io.enq.ready, top.io.debug.error_mode)
  rvalid(3) := Bool(true)

  // writes to cr2, cr3 ignored
  wready(2) := Bool(true)
  wready(3) := Bool(true)
}
