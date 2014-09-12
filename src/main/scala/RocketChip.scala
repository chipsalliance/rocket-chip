// See LICENSE for license details.

package rocketchip

import Chisel._
import uncore._
import rocket._
import rocket.Util._

case object NTiles extends Field[Int]
case object NBanks extends Field[Int]
case object BankIdLSB extends Field[Int]
case object CacheBlockBytes extends Field[Int]
case object CacheBlockOffsetBits extends Field[Int]
case object BuildDRAMSideLLC extends Field[(Int) => DRAMSideLLCLike]
case object BuildCoherenceMaster extends Field[(Int) => CoherenceAgent]
case object UseBackupMemoryPort extends Field[Boolean]
case object Coherence extends Field[CoherencePolicyWithUncached]

abstract trait TopLevelParameters extends UsesParameters {
  val htifW = params(HTIFWidth)
  val nTiles = params(NTiles)
  val nBanks = params(NBanks)
  val lsb = params(BankIdLSB)
  val refillCycles = params(MIFDataBeats)
}
class OuterMemorySystem extends Module with TopLevelParameters {
  val io = new Bundle {
    val tiles = Vec.fill(params(NTiles)){new TileLinkIO}.flip
    val htif = (new TileLinkIO).flip
    val incoherent = Vec.fill(params(LNClients)){Bool()}.asInput
    val mem = new MemIO
    val mem_backup = new MemSerializedIO(params(HTIFWidth))
    val mem_backup_en = Bool(INPUT)
  }

  // Create a simple NoC and points of coherence serialization
  val net = Module(new RocketChipCrossbarNetwork)
  val masterEndpoints = (0 until params(NBanks)).map(params(BuildCoherenceMaster))
  net.io.clients zip (io.tiles :+ io.htif) map { case (net, end) => net <> end }
  net.io.masters zip (masterEndpoints.map(_.io.inner)) map { case (net, end) => net <> end }
  masterEndpoints.map{ _.io.incoherent zip io.incoherent map { case (m, c) => m := c } }

  // Create a converter between TileLinkIO and MemIO
  val conv = Module(new MemIOUncachedTileLinkIOConverter(2))
  if(params(NBanks) > 1) {
    val arb = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(params(NBanks)))
    arb.io.in zip masterEndpoints.map(_.io.outer) map { case (arb, cache) => arb <> cache }
    conv.io.uncached <> arb.io.out
  } else {
    conv.io.uncached <> masterEndpoints.head.io.outer
  }

  // Create a DRAM-side LLC
  val llc = params(BuildDRAMSideLLC)(refillCycles)
  llc.io.cpu.req_cmd <> Queue(conv.io.mem.req_cmd, 2)
  llc.io.cpu.req_data <> Queue(conv.io.mem.req_data, refillCycles)
  conv.io.mem.resp <> llc.io.cpu.resp
  
  // Create a SerDes for backup memory port
  if(params(UseBackupMemoryPort)) {
    VLSIUtils.doOuterMemorySystemSerdes(llc.io.mem, io.mem, io.mem_backup,
                                        io.mem_backup_en, htifW)
  } else {
    io.mem <> llc.io.mem 
  }
}

class Uncore extends Module with TopLevelParameters {
  val io = new Bundle {
    val host = new HostIO
    val mem = new MemIO
    val tiles = Vec.fill(nTiles){new TileLinkIO}.flip
    val htif = Vec.fill(nTiles){new HTIFIO}.flip
    val incoherent = Vec.fill(nTiles){Bool()}.asInput
    val mem_backup = new MemSerializedIO(htifW)
    val mem_backup_en = Bool(INPUT)
  }

  // Used to hash physical addresses to banks
  require(params(BankIdLSB) + log2Up(params(NBanks)) < params(MIFAddrBits))
  def addrToBank(addr: Bits): UInt = {
    if(nBanks > 1) addr( lsb + log2Up(nBanks) - 1, lsb)
    else UInt(0)
  }

  val htif = Module(new HTIF(CSRs.reset)) // One HTIF module per chip
  val outmemsys = Module(new OuterMemorySystem) // NoC, LLC and SerDes

  // Wire outer mem system to tiles and htif, adding
  //   networking headers and endpoint queues
  (outmemsys.io.tiles :+ outmemsys.io.htif) // Collect outward-facing TileLink ports
    .zip(io.tiles :+ htif.io.mem)           // Zip them with matching ports from clients
    .zipWithIndex                           // Index them
    .map { case ((outer, client), i) =>     // Then use the index and bank hash to
                                            //   overwrite the networking header
      outer.acquire <> Queue(TileLinkHeaderOverwriter(client.acquire, i, nBanks, addrToBank _))
      outer.release <> Queue(TileLinkHeaderOverwriter(client.release, i, nBanks, addrToBank _))
      outer.finish <> Queue(TileLinkHeaderOverwriter(client.finish, i, true))
      client.grant <> Queue(outer.grant, 1, pipe = true)
      client.probe <> Queue(outer.probe)
    } 
  outmemsys.io.incoherent := (io.incoherent :+ Bool(true).asInput)

  // Wire the htif to the memory port(s) and host interface
  io.host.debug_stats_pcr := htif.io.host.debug_stats_pcr
  htif.io.cpu <> io.htif
  outmemsys.io.mem <> io.mem
  if(params(UseBackupMemoryPort)) {
    outmemsys.io.mem_backup_en := io.mem_backup_en
    VLSIUtils.padOutHTIFWithDividedClock(htif.io, outmemsys.io.mem_backup, 
      io.mem_backup, io.host, io.mem_backup_en, htifW)
  } else {
    htif.io.host.out <> io.host.out
    htif.io.host.in <> io.host.in
  }
}

class TopIO extends Bundle {
  val host    = new HostIO
  val mem     = new MemIO
  val mem_backup_en = Bool(INPUT)
  val in_mem_ready = Bool(OUTPUT)
  val in_mem_valid = Bool(INPUT)
  val out_mem_ready = Bool(INPUT)
  val out_mem_valid = Bool(OUTPUT)
}

class Top extends Module with TopLevelParameters {
  val io = new TopIO

  val resetSigs = Vec.fill(nTiles){Bool()}
  val tileList = (0 until nTiles).map(r => Module(new Tile(resetSignal = resetSigs(r))))
  val uncore = Module(new Uncore)

  for (i <- 0 until nTiles) {
    val hl = uncore.io.htif(i)
    val tl = uncore.io.tiles(i)
    val il = uncore.io.incoherent(i)

    resetSigs(i) := hl.reset
    val tile = tileList(i)

    tile.io.tilelink <> tl
    il := hl.reset
    tile.io.host.id := UInt(i)
    tile.io.host.reset := Reg(next=Reg(next=hl.reset))
    tile.io.host.pcr_req <> Queue(hl.pcr_req)
    hl.pcr_rep <> Queue(tile.io.host.pcr_rep)
    hl.ipi_req <> Queue(tile.io.host.ipi_req)
    tile.io.host.ipi_rep <> Queue(hl.ipi_rep)
    hl.debug_stats_pcr := tile.io.host.debug_stats_pcr
  }

  io.host <> uncore.io.host
  io.mem <> uncore.io.mem

  if(params(UseBackupMemoryPort)) {
    uncore.io.mem_backup.resp.valid := io.in_mem_valid
    io.out_mem_valid := uncore.io.mem_backup.req.valid
    uncore.io.mem_backup.req.ready := io.out_mem_ready
    io.mem_backup_en <> uncore.io.mem_backup_en
  }
}
