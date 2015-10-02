// See LICENSE for license details.

package rocketchip

import Chisel._
import junctions._
import uncore._
import rocket._
import rocket.Util._

/** Top-level parameters of RocketChip, values set in e.g. PublicConfigs.scala */

/** Number of tiles */
case object NTiles extends Field[Int]
/** Number of memory channels */
case object NMemoryChannels extends Field[Int]
/** Number of banks per memory channel */
case object NBanksPerMemoryChannel extends Field[Int]
/** Least significant bit of address used for bank partitioning */
case object BankIdLSB extends Field[Int]
/** Number of outstanding memory requests */
case object NOutstandingMemReqsPerChannel extends Field[Int]
/** Whether to use the slow backup memory port [VLSI] */
case object UseBackupMemoryPort extends Field[Boolean]
/** Function for building some kind of coherence manager agent */
case object BuildL2CoherenceManager extends Field[() => CoherenceAgent]
/** Function for building some kind of tile connected to a reset signal */
case object BuildTiles extends Field[Seq[(Bool) => Tile]]
/** Start address of the "io" region in the memory map */
case object ExternalIOStart extends Field[BigInt]

/** Utility trait for quick access to some relevant parameters */
trait TopLevelParameters extends UsesParameters {
  implicit val p: Parameters
  lazy val htifW = p(HTIFWidth)
  lazy val nTiles = p(NTiles)
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val nBanksPerMemChannel = p(NBanksPerMemoryChannel)
  lazy val nBanks = nMemChannels*nBanksPerMemChannel
  lazy val lsb = p(BankIdLSB)
  lazy val nMemReqs = p(NOutstandingMemReqsPerChannel)
  lazy val mifAddrBits = p(MIFAddrBits)
  lazy val mifDataBeats = p(MIFDataBeats)
  lazy val scrAddrBits = log2Up(p(HTIFNSCR))
  lazy val pcrAddrBits = 12
  lazy val xLen = p(XLen)
  //require(lsb + log2Up(nBanks) < mifAddrBits)
}

class MemBackupCtrlIO extends Bundle {
  val en = Bool(INPUT)
  val in_valid = Bool(INPUT)
  val out_ready = Bool(INPUT)
  val out_valid = Bool(OUTPUT)
}

/** Top-level io for the chip */
class BasicTopIO extends Bundle {
  val host = new HostIO
  val mem_backup_ctrl = new MemBackupCtrlIO
}

class TopIO(implicit val p: Parameters) extends BasicTopIO {
  val mem = new MemIO
}

class MultiChannelTopIO(implicit val p: Parameters) extends BasicTopIO with TopLevelParameters {
  val mem = Vec(new NastiIO, nMemChannels)
  val mmio = new NastiIO
}

/** Top-level module for the chip */
//TODO: Remove this wrapper once multichannel DRAM controller is provided
class Top extends Module with TopLevelParameters {
  implicit val p = params
  val io = new TopIO
  if(!p(UseZscale)) {
    val temp = Module(new MultiChannelTop)
    val arb = Module(new NastiArbiter(nMemChannels))
    val conv = Module(new MemIONastiIOConverter(p(CacheBlockOffsetBits)))
    arb.io.master <> temp.io.mem
    conv.io.nasti <> arb.io.slave
    io.mem.req_cmd <> Queue(conv.io.mem.req_cmd)
    io.mem.req_data <> Queue(conv.io.mem.req_data, mifDataBeats)
    conv.io.mem.resp <> Queue(io.mem.resp, mifDataBeats)
    io.mem_backup_ctrl <> temp.io.mem_backup_ctrl
    io.host <> temp.io.host

    // tie off the mmio port
    val errslave = Module(new NastiErrorSlave)
    errslave.io <> temp.io.mmio
  } else {
    val temp = Module(new ZscaleTop)
    io.host <> temp.io.host
  }
}

class MultiChannelTop(implicit val p: Parameters) extends Module with TopLevelParameters {
  val io = new MultiChannelTopIO

  // Build an Uncore and a set of Tiles
  val innerTLParams = p.alterPartial({case TLId => "L1ToL2" })
  val uncore = Module(new Uncore()(innerTLParams))(innerTLParams)
  val tileList = uncore.io.htif zip p(BuildTiles) map { case(hl, bt) => bt(hl.reset) }

  // Connect each tile to the HTIF
  uncore.io.htif.zip(tileList).zipWithIndex.foreach {
    case ((hl, tile), i) =>
      tile.io.host.id := UInt(i)
      tile.io.host.reset := Reg(next=Reg(next=hl.reset))
      tile.io.host.pcr.req <> Queue(hl.pcr.req)
      hl.pcr.resp <> Queue(tile.io.host.pcr.resp)
      hl.ipi_req <> Queue(tile.io.host.ipi_req)
      tile.io.host.ipi_rep <> Queue(hl.ipi_rep)
      hl.debug_stats_pcr := tile.io.host.debug_stats_pcr
  }

  // Connect the uncore to the tile memory ports, HostIO and MemIO
  uncore.io.tiles_cached <> tileList.map(_.io.cached)
  uncore.io.tiles_uncached <> tileList.map(_.io.uncached)
  io.host <> uncore.io.host
  io.mem <> uncore.io.mem
  io.mmio <> uncore.io.mmio
  if(p(UseBackupMemoryPort)) { io.mem_backup_ctrl <> uncore.io.mem_backup_ctrl }
}

/** Wrapper around everything that isn't a Tile.
  *
  * Usually this is clocked and/or place-and-routed separately from the Tiles.
  * Contains the Host-Target InterFace module (HTIF).
  */
class Uncore(implicit val p: Parameters) extends Module with TopLevelParameters {
  val io = new Bundle {
    val host = new HostIO
    val mem = Vec(new NastiIO, nMemChannels)
    val tiles_cached = Vec(new ClientTileLinkIO, nTiles).flip
    val tiles_uncached = Vec(new ClientUncachedTileLinkIO, nTiles).flip
    val htif = Vec(new HTIFIO, nTiles).flip
    val mem_backup_ctrl = new MemBackupCtrlIO
    val mmio = new NastiIO
  }

  val htif = Module(new HTIF(CSRs.mreset)) // One HTIF module per chip
  val outmemsys = Module(new OuterMemorySystem) // NoC, LLC and SerDes
  outmemsys.io.incoherent := htif.io.cpu.map(_.reset)
  outmemsys.io.htif_uncached <> htif.io.mem
  outmemsys.io.tiles_uncached <> io.tiles_uncached
  outmemsys.io.tiles_cached <> io.tiles_cached

  for (i <- 0 until nTiles) {
    io.htif(i).reset := htif.io.cpu(i).reset
    io.htif(i).id := htif.io.cpu(i).id
    htif.io.cpu(i).ipi_req <> io.htif(i).ipi_req
    io.htif(i).ipi_rep <> htif.io.cpu(i).ipi_rep
    htif.io.cpu(i).debug_stats_pcr <> io.htif(i).debug_stats_pcr

    val pcr_arb = Module(new SMIArbiter(2, 64, 12))
    pcr_arb.io.in(0) <> htif.io.cpu(i).pcr
    pcr_arb.io.in(1) <> outmemsys.io.pcr(i)
    io.htif(i).pcr <> pcr_arb.io.out
  }

  // Arbitrate SCR access between MMIO and HTIF
  val scrArb = Module(new SMIArbiter(2, 64, scrAddrBits))
  val scrFile = Module(new SCRFile)

  scrArb.io.in(0) <> htif.io.scr
  scrArb.io.in(1) <> outmemsys.io.scr
  scrFile.io.smi <> scrArb.io.out
  // scrFile.io.scr <> (... your SCR connections ...)

  // Wire the htif to the memory port(s) and host interface
  io.host.debug_stats_pcr := htif.io.host.debug_stats_pcr
  io.mem <> outmemsys.io.mem
  io.mmio <> outmemsys.io.mmio
  if(p(UseBackupMemoryPort)) {
    outmemsys.io.mem_backup_en := io.mem_backup_ctrl.en
    VLSIUtils.padOutHTIFWithDividedClock(htif.io.host, scrFile.io.scr,
      outmemsys.io.mem_backup, io.mem_backup_ctrl, io.host, htifW)
  } else {
    htif.io.host.out <> io.host.out
    htif.io.host.in <> io.host.in
  }
}

/** The whole outer memory hierarchy, including a NoC, some kind of coherence
  * manager agent, and a converter from TileLink to MemIO.
  */ 
class OuterMemorySystem(implicit val p: Parameters) extends Module with TopLevelParameters {
  val io = new Bundle {
    val tiles_cached = Vec(new ClientTileLinkIO, nTiles).flip
    val tiles_uncached = Vec(new ClientUncachedTileLinkIO, nTiles).flip
    val htif_uncached = (new ClientUncachedTileLinkIO).flip
    val incoherent = Vec(Bool(), nTiles).asInput
    val mem = Vec(new NastiIO, nMemChannels)
    val mem_backup = new MemSerializedIO(htifW)
    val mem_backup_en = Bool(INPUT)
    val pcr = Vec(new SMIIO(xLen, pcrAddrBits), nTiles)
    val scr = new SMIIO(xLen, scrAddrBits)
    val mmio = new NastiIO
  }

  // Create a simple L1toL2 NoC between the tiles+htif and the banks of outer memory
  // Cached ports are first in client list, making sharerToClientId just an indentity function
  // addrToBank is sed to hash physical addresses (of cache blocks) to banks (and thereby memory channels)
  val ordered_clients = (io.tiles_cached ++ (io.tiles_uncached :+ io.htif_uncached).map(TileLinkIOWrapper(_))) 
  def sharerToClientId(sharerId: UInt) = sharerId
  def addrToBank(addr: Bits): UInt = if(nBanks > 1) addr(lsb + log2Up(nBanks) - 1, lsb) else UInt(0)
  val preBuffering = TileLinkDepths(2,2,2,2,2)
  val postBuffering = TileLinkDepths(0,0,1,0,0) //TODO: had EOS24 crit path on inner.release
  val l1tol2net = Module(
    if(nBanks == 1) new RocketChipTileLinkArbiter(sharerToClientId, preBuffering, postBuffering)
    else new RocketChipTileLinkCrossbar(addrToBank, sharerToClientId, preBuffering, postBuffering))

  // Create point(s) of coherence serialization
  val nManagers = nMemChannels * nBanksPerMemChannel
  val managerEndpoints = List.fill(nManagers) { p(BuildL2CoherenceManager)()}
  managerEndpoints.foreach { _.incoherent := io.incoherent }

  // Wire the tiles and htif to the TileLink client ports of the L1toL2 network,
  // and coherence manager(s) to the other side
  l1tol2net.io.clients <> ordered_clients
  l1tol2net.io.managers <> managerEndpoints.map(_.innerTL)

  // Create a converter between TileLinkIO and MemIO for each channel
  val outerTLParams = p.alterPartial({ case TLId => "L2ToMC" })
  val backendBuffering = TileLinkDepths(0,0,0,0,0)

  val addrMap = new AddrHashMap(p(NastiAddrMap))
  val nMasters = managerEndpoints.size + 1
  val nSlaves = addrMap.nEntries

  println("Generated Address Map")
  for ((name, base, size, _) <- addrMap.sortedEntries) {
    println(f"\t$name%s $base%x - ${base + size - 1}%x")
  }

  val interconnect = Module(new NastiTopInterconnect(nMasters, nSlaves)(p))

  for ((bank, i) <- managerEndpoints.zipWithIndex) {
    val unwrap = Module(new ClientTileLinkIOUnwrapper)(outerTLParams)
    val conv = Module(new NastiIOTileLinkIOConverter)(outerTLParams)
    unwrap.io.in <> bank.outerTL
    conv.io.tl <> unwrap.io.out
    interconnect.io.masters(i) <> conv.io.nasti
  }

  val rtc = Module(new RTC(CSRs.mtime))
  interconnect.io.masters(nManagers) <> rtc.io

  for (i <- 0 until nTiles) {
    val csrName = s"conf:csr$i"
    val csrPort = addrMap(csrName).port
    val conv = Module(new SMIIONastiIOConverter(xLen, pcrAddrBits))
    conv.io.nasti <> interconnect.io.slaves(csrPort)
    io.pcr(i) <> conv.io.smi
  }

  val conv = Module(new SMIIONastiIOConverter(xLen, scrAddrBits))
  conv.io.nasti <> interconnect.io.slaves(addrMap("conf:scr").port)
  io.scr <> conv.io.smi

  io.mmio <> interconnect.io.slaves(addrMap("io").port)

  val mem_channels = interconnect.io.slaves.take(nMemChannels)

  // Create a SerDes for backup memory port
  if(p(UseBackupMemoryPort)) {
    VLSIUtils.doOuterMemorySystemSerdes(
      mem_channels, io.mem, io.mem_backup, io.mem_backup_en,
      nMemChannels, p(HTIFWidth), p(CacheBlockOffsetBits))
  } else { io.mem <> mem_channels }
}
