// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
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
/** Maximum number of banks per memory channel, when configurable */
case object MaxBanksPerMemoryChannel extends Field[Int]
/** Dynamic memory channel configurations */
case object MemoryChannelMuxConfigs extends Field[List[Int]]
/** Least significant bit of address used for bank partitioning */
case object BankIdLSB extends Field[Int]
/** Number of outstanding memory requests */
case object NOutstandingMemReqsPerChannel extends Field[Int]
/** Whether to divide HTIF clock */
case object UseHtifClockDiv extends Field[Boolean]
/** Function for building some kind of coherence manager agent */
case object BuildL2CoherenceManager extends Field[(Int, Parameters) => CoherenceAgent]
/** Function for building some kind of tile connected to a reset signal */
case object BuildTiles extends Field[Seq[(Bool, Parameters) => Tile]]
/** A string describing on-chip devices, readable by target software */
case object ConfigString extends Field[Array[Byte]]
/** Number of L1 clients besides the CPU cores */
case object ExtraL1Clients extends Field[Int] 

case object UseStreamLoopback extends Field[Boolean]
case object StreamLoopbackSize extends Field[Int]
case object StreamLoopbackWidth extends Field[Int]

/** Utility trait for quick access to some relevant parameters */
trait HasTopLevelParameters {
  implicit val p: Parameters
  lazy val nTiles = p(NTiles)
  lazy val nCachedTilePorts = p(TLKey("L1toL2")).nCachingClients
  lazy val nUncachedTilePorts =
    p(TLKey("L1toL2")).nCachelessClients - p(ExtraL1Clients)
  lazy val htifW = p(HtifKey).width
  lazy val csrAddrBits = 12
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val nBanksPerMemChannel = p(NBanksPerMemoryChannel)
  lazy val nBanks = nMemChannels*nBanksPerMemChannel
  lazy val lsb = p(BankIdLSB)
  lazy val nMemReqs = p(NOutstandingMemReqsPerChannel)
  lazy val mifAddrBits = p(MIFAddrBits)
  lazy val mifDataBeats = p(MIFDataBeats)
  lazy val xLen = p(XLen)
  lazy val nSCR =  p(HtifKey).nSCR
  lazy val scrAddrBits = log2Up(nSCR)
  lazy val scrDataBits = 64
  lazy val scrDataBytes = scrDataBits / 8
  lazy val memoryChannelMuxConfigs = p(MemoryChannelMuxConfigs)
  //require(lsb + log2Up(nBanks) < mifAddrBits)
}

class MemBackupCtrlIO extends Bundle {
  val en = Bool(INPUT)
  val in_valid = Bool(INPUT)
  val out_ready = Bool(INPUT)
  val out_valid = Bool(OUTPUT)
}

/** Top-level io for the chip */
class BasicTopIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
    with HasTopLevelParameters {
  val host = new HostIO(htifW)
}

class TopIO(implicit p: Parameters) extends BasicTopIO()(p) {
  val mem = Vec(nMemChannels, new NastiIO)
}

object TopUtils {
  // Connect two Nasti interfaces with queues in-between
  def connectNasti(outer: NastiIO, inner: NastiIO)(implicit p: Parameters) {
    val mifDataBeats = p(MIFDataBeats)
    outer.ar <> Queue(inner.ar)
    outer.aw <> Queue(inner.aw)
    outer.w  <> Queue(inner.w, mifDataBeats)
    inner.r  <> Queue(outer.r, mifDataBeats)
    inner.b  <> Queue(outer.b)
  }
  def connectTilelinkNasti(nasti: NastiIO, tl: ClientUncachedTileLinkIO)(implicit p: Parameters) = {
    val conv = Module(new NastiIOTileLinkIOConverter())
    conv.io.tl <> tl
    TopUtils.connectNasti(nasti, conv.io.nasti)
  }
}

/** Top-level module for the chip */
//TODO: Remove this wrapper once multichannel DRAM controller is provided
class Top(topParams: Parameters) extends Module with HasTopLevelParameters {
  implicit val p = topParams
  val io = new TopIO

  // Build an Uncore and a set of Tiles
  val innerTLParams = p.alterPartial({case TLId => "L1toL2" })
  val uncore = Module(new Uncore()(innerTLParams))
  val tileList = uncore.io.htif zip p(BuildTiles) map { case(hl, bt) => bt(hl.reset, p) }

  // Connect each tile to the HTIF
  uncore.io.htif.zip(tileList).zipWithIndex.foreach {
    case ((hl, tile), i) =>
      tile.io.host.timerIRQ := uncore.io.timerIRQs(i)
      tile.io.host.id := UInt(i)
      tile.io.host.reset := Reg(next=Reg(next=hl.reset))
      tile.io.host.csr.req <> Queue(hl.csr.req)
      hl.csr.resp <> Queue(tile.io.host.csr.resp)
  }

  // Connect the uncore to the tile memory ports, HostIO and MemIO
  uncore.io.tiles_cached <> tileList.map(_.io.cached).flatten
  uncore.io.tiles_uncached <> tileList.map(_.io.uncached).flatten
  io.host <> uncore.io.host

  io.mem.zip(uncore.io.mem).foreach { case (outer, inner) =>
    TopUtils.connectNasti(outer, inner)
    // Memory cache type should be normal non-cacheable bufferable
    outer.ar.bits.cache := UInt("b0011")
    outer.aw.bits.cache := UInt("b0011")
  }
}

/** Wrapper around everything that isn't a Tile.
  *
  * Usually this is clocked and/or place-and-routed separately from the Tiles.
  * Contains the Host-Target InterFace module (HTIF).
  */
class Uncore(implicit val p: Parameters) extends Module
    with HasTopLevelParameters {
  val io = new Bundle {
    val host = new HostIO(htifW)
    val mem = Vec(nMemChannels, new NastiIO)
    val tiles_cached = Vec(nCachedTilePorts, new ClientTileLinkIO).flip
    val tiles_uncached = Vec(nUncachedTilePorts, new ClientUncachedTileLinkIO).flip
    val htif = Vec(nTiles, new HtifIO).flip
    val timerIRQs = Vec(nTiles, Bool()).asOutput
    val mmio = new NastiIO
  }

  val htif = Module(new Htif(CSRs.mreset)) // One HTIF module per chip
  val outmemsys = Module(new OuterMemorySystem) // NoC, LLC and SerDes
  outmemsys.io.incoherent := htif.io.cpu.map(_.reset)
  outmemsys.io.htif_uncached <> htif.io.mem
  outmemsys.io.tiles_uncached <> io.tiles_uncached
  outmemsys.io.tiles_cached <> io.tiles_cached

  for (i <- 0 until nTiles) {
    io.htif(i).reset := htif.io.cpu(i).reset
    io.htif(i).id := htif.io.cpu(i).id
    io.htif(i).csr <> htif.io.cpu(i).csr
  }

  val addrMap = p(GlobalAddrMap)
  val addrHashMap = new AddrHashMap(addrMap)
  val memSize = addrHashMap("mem").size
  val scrFile = Module(new SCRFile("UNCORE_SCR", 0))
  scrFile.io.smi <> htif.io.scr
  scrFile.io.scr.attach(Wire(init = UInt(nTiles)), "N_CORES")
  scrFile.io.scr.attach(Wire(init = UInt(memSize >> 20)), "MMIO_BASE")
  // scrFile.io.scr <> (... your SCR connections ...)

  buildMMIONetwork(p.alterPartial({case TLId => "MMIO_Outermost"}))

  // Configures the enabled memory channels.  This can't be changed while the
  // chip is actively using memory, as it both drops Nasti messages and garbles
  // all of memory.
  val memory_channel_mux_select = scrFile.io.scr.attach(
    Reg(UInt(width = log2Up(memoryChannelMuxConfigs.size))),
    "MEMORY_CHANNEL_MUX_SELECT")
  outmemsys.io.memory_channel_mux_select := memory_channel_mux_select

  // Wire the htif to the memory port(s) and host interface
  io.mem <> outmemsys.io.mem
  if(p(UseHtifClockDiv)) {
    VLSIUtils.padOutHTIFWithDividedClock(htif.io.host, scrFile.io.scr, io.host, htifW)
  } else {
    io.host <> htif.io.host
  }

  def buildMMIONetwork(implicit p: Parameters) = {
    val mmioNarrower = Module(new TileLinkIONarrower("L2toMMIO", "MMIO_Outermost"))
    val mmioNetwork = Module(new TileLinkRecursiveInterconnect(1, addrMap.tail, memSize))

    mmioNarrower.io.in <> outmemsys.io.mmio
    mmioNetwork.io.in.head <> mmioNarrower.io.out

    if (p(UseStreamLoopback)) {
      val lo_width = p(StreamLoopbackWidth)
      val lo_size = p(StreamLoopbackSize)
      val lo_conv = Module(new NastiIOStreamIOConverter(lo_width))
      val lo_port = addrHashMap("devices:loopback").port - 1
      TopUtils.connectTilelinkNasti(lo_conv.io.nasti, mmioNetwork.io.out(lo_port))
      lo_conv.io.stream.in <> Queue(lo_conv.io.stream.out, lo_size)
    }

    val rtc = Module(new RTC(p(NTiles)))
    val rtcAddr = addrHashMap("conf:rtc")
    val rtcPort = rtcAddr.port - 1
    require(rtc.size <= rtcAddr.size)
    rtc.io.tl <> mmioNetwork.io.out(rtcPort)
    io.timerIRQs := rtc.io.irqs

    val deviceTree = Module(new ROMSlave(p(ConfigString).toSeq))
    val dtPort = addrHashMap("conf:devicetree").port - 1
    deviceTree.io <> mmioNetwork.io.out(dtPort)
  }
}

/** The whole outer memory hierarchy, including a NoC, some kind of coherence
  * manager agent, and a converter from TileLink to MemIO.
  */ 
class OuterMemorySystem(implicit val p: Parameters) extends Module with HasTopLevelParameters {
  val io = new Bundle {
    val tiles_cached = Vec(nCachedTilePorts, new ClientTileLinkIO).flip
    val tiles_uncached = Vec(nUncachedTilePorts, new ClientUncachedTileLinkIO).flip
    val htif_uncached = (new ClientUncachedTileLinkIO).flip
    val incoherent = Vec(nTiles, Bool()).asInput
    val mem = Vec(nMemChannels, new NastiIO)
    val memory_channel_mux_select = UInt(INPUT, log2Up(memoryChannelMuxConfigs.size))
    val mmio = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => "L2toMMIO"}))
  }

  val addrHashMap = new AddrHashMap(p(GlobalAddrMap))

  // Create a simple L1toL2 NoC between the tiles+htif and the banks of outer memory
  // Cached ports are first in client list, making sharerToClientId just an indentity function
  // addrToBank is sed to hash physical addresses (of cache blocks) to banks (and thereby memory channels)
  def sharerToClientId(sharerId: UInt) = sharerId
  def addrToBank(addr: UInt): UInt = {
    val isMemory = addrHashMap.isInRegion("mem", addr << log2Up(p(CacheBlockBytes)))
    Mux(isMemory,
      if (nBanks > 1) addr(lsb + log2Up(nBanks) - 1, lsb) else UInt(0),
      UInt(nBanks))
  }
  val preBuffering = TileLinkDepths(2,2,2,2,2)
  val l1tol2net = Module(new PortedTileLinkCrossbar(addrToBank, sharerToClientId, preBuffering))

  // Create point(s) of coherence serialization
  val managerEndpoints = List.tabulate(nBanks){id => p(BuildL2CoherenceManager)(id, p)}
  managerEndpoints.foreach { _.incoherent := io.incoherent }

  val mmioManager = Module(new MMIOTileLinkManager()(p.alterPartial({
    case TLId => "L1toL2"
    case InnerTLId => "L1toL2"
    case OuterTLId => "L2toMMIO"
  })))
  io.mmio <> mmioManager.io.outer

  // Wire the tiles and htif to the TileLink client ports of the L1toL2 network,
  // and coherence manager(s) to the other side
  l1tol2net.io.clients_cached <> io.tiles_cached
  l1tol2net.io.clients_uncached <> io.tiles_uncached ++ Seq(io.htif_uncached)
  l1tol2net.io.managers <> managerEndpoints.map(_.innerTL) :+ mmioManager.io.inner

  // Create a converter between TileLinkIO and MemIO for each channel
  val outerTLParams = p.alterPartial({ case TLId => "L2toMC" })
  val outermostTLParams = p.alterPartial({case TLId => "Outermost"})
  val backendBuffering = TileLinkDepths(0,0,0,0,0)

  // TODO: the code to print this stuff should live somewhere else
  println("Generated Address Map")
  for ((name, base, size, _, _) <- addrHashMap.sortedEntries) {
    println(f"\t$name%s $base%x - ${base + size - 1}%x")
  }
  println("Generated Configuration String")
  println(new String(p(ConfigString)))

  val channelConfigs = p(MemoryChannelMuxConfigs)
  require(channelConfigs.sortWith(_ > _)(0) == nMemChannels,
                "More memory channels elaborated than can be enabled")
  val mem_ic =
    if (channelConfigs.size == 1) {
      val ic = Module(new TileLinkMemoryInterconnect(
        nBanksPerMemChannel, nMemChannels)(outermostTLParams))
      ic
    } else {
      val nBanks = nBanksPerMemChannel * nMemChannels
      val ic = Module(new TileLinkMemorySelector(
        nBanks, nMemChannels, channelConfigs)(outermostTLParams))
      ic.io.select := io.memory_channel_mux_select
      ic
    }

  for ((bank, i) <- managerEndpoints.zipWithIndex) {
    val unwrap = Module(new ClientTileLinkIOUnwrapper()(outerTLParams))
    val narrow = Module(new TileLinkIONarrower("L2toMC", "Outermost"))
    unwrap.io.in <> ClientTileLinkEnqueuer(bank.outerTL, backendBuffering)(outerTLParams)
    narrow.io.in <> unwrap.io.out
    mem_ic.io.in(i) <> narrow.io.out
  }

  for ((nasti, tl) <- io.mem zip mem_ic.io.out)
    TopUtils.connectTilelinkNasti(nasti, tl)(outermostTLParams)
}
