// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore._
import rocket._
import rocket.Util._

/** Top-level parameters of RocketChip, values set in e.g. PublicConfigs.scala */

/** Number of memory channels */
case object NMemoryChannels extends Field[Int]
/** Number of banks per memory channel */
case object NBanksPerMemoryChannel extends Field[Int]
/** Least significant bit of address used for bank partitioning */
case object BankIdLSB extends Field[Int]
/** Number of outstanding memory requests */
case object NOutstandingMemReqsPerChannel extends Field[Int]
/** Number of exteral MMIO ports */
case object NExtMMIOChannels extends Field[Int]
/** Whether to divide HTIF clock */
case object UseHtifClockDiv extends Field[Boolean]
/** Function for building some kind of coherence manager agent */
case object BuildL2CoherenceManager extends Field[(Int, Parameters) => CoherenceAgent]
/** Function for building some kind of tile connected to a reset signal */
case object BuildTiles extends Field[Seq[(Clock, Bool, Parameters) => Tile]]
/** A string describing on-chip devices, readable by target software */
case object ConfigString extends Field[Array[Byte]]
/** Number of L1 clients besides the CPU cores */
case object ExtraL1Clients extends Field[Int] 
/** Number of external interrupt sources */
case object NExtInterrupts extends Field[Int]
/** Interrupt controller configuration */
case object PLICKey extends Field[PLICConfig]

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
  val interrupts = Vec(p(NExtInterrupts), Bool()).asInput
  val mmio = Vec(p(NExtMMIOChannels), new NastiIO)
  val core_clk = Vec(p(NTiles) - 1, Clock()).asInput
  val oms_clk = Clock().asInput
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
  def makeBootROM()(implicit p: Parameters) = {
    val rom = java.nio.ByteBuffer.allocate(32)
    rom.order(java.nio.ByteOrder.LITTLE_ENDIAN)

    // for now, have the reset vector jump straight to memory
    val addrHashMap = p(GlobalAddrHashMap)
    val resetToMemDist = addrHashMap("mem").start - p(ResetVector)
    require(resetToMemDist == (resetToMemDist.toInt >> 12 << 12))
    val configStringAddr = p(ResetVector).toInt + rom.capacity

    rom.putInt(0x00000297 + resetToMemDist.toInt) // auipc t0, &mem - &here
    rom.putInt(0x00028067)                        // jr t0
    rom.putInt(0)                                 // reserved
    rom.putInt(configStringAddr)                  // pointer to config string
    rom.putInt(0)                                 // default trap vector
    rom.putInt(0)                                 //   ...
    rom.putInt(0)                                 //   ...
    rom.putInt(0)                                 //   ...

    rom.array() ++ p(ConfigString).toSeq
  }
}

/** Top-level module for the chip */
//TODO: Remove this wrapper once multichannel DRAM controller is provided
class Top(topParams: Parameters) extends Module with HasTopLevelParameters {
  implicit val p = topParams
  val io = new TopIO

  // Build an Uncore and a set of Tiles
  val innerTLParams = p.alterPartial({case TLId => "L1toL2" })
  val syncedReset = RegNext(RegNext(reset))
  val uncore = Module(new Uncore(resetSignal = syncedReset)(innerTLParams))
  val tileList = uncore.io.prci.zip(p(BuildTiles)).zipWithIndex.map
  {
    case((prci, tile), i) =>
      if(i == nTiles-1) tile(clock, prci.reset, p) // PMU is on same clock as uncore
      else tile(io.core_clk(i), prci.reset, p)
  }

  // Connect each tile to the HTIF
  for ((prci, tile) <- uncore.io.prci zip tileList) {
    tile.io.prci <> prci
  }

  // Connect the uncore to the tile memory ports, HostIO and MemIO
  uncore.io.tiles_cached <> tileList.map(_.io.cached).flatten
  uncore.io.tiles_uncached <> tileList.map(_.io.uncached).flatten
  io.host <> uncore.io.host
  uncore.io.interrupts <> io.interrupts
  uncore.io.oms_clk := io.oms_clk

  io.mmio <> uncore.io.mmio
  io.mem <> uncore.io.mem
}

/** Wrapper around everything that isn't a Tile.
  *
  * Usually this is clocked and/or place-and-routed separately from the Tiles.
  * Contains the Host-Target InterFace module (HTIF).
  */
class Uncore(resetSignal:Bool = null)(implicit val p: Parameters) extends Module(_reset = resetSignal)
    with HasTopLevelParameters {
  val io = new Bundle {
    val host = new HostIO(htifW)
    val mem = Vec(nMemChannels, new NastiIO)
    val tiles_cached = Vec(nCachedTilePorts, new ClientTileLinkIO).flip
    val tiles_uncached = Vec(nUncachedTilePorts, new ClientUncachedTileLinkIO).flip
    val prci = Vec(nTiles, new PRCITileIO).asOutput
    val mmio = Vec(p(NExtMMIOChannels), new NastiIO)
    val interrupts = Vec(p(NExtInterrupts), Bool()).asInput
    val core_clk = Vec(p(NTiles) - 1, Clock()).asInput
    val oms_clk = Clock().asInput
  }

  val htif = Module(new Htif(CSRs.mreset)) // One HTIF module per chip
  val oms_reset = RegNext(RegNext(reset))
  val outmemsys = Module(new OuterMemorySystem(clockSignal = io.oms_clk, resetSignal = oms_reset)) // NoC, LLC and SerDes
  outmemsys.io.incoherent := htif.io.cpu.map(_.reset)
  outmemsys.io.htif_uncached <> htif.io.mem
  outmemsys.io.tiles_uncached <> io.tiles_uncached
  outmemsys.io.tiles_cached <> io.tiles_cached

  val addrMap = p(GlobalAddrMap)
  val addrHashMap = p(GlobalAddrHashMap)
  val scrFile = Module(new SCRFile("UNCORE_SCR", 0))
  scrFile.io.smi <> htif.io.scr
  // scrFile.io.scr <> (... your SCR connections ...)

  buildMMIONetwork(p.alterPartial({case TLId => "MMIO_Outermost"}))

  // Wire the htif to the memory port(s) and host interface
  io.mem <> outmemsys.io.mem
  if(p(UseHtifClockDiv)) {
    VLSIUtils.padOutHTIFWithDividedClock(htif.io.host, scrFile.io.scr, io.host, htifW)
  } else {
    io.host <> htif.io.host
  }

  // Tie off HTIF CSR ports
  htif.io.cpu.foreach { _.csr.resp.valid := Bool(false) }

  def buildMMIONetwork(implicit p: Parameters) = {
    val (ioBase, ioAddrMap) = addrHashMap.subMap("io")
    val ioAddrHashMap = new AddrHashMap(ioAddrMap, ioBase)

    val mmioNarrower = Module(new TileLinkIONarrower("L2toMMIO", "MMIO_Outermost"))
    val mmioNetwork = Module(new TileLinkRecursiveInterconnect(1, ioAddrMap, ioBase))

    mmioNarrower.io.in <> outmemsys.io.mmio
    mmioNetwork.io.in.head <> mmioNarrower.io.out

    val rtc = Module(new RTC(p(NTiles)))
    val rtcAddr = ioAddrHashMap("int:rtc")
    require(rtc.size <= rtcAddr.region.size)
    rtc.io.tl <> mmioNetwork.io.out(rtcAddr.port)

    val plic = Module(new PLIC(p(PLICKey)))
    val plicAddr = ioAddrHashMap("int:plic")
    plic.io.tl <> mmioNetwork.io.out(plicAddr.port)
    for (i <- 0 until io.interrupts.size) {
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := io.interrupts(i)
      plic.io.devices(i) <> gateway.io.plic
    }

    for (i <- 0 until nTiles) {
      val prci = Module(new PRCI)
      val prciAddr = ioAddrHashMap(s"int:prci$i")
      prci.io.tl <> mmioNetwork.io.out(prciAddr.port)

      prci.io.id := UInt(i)
      prci.io.interrupts.mtip := rtc.io.irqs(i)
      prci.io.interrupts.meip := plic.io.harts(plic.cfg.context(i, 'M'))
      if (p(UseVM))
        prci.io.interrupts.seip := plic.io.harts(plic.cfg.context(i, 'S'))
      prci.io.interrupts.debug := Bool(false)

      io.prci(i) := prci.io.tile
      if( i == (nTiles - 1))
        io.prci(i).reset := htif.io.cpu(i).reset // TODO PMU core does not need sync
      else
        io.prci(i).reset := Reg(next=Reg(next=htif.io.cpu(i).reset)) // TODO
    }

    val bootROM = Module(new ROMSlave(TopUtils.makeBootROM()))
    val bootROMAddr = ioAddrHashMap("int:bootrom")
    bootROM.io <> mmioNetwork.io.out(bootROMAddr.port)

    val debugModule = Module(new ROMSlave(Seq())) // TODO
    val debugModuleAddr = ioAddrHashMap("int:debug")
    debugModule.io <> mmioNetwork.io.out(debugModuleAddr.port)

    val mmioEndpoint = p(NExtMMIOChannels) match {
      case 0 => Module(new NastiErrorSlave).io
      case 1 => io.mmio(0)
      // The memory map presently has only one external I/O region
    }
    TopUtils.connectTilelinkNasti(mmioEndpoint, mmioNetwork.io.out(ioAddrHashMap("ext").port))
  }
}

/** The whole outer memory hierarchy, including a NoC, some kind of coherence
  * manager agent, and a converter from TileLink to MemIO.
  */ 
class OuterMemorySystem(clockSignal: Clock = null, resetSignal: Bool = null)(implicit val p: Parameters)
    extends Module(Option(clockSignal), Option(resetSignal)) with HasTopLevelParameters {
  val io = new Bundle {
    val tiles_cached = Vec(nCachedTilePorts, new ClientTileLinkIO).flip
    val tiles_uncached = Vec(nUncachedTilePorts, new ClientUncachedTileLinkIO).flip
    val htif_uncached = (new ClientUncachedTileLinkIO).flip
    val incoherent = Vec(nTiles, Bool()).asInput
    val mem = Vec(nMemChannels, new NastiIO)
    val mmio = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => "L2toMMIO"}))
  }

  val addrHashMap = p(GlobalAddrHashMap)

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
  val backendBuffering = TileLinkDepths(2,0,0,2,0)

  // TODO: the code to print this stuff should live somewhere else
  println("Generated Address Map")
  for ((name, base, region) <- addrHashMap.sortedEntries) {
    println(f"\t$name%s $base%x - ${base + region.size - 1}%x")
  }
  println("Generated Configuration String")
  println(new String(p(ConfigString)))

  val mem_ic = Module(new TileLinkMemoryInterconnect(nBanksPerMemChannel, nMemChannels)(outermostTLParams))

  for ((bank, icPort) <- managerEndpoints zip mem_ic.io.in) {
    val unwrap = Module(new ClientTileLinkIOUnwrapper()(outerTLParams))
    val narrow = Module(new TileLinkIONarrower("L2toMC", "Outermost"))
    unwrap.io.in <> ClientTileLinkEnqueuer(bank.outerTL, backendBuffering)(outerTLParams)
    narrow.io.in <> unwrap.io.out
    icPort <> narrow.io.out
  }

  for ((nasti, tl) <- io.mem zip mem_ic.io.out) {
    TopUtils.connectTilelinkNasti(nasti, tl)(outermostTLParams)
    // Memory cache type should be normal non-cacheable bufferable
    // TODO why is this happening here?  Would 0000 (device) be OK instead?
    nasti.ar.bits.cache := UInt("b0011")
    nasti.aw.bits.cache := UInt("b0011")
  }
}
