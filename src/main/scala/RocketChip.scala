// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.util._
import uncore.converters._
import rocket._
import rocket.Util._

/** Top-level parameters of RocketChip, values set in e.g. PublicConfigs.scala */

/** Options for memory bus interface */
object BusType {
  sealed trait EnumVal
  case object AXI extends EnumVal
  case object AHB extends EnumVal
  case object TL  extends EnumVal
  val busTypes = Seq(AXI, AHB, TL)
}

/** Number of memory channels */
case object NMemoryChannels extends Field[Int]
case object TMemoryChannels extends Field[BusType.EnumVal]
/** Number of banks per memory channel */
case object NBanksPerMemoryChannel extends Field[Int]
/** Least significant bit of address used for bank partitioning */
case object BankIdLSB extends Field[Int]
/** Number of outstanding memory requests */
case object NOutstandingMemReqsPerChannel extends Field[Int]
/** Number of exteral MMIO ports */
case object ExtMMIOPorts extends Field[AddrMap]
case object NExtMMIOAXIChannels extends Field[Int]
case object NExtMMIOAHBChannels extends Field[Int]
case object NExtMMIOTLChannels  extends Field[Int]
/** Function for building some kind of coherence manager agent */
case object BuildL2CoherenceManager extends Field[(Int, Parameters) => CoherenceAgent]
/** Function for building some kind of tile connected to a reset signal */
case object BuildTiles extends Field[Seq[(Bool, Parameters) => Tile]]
/** A string describing on-chip devices, readable by target software */
case object ConfigString extends Field[Array[Byte]]
/** Number of external interrupt sources */
case object NExtInterrupts extends Field[Int]
/** Interrupt controller configuration */
case object PLICKey extends Field[PLICConfig]
/** Number of clock cycles per RTC tick */
case object RTCPeriod extends Field[Int]

case object UseStreamLoopback extends Field[Boolean]
case object StreamLoopbackSize extends Field[Int]
case object StreamLoopbackWidth extends Field[Int]

/** Utility trait for quick access to some relevant parameters */
trait HasTopLevelParameters {
  implicit val p: Parameters
  lazy val nTiles = p(NTiles)
  lazy val nCachedTilePorts = p(NCachedTileLinkPorts)
  lazy val nUncachedTilePorts = p(NUncachedTileLinkPorts)
  lazy val csrAddrBits = 12
  lazy val tMemChannels = p(TMemoryChannels)
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val nMemAXIChannels = if (tMemChannels == BusType.AXI) nMemChannels else 0
  lazy val nMemAHBChannels = if (tMemChannels == BusType.AHB) nMemChannels else 0
  lazy val nMemTLChannels  = if (tMemChannels == BusType.TL)  nMemChannels else 0
  lazy val nBanksPerMemChannel = p(NBanksPerMemoryChannel)
  lazy val nBanks = nMemChannels*nBanksPerMemChannel
  lazy val lsb = p(BankIdLSB)
  lazy val nMemReqs = p(NOutstandingMemReqsPerChannel)
  lazy val mifAddrBits = p(MIFAddrBits)
  lazy val mifDataBeats = p(MIFDataBeats)
  lazy val xLen = p(XLen)
  lazy val outermostParams = p.alterPartial({ case TLId => "Outermost" })
  lazy val outermostMMIOParams = p.alterPartial({ case TLId => "MMIO_Outermost" })
}

class MemBackupCtrlIO extends Bundle {
  val en = Bool(INPUT)
  val in_valid = Bool(INPUT)
  val out_ready = Bool(INPUT)
  val out_valid = Bool(OUTPUT)
}

/** Top-level io for the chip */
class BasicTopIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
    with HasTopLevelParameters

class TopIO(implicit p: Parameters) extends BasicTopIO()(p) {
  val mem_axi = Vec(nMemAXIChannels, new NastiIO)
  val mem_ahb = Vec(nMemAHBChannels, new HastiMasterIO)
  val mem_tl  = Vec(nMemTLChannels,  new ClientUncachedTileLinkIO()(outermostParams))
  val interrupts = Vec(p(NExtInterrupts), Bool()).asInput
  val mmio_axi = Vec(p(NExtMMIOAXIChannels), new NastiIO)
  val mmio_ahb = Vec(p(NExtMMIOAHBChannels), new HastiMasterIO)
  val mmio_tl  = Vec(p(NExtMMIOTLChannels),  new ClientUncachedTileLinkIO()(outermostMMIOParams))
  val debug = new DebugBusIO()(p).flip
}

object TopUtils {
  // Connect two Nasti interfaces with queues in-between
  def connectNasti(outer: NastiIO, inner: NastiIO)(implicit p: Parameters) {
    val mifDataBeats = p(MIFDataBeats)
    outer.ar <> Queue(inner.ar, 1)
    outer.aw <> Queue(inner.aw, 1)
    outer.w  <> Queue(inner.w)
    inner.r  <> Queue(outer.r)
    inner.b  <> Queue(outer.b, 1)
  }
  def connectTilelinkNasti(nasti: NastiIO, tl: ClientUncachedTileLinkIO)(implicit p: Parameters) = {
    val conv = Module(new NastiIOTileLinkIOConverter())
    conv.io.tl <> tl
    TopUtils.connectNasti(nasti, conv.io.nasti)
  }
  def connectTilelinkAhb(ahb: HastiMasterIO, tl: ClientUncachedTileLinkIO)(implicit p: Parameters) = {
    val bridge = Module(new AHBBridge(true))
    bridge.io.tl <> tl
    bridge.io.ahb
  }
  def connectTilelink(
      outer: ClientUncachedTileLinkIO, inner: ClientUncachedTileLinkIO)(implicit p: Parameters) = {
    outer.acquire <> Queue(inner.acquire)
    inner.grant <> Queue(outer.grant)
  }
  def makeBootROM()(implicit p: Parameters) = {
    val rom = java.nio.ByteBuffer.allocate(32)
    rom.order(java.nio.ByteOrder.LITTLE_ENDIAN)

    // for now, have the reset vector jump straight to memory
    val resetToMemDist = p(GlobalAddrMap)("mem").start - p(ResetVector)
    require(resetToMemDist == (resetToMemDist.toInt >> 12 << 12))
    val configStringAddr = p(ResetVector).toInt + rom.capacity

    // This boot ROM doesn't know about any boot devices, so it just spins,
    // waiting for the debugger to load a program and change the PC.
    rom.putInt(0x0000006f)                        // loop forever
    rom.putInt(0)                                 // reserved
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
  val tileResets = Wire(Vec(nTiles, Bool()))
  val tileList = p(BuildTiles).zip(tileResets).map {
    case (tile, rst) => tile(rst, p)
  }
  val nCachedPorts = tileList.map(tile => tile.io.cached.size).reduce(_ + _)
  val nUncachedPorts = tileList.map(tile => tile.io.uncached.size).reduce(_ + _)

  val innerTLParams = p.alterPartial({
    case HastiId => "TL"
    case TLId => "L1toL2"
    case NCachedTileLinkPorts => nCachedPorts
    case NUncachedTileLinkPorts => nUncachedPorts
  })

  val uncore = Module(new Uncore()(innerTLParams))

  uncore.io.prci.zip(tileResets).zip(tileList).foreach {
    case ((prci, rst), tile) =>
      rst := prci.reset
      tile.io.prci <> prci
  }

  // Connect the uncore to the tile memory ports, HostIO and MemIO
  uncore.io.tiles_cached <> tileList.map(_.io.cached).flatten
  uncore.io.tiles_uncached <> tileList.map(_.io.uncached).flatten
  uncore.io.interrupts <> io.interrupts
  uncore.io.debugBus <> io.debug

  io.mmio_axi <> uncore.io.mmio_axi
  io.mmio_ahb <> uncore.io.mmio_ahb
  io.mmio_tl <> uncore.io.mmio_tl
  io.mem_axi <> uncore.io.mem_axi
  io.mem_ahb <> uncore.io.mem_ahb
  io.mem_tl <> uncore.io.mem_tl
}

/** Wrapper around everything that isn't a Tile.
  *
  * Usually this is clocked and/or place-and-routed separately from the Tiles.
  */
class Uncore(implicit val p: Parameters) extends Module
    with HasTopLevelParameters {

  val io = new Bundle {
    val mem_axi = Vec(nMemAXIChannels, new NastiIO)
    val mem_ahb = Vec(nMemAHBChannels, new HastiMasterIO)
    val mem_tl  = Vec(nMemTLChannels,  new ClientUncachedTileLinkIO()(outermostParams))
    val tiles_cached = Vec(nCachedTilePorts, new ClientTileLinkIO).flip
    val tiles_uncached = Vec(nUncachedTilePorts, new ClientUncachedTileLinkIO).flip
    val prci = Vec(nTiles, new PRCITileIO).asOutput
    val mmio_axi = Vec(p(NExtMMIOAXIChannels), new NastiIO)
    val mmio_ahb = Vec(p(NExtMMIOAHBChannels), new HastiMasterIO)
    val mmio_tl  = Vec(p(NExtMMIOTLChannels),  new ClientUncachedTileLinkIO()(outermostMMIOParams))
    val interrupts = Vec(p(NExtInterrupts), Bool()).asInput
    val debugBus = new DebugBusIO()(p).flip
  }

  val outmemsys = if (nCachedTilePorts + nUncachedTilePorts > 0)
    Module(new OuterMemorySystem) // NoC, LLC and SerDes
  else Module(new DummyOuterMemorySystem)
  outmemsys.io.incoherent foreach (_ := false)
  outmemsys.io.tiles_uncached <> io.tiles_uncached
  outmemsys.io.tiles_cached <> io.tiles_cached

  buildMMIONetwork(p.alterPartial({case TLId => "L2toMMIO"}))

  io.mem_axi <> outmemsys.io.mem_axi
  io.mem_ahb <> outmemsys.io.mem_ahb
  io.mem_tl  <> outmemsys.io.mem_tl

  def connectExternalMMIO(ports: Seq[ClientUncachedTileLinkIO])(implicit p: Parameters) {
    val mmio_axi_start = 0
    val mmio_axi_end   = mmio_axi_start + p(NExtMMIOAXIChannels)
    val mmio_ahb_start = mmio_axi_end
    val mmio_ahb_end   = mmio_ahb_start + p(NExtMMIOAHBChannels)
    val mmio_tl_start  = mmio_ahb_end
    val mmio_tl_end    = mmio_tl_start  + p(NExtMMIOTLChannels)
    require (mmio_tl_end == ports.size)
    
    for (i <- 0 until ports.size) {
      if (mmio_axi_start <= i && i < mmio_axi_end) {
        TopUtils.connectTilelinkNasti(io.mmio_axi(i-mmio_axi_start), ports(i))
      } else if (mmio_ahb_start <= i && i < mmio_ahb_end) {
        val ahbBridge = Module(new AHBBridge(true))
        io.mmio_ahb(i-mmio_ahb_start) <> ahbBridge.io.ahb
        ahbBridge.io.tl <> ports(i)
      } else if (mmio_tl_start <= i && i < mmio_tl_end) {
        TopUtils.connectTilelink(io.mmio_tl(i-mmio_tl_start), ports(i))
      } else {
        TopUtils.connectTilelinkNasti(Module(new NastiErrorSlave).io, ports(i))
      }
    }
  }

  def buildMMIONetwork(implicit p: Parameters) = {
    val ioAddrMap = p(GlobalAddrMap).subMap("io")

    val mmioNetwork = Module(new TileLinkRecursiveInterconnect(1, ioAddrMap))
    mmioNetwork.io.in.head <> outmemsys.io.mmio

    val plic = Module(new PLIC(p(PLICKey)))
    plic.io.tl <> mmioNetwork.port("int:plic")
    for (i <- 0 until io.interrupts.size) {
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := io.interrupts(i)
      plic.io.devices(i) <> gateway.io.plic
    }

    val debugModule = Module(new DebugModule)
    debugModule.io.tl <> mmioNetwork.port("int:debug")
    debugModule.io.db <> io.debugBus

    val prci = Module(new PRCI)
    prci.io.tl <> mmioNetwork.port("int:prci")
    io.prci := prci.io.tiles
    prci.io.rtcTick := Counter(p(RTCPeriod)).inc() // placeholder for real RTC

    for (i <- 0 until nTiles) {
      prci.io.interrupts(i).meip := plic.io.harts(plic.cfg.context(i, 'M'))
      if (p(UseVM))
        prci.io.interrupts(i).seip := plic.io.harts(plic.cfg.context(i, 'S'))
      prci.io.interrupts(i).debug := debugModule.io.debugInterrupts(i)

      io.prci(i).reset := reset
    }

    val bootROM = Module(new ROMSlave(TopUtils.makeBootROM()))
    bootROM.io <> mmioNetwork.port("int:bootrom")

    // The memory map presently has only one external I/O region
    val ext = p(ExtMMIOPorts).entries.map(port => TileLinkWidthAdapter(mmioNetwork.port(port.name), "MMIO_Outermost"))
    connectExternalMMIO(ext)(outermostMMIOParams)
  }
}

abstract class AbstractOuterMemorySystem(implicit val p: Parameters)
    extends Module with HasTopLevelParameters {
  val io = new Bundle {
    val tiles_cached = Vec(nCachedTilePorts, new ClientTileLinkIO).flip
    val tiles_uncached = Vec(nUncachedTilePorts, new ClientUncachedTileLinkIO).flip
    val incoherent = Vec(nCachedTilePorts, Bool()).asInput
    val mem_axi = Vec(nMemAXIChannels, new NastiIO)
    val mem_ahb = Vec(nMemAHBChannels, new HastiMasterIO)
    val mem_tl  = Vec(nMemTLChannels,  new ClientUncachedTileLinkIO()(outermostParams))
    val mmio = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => "L2toMMIO"}))
  }
}

/** Use in place of OuterMemorySystem if there are no clients to connect. */
class DummyOuterMemorySystem(implicit p: Parameters) extends AbstractOuterMemorySystem()(p) {
  require(nCachedTilePorts + nUncachedTilePorts == 0)

  io.mem_axi.foreach { axi =>
    axi.ar.valid := Bool(false)
    axi.aw.valid := Bool(false)
    axi.w.valid := Bool(false)
    axi.r.ready := Bool(false)
    axi.b.ready := Bool(false)
  }

  io.mem_ahb.foreach { ahb =>
    ahb.htrans := UInt(0)
    ahb.hmastlock := Bool(false)
    ahb.hwrite := Bool(false)
    ahb.haddr := UInt(0)
    ahb.hburst := UInt(0)
    ahb.hsize := UInt(0)
    ahb.hprot := UInt(0)
  }

  io.mem_tl.foreach { tl =>
    tl.acquire.valid := Bool(false)
    tl.grant.ready := Bool(false)
  }

  io.mmio.acquire.valid := Bool(false)
  io.mmio.grant.ready := Bool(false)
}

/** The whole outer memory hierarchy, including a NoC, some kind of coherence
  * manager agent, and a converter from TileLink to MemIO.
  */ 
class OuterMemorySystem(implicit p: Parameters) extends AbstractOuterMemorySystem()(p) {
  // Create a simple L1toL2 NoC between the tiles and the banks of outer memory
  // Cached ports are first in client list, making sharerToClientId just an indentity function
  // addrToBank is sed to hash physical addresses (of cache blocks) to banks (and thereby memory channels)
  def sharerToClientId(sharerId: UInt) = sharerId
  def addrToBank(addr: UInt): UInt = {
    val isMemory = p(GlobalAddrMap).isInRegion("mem", addr << log2Up(p(CacheBlockBytes)))
    Mux(isMemory,
      if (nBanks > 1) addr(lsb + log2Up(nBanks) - 1, lsb) else UInt(0),
      UInt(nBanks))
  }
  val preBuffering = TileLinkDepths(1,1,2,2,0)
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

  // Wire the tiles to the TileLink client ports of the L1toL2 network,
  // and coherence manager(s) to the other side
  l1tol2net.io.clients_cached <> io.tiles_cached
  l1tol2net.io.clients_uncached <> io.tiles_uncached
  l1tol2net.io.managers <> managerEndpoints.map(_.innerTL) :+ mmioManager.io.inner

  // Create a converter between TileLinkIO and MemIO for each channel
  val outerTLParams = p.alterPartial({ case TLId => "L2toMC" })
  val outermostTLParams = p.alterPartial({case TLId => "Outermost"})
  val backendBuffering = TileLinkDepths(0,0,0,0,0)

  // TODO: the code to print this stuff should live somewhere else
  println("Generated Address Map")
  for ((name, region) <- p(GlobalAddrMap).flatten) {
    println(f"\t$name%s ${region.start}%x - ${region.start + region.size - 1}%x")
  }
  println("Generated Configuration String")
  println(new String(p(ConfigString)))

  val mem_ic = Module(new TileLinkMemoryInterconnect(nBanksPerMemChannel, nMemChannels)(outermostTLParams))

  for ((bank, icPort) <- managerEndpoints zip mem_ic.io.in) {
    val unwrap = Module(new ClientTileLinkIOUnwrapper()(outerTLParams))
    unwrap.io.in <> ClientTileLinkEnqueuer(bank.outerTL, backendBuffering)(outerTLParams)
    TileLinkWidthAdapter(icPort, unwrap.io.out)
  }

  for ((nasti, tl) <- io.mem_axi zip mem_ic.io.out) {
    TopUtils.connectTilelinkNasti(nasti, tl)(outermostTLParams)
    // Memory cache type should be normal non-cacheable bufferable
    // TODO why is this happening here?  Would 0000 (device) be OK instead?
    nasti.ar.bits.cache := UInt("b0011")
    nasti.aw.bits.cache := UInt("b0011")
  }
  
  // Abuse the fact that zip takes the shorter of the two lists
  for ((ahb, tl) <- io.mem_ahb zip mem_ic.io.out) {
    val bridge = Module(new AHBBridge(false)) // no atomics
    ahb <> bridge.io.ahb
    bridge.io.tl <> tl
  }

  for ((mem_tl, tl) <- io.mem_tl zip mem_ic.io.out) {
    TopUtils.connectTilelink(mem_tl, tl)
  }
}
