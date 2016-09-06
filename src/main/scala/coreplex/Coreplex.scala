package coreplex

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
import java.nio.{ByteBuffer,ByteOrder}
import java.nio.file.{Files, Paths}

/** Number of memory channels */
case object NMemoryChannels extends Field[Int]
/** Number of banks per memory channel */
case object NBanksPerMemoryChannel extends Field[Int]
/** Least significant bit of address used for bank partitioning */
case object BankIdLSB extends Field[Int]
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
/** The file to read the BootROM contents from */
case object BootROMFile extends Field[String]
/** Export an external MMIO slave port */
case object ExportMMIOPort extends Field[Boolean]
/** Expose additional TileLink client ports */
case object NExternalClients extends Field[Int]
/** Extra top-level ports exported from the coreplex */
case object ExtraCoreplexPorts extends Field[Parameters => Bundle]

trait HasCoreplexParameters {
  implicit val p: Parameters
  lazy val nTiles = p(NTiles)
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val nBanksPerMemChannel = p(NBanksPerMemoryChannel)
  lazy val nBanks = nMemChannels*nBanksPerMemChannel
  lazy val lsb = p(BankIdLSB)
  lazy val innerParams = p.alterPartial({ case TLId => "L1toL2" })
  lazy val outermostParams = p.alterPartial({ case TLId => "Outermost" })
  lazy val outermostMMIOParams = p.alterPartial({ case TLId => "MMIO_Outermost" })
  lazy val nExtClients = p(NExternalClients)
  lazy val exportMMIO = p(ExportMMIOPort)
}

abstract class Coreplex(implicit val p: Parameters) extends Module
    with HasCoreplexParameters {
  class CoreplexIO(implicit val p: Parameters) extends Bundle {
    val mem  = Vec(nMemChannels, new ClientUncachedTileLinkIO()(outermostParams))
    val ext_clients = Vec(nExtClients, new ClientUncachedTileLinkIO()(innerParams)).flip
    val mmio = p(ExportMMIOPort).option(new ClientUncachedTileLinkIO()(outermostMMIOParams))
    val interrupts = Vec(p(NExtInterrupts), Bool()).asInput
    val debug = new DebugBusIO()(p).flip
    val rtcTick = new Bool(INPUT)
    val extra = p(ExtraCoreplexPorts)(p)
    val success: Option[Bool] = hasSuccessFlag.option(Bool(OUTPUT))
  }

  def hasSuccessFlag: Boolean = false
  val io = new CoreplexIO
}

class DefaultCoreplex(topParams: Parameters) extends Coreplex()(topParams) {
  // Build a set of Tiles
  val tileResets = Wire(Vec(nTiles, Bool()))
  val tileList = p(BuildTiles).zip(tileResets).map {
    case (tile, rst) => tile(rst, p)
  }
  val nCachedPorts = tileList.map(tile => tile.io.cached.size).reduce(_ + _)
  val nUncachedPorts = tileList.map(tile => tile.io.uncached.size).reduce(_ + _)

  printConfigString
  buildUncore(p.alterPartial({
    case HastiId => "TL"
    case TLId => "L1toL2"
    case NCachedTileLinkPorts => nCachedPorts
    case NUncachedTileLinkPorts => nUncachedPorts
  }))

  def printConfigString(implicit p: Parameters) = {
    println("Generated Address Map")
    for (entry <- p(GlobalAddrMap).flatten) {
      val name = entry.name
      val start = entry.region.start
      val end = entry.region.start + entry.region.size - 1
      println(f"\t$name%s $start%x - $end%x")
    }
    println("Generated Configuration String")
    println(new String(p(ConfigString)))
  }

  def buildUncore(implicit p: Parameters) = {
    // Create a simple L1toL2 NoC between the tiles and the banks of outer memory
    // Cached ports are first in client list, making sharerToClientId just an indentity function
    // addrToBank is sed to hash physical addresses (of cache blocks) to banks (and thereby memory channels)
    def sharerToClientId(sharerId: UInt) = sharerId
    def addrToBank(addr: UInt): UInt = if (nBanks == 0) UInt(0) else {
      val isMemory = p(GlobalAddrMap).isInRegion("mem", addr << log2Up(p(CacheBlockBytes)))
      Mux(isMemory, addr.extract(lsb + log2Ceil(nBanks) - 1, lsb), UInt(nBanks))
    }
    val preBuffering = TileLinkDepths(1,1,2,2,0)
    val l1tol2net = Module(new PortedTileLinkCrossbar(addrToBank, sharerToClientId, preBuffering))

    // Create point(s) of coherence serialization
    val managerEndpoints = List.tabulate(nBanks){id => p(BuildL2CoherenceManager)(id, p)}
    managerEndpoints.flatMap(_.incoherent).foreach(_ := Bool(false))

    val mmioManager = Module(new MMIOTileLinkManager()(p.alterPartial({
        case TLId => "L1toL2"
        case InnerTLId => "L1toL2"
        case OuterTLId => "L2toMMIO"
      })))

    // Wire the tiles to the TileLink client ports of the L1toL2 network,
    // and coherence manager(s) to the other side
    l1tol2net.io.clients_cached <> tileList.map(_.io.cached).flatten
    l1tol2net.io.clients_uncached <> tileList.map(_.io.uncached).flatten ++ io.ext_clients
    l1tol2net.io.managers <> managerEndpoints.map(_.innerTL) :+ mmioManager.io.inner

    // Create a converter between TileLinkIO and MemIO for each channel
    val mem_ic = Module(new TileLinkMemoryInterconnect(nBanksPerMemChannel, nMemChannels)(outermostParams))

    val outerTLParams = p.alterPartial({ case TLId => "L2toMC" })
    val backendBuffering = TileLinkDepths(0,0,0,0,0)
    for ((bank, icPort) <- managerEndpoints zip mem_ic.io.in) {
      val unwrap = Module(new ClientTileLinkIOUnwrapper()(outerTLParams))
      unwrap.io.in <> ClientTileLinkEnqueuer(bank.outerTL, backendBuffering)(outerTLParams)
      TileLinkWidthAdapter(icPort, unwrap.io.out)
    }

    io.mem <> mem_ic.io.out

    buildMMIONetwork(ClientUncachedTileLinkEnqueuer(mmioManager.io.outer, 1))(
        p.alterPartial({case TLId => "L2toMMIO"}))
  }

  def makeBootROM()(implicit p: Parameters) = {
    val romdata = Files.readAllBytes(Paths.get(p(BootROMFile)))
    val rom = ByteBuffer.wrap(romdata)

    rom.order(ByteOrder.LITTLE_ENDIAN)

    // for now, have the reset vector jump straight to memory
    val memBase = (if (p(GlobalAddrMap) contains "mem") p(GlobalAddrMap)("mem") else p(GlobalAddrMap)("io:int:dmem0")).start
    val resetToMemDist = memBase - p(ResetVector)
    require(resetToMemDist == (resetToMemDist.toInt >> 12 << 12))
    val configStringAddr = p(ResetVector).toInt + rom.capacity

    require(rom.getInt(12) == 0,
      "Config string address position should not be occupied by code")
    rom.putInt(12, configStringAddr)
    rom.array() ++ p(ConfigString).toSeq
  }


  def buildMMIONetwork(mmio: ClientUncachedTileLinkIO)(implicit p: Parameters) = {
    val ioAddrMap = p(GlobalAddrMap).subMap("io")

    val mmioNetwork = Module(new TileLinkRecursiveInterconnect(1, ioAddrMap))
    mmioNetwork.io.in.head <> mmio

    val plic = Module(new PLIC(p(PLICKey)))
    plic.io.tl <> mmioNetwork.port("int:plic")
    for (i <- 0 until io.interrupts.size) {
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := io.interrupts(i)
      plic.io.devices(i) <> gateway.io.plic
    }

    val debugModule = Module(new DebugModule)
    debugModule.io.tl <> mmioNetwork.port("int:debug")
    debugModule.io.db <> io.debug

    val prci = Module(new PRCI)
    prci.io.tl <> mmioNetwork.port("int:prci")
    prci.io.rtcTick := io.rtcTick

    (prci.io.tiles, tileResets, tileList).zipped.foreach {
      case (prci, rst, tile) =>
        rst := reset
        tile.io.prci <> prci
    }

    for (i <- 0 until nTiles) {
      prci.io.interrupts(i).meip := plic.io.harts(plic.cfg.context(i, 'M'))
      if (p(UseVM))
        prci.io.interrupts(i).seip := plic.io.harts(plic.cfg.context(i, 'S'))
      prci.io.interrupts(i).debug := debugModule.io.debugInterrupts(i)
    }

    val tileSlavePorts = (0 until nTiles) map (i => s"int:dmem$i") filter (ioAddrMap contains _)
    for ((t, m) <- (tileList.map(_.io.slave).flatten) zip (tileSlavePorts map (mmioNetwork port _)))
      t <> ClientUncachedTileLinkEnqueuer(m, 1)

    val bootROM = Module(new ROMSlave(makeBootROM()))
    bootROM.io <> mmioNetwork.port("int:bootrom")

    io.mmio.foreach { _ <> mmioNetwork.port("ext") }
  }
}

class GroundTestCoreplex(topParams: Parameters) extends DefaultCoreplex(topParams) {
  override def hasSuccessFlag = true
  io.success.get := tileList.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
