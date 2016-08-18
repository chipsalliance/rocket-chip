// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.devices._
import uncore.util._
import uncore.converters._
import rocket._
import coreplex._

/** Top-level parameters of RocketChip, values set in e.g. PublicConfigs.scala */

/** Options for memory bus interface */
object BusType {
  sealed trait EnumVal
  case object AXI extends EnumVal
  case object AHB extends EnumVal
  case object TL  extends EnumVal
  val busTypes = Seq(AXI, AHB, TL)
}

/** Memory channel controls */
case object TMemoryChannels extends Field[BusType.EnumVal]
/** External MMIO controls */
case object NExtMMIOAXIChannels extends Field[Int]
case object NExtMMIOAHBChannels extends Field[Int]
case object NExtMMIOTLChannels  extends Field[Int]
/** External Bus controls */
case object NExtBusAXIChannels extends Field[Int]
/** Async configurations */
case object AsyncBusChannels extends Field[Boolean]
case object AsyncDebugBus extends Field[Boolean]
case object AsyncMemChannels extends Field[Boolean]
case object AsyncMMIOChannels extends Field[Boolean]
/** External address map settings */
case object ExtMMIOPorts extends Field[Seq[AddrMapEntry]]
case object ExtIOAddrMapEntries extends Field[Seq[AddrMapEntry]]
/** Function for building Coreplex */
case object BuildCoreplex extends Field[Parameters => Coreplex]
/** Function for connecting coreplex extra ports to top-level extra ports */
case object ConnectExtraPorts extends Field[(Bundle, Bundle, Parameters) => Unit]
/** Specifies the size of external memory */
case object ExtMemSize extends Field[Long]

/** Utility trait for quick access to some relevant parameters */
trait HasTopLevelParameters {
  implicit val p: Parameters
  lazy val tMemChannels = p(TMemoryChannels)
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val nMemAXIChannels = if (tMemChannels == BusType.AXI) nMemChannels else 0
  lazy val nMemAHBChannels = if (tMemChannels == BusType.AHB) nMemChannels else 0
  lazy val nMemTLChannels  = if (tMemChannels == BusType.TL)  nMemChannels else 0
  lazy val innerParams = p.alterPartial({ case TLId => "L1toL2" })
  lazy val outermostParams = p.alterPartial({ case TLId => "Outermost" })
  lazy val outermostMMIOParams = p.alterPartial({ case TLId => "MMIO_Outermost" })
  lazy val exportMMIO = p(ExportMMIOPort)
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
  val mem_clk = if (p(AsyncMemChannels)) Some(Vec(nMemChannels, Clock(INPUT))) else None
  val mem_rst = if (p(AsyncMemChannels)) Some(Vec(nMemChannels, Bool (INPUT))) else None
  val mem_axi = Vec(nMemAXIChannels, new NastiIO)
  val mem_ahb = Vec(nMemAHBChannels, new HastiMasterIO)
  val mem_tl  = Vec(nMemTLChannels,  new ClientUncachedTileLinkIO()(outermostParams))
  val interrupts = Vec(p(NExtInterrupts), Bool()).asInput
  val bus_clk = if (p(AsyncBusChannels)) Some(Vec(p(NExtBusAXIChannels), Clock(INPUT))) else None
  val bus_rst = if (p(AsyncBusChannels)) Some(Vec(p(NExtBusAXIChannels), Bool (INPUT))) else None
  val bus_axi = Vec(p(NExtBusAXIChannels), new NastiIO).flip
  val mmio_clk = if (p(AsyncMMIOChannels)) Some(Vec(p(NExtMMIOAXIChannels), Clock(INPUT))) else None
  val mmio_rst = if (p(AsyncMMIOChannels)) Some(Vec(p(NExtMMIOAXIChannels), Bool (INPUT))) else None
  val mmio_axi = Vec(p(NExtMMIOAXIChannels), new NastiIO)
  val mmio_ahb = Vec(p(NExtMMIOAHBChannels), new HastiMasterIO)
  val mmio_tl  = Vec(p(NExtMMIOTLChannels),  new ClientUncachedTileLinkIO()(outermostMMIOParams))
  val debug_clk = if (p(AsyncDebugBus)) Some(Clock(INPUT)) else None
  val debug_rst = if (p(AsyncDebugBus)) Some(Bool(INPUT)) else None
  val debug = new DebugBusIO()(p).flip
  val extra = p(ExtraTopPorts)(p)
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
}

/** Top-level module for the chip */
//TODO: Remove this wrapper once multichannel DRAM controller is provided
class Top(topParams: Parameters) extends Module with HasTopLevelParameters {
  implicit val p = topParams

  val coreplex = p(BuildCoreplex)(p)
  val periphery = Module(new Periphery()(innerParams))

  val io = new TopIO {
    val success = if (coreplex.hasSuccessFlag) Some(Bool(OUTPUT)) else None
  }
  io.success zip coreplex.io.success map { case (x, y) => x := y }

  if (exportMMIO) { periphery.io.mmio_in.get <> coreplex.io.mmio.get }
  periphery.io.mem_in <> coreplex.io.mem
  coreplex.io.ext_clients <> periphery.io.clients_out

  coreplex.io.debug <>
    (if (p(AsyncDebugBus))
      AsyncDebugBusFrom(io.debug_clk.get, io.debug_rst.get, io.debug)
    else io.debug)

  def asyncAxiTo(clocks: Seq[Clock], resets: Seq[Bool], inner_axis: Seq[NastiIO]): Seq[NastiIO] =
    (clocks, resets, inner_axis).zipped.map {
      case (clk, rst, in_axi) => AsyncNastiTo(clk, rst, in_axi)
    }

  def asyncAxiFrom(clocks: Seq[Clock], resets: Seq[Bool], outer_axis: Seq[NastiIO]): Seq[NastiIO] =
    (clocks, resets, outer_axis).zipped.map {
      case (clk, rst, out_axi) => AsyncNastiFrom(clk, rst, out_axi)
    }

  io.mmio_axi <>
    (if (p(AsyncMMIOChannels))
      asyncAxiTo(io.mmio_clk.get, io.mmio_rst.get, periphery.io.mmio_axi)
    else periphery.io.mmio_axi)
  io.mmio_ahb <> periphery.io.mmio_ahb
  io.mmio_tl <> periphery.io.mmio_tl

  io.mem_axi <>
    (if (p(AsyncMemChannels))
      asyncAxiTo(io.mem_clk.get, io.mem_rst.get, periphery.io.mem_axi)
    else periphery.io.mem_axi)
  io.mem_ahb <> periphery.io.mem_ahb
  io.mem_tl <> periphery.io.mem_tl

  periphery.io.bus_axi <>
    (if (p(AsyncBusChannels))
      asyncAxiFrom(io.bus_clk.get, io.bus_rst.get, io.bus_axi)
    else io.bus_axi)

  io.extra <> periphery.io.extra
  p(ConnectExtraPorts)(io.extra, coreplex.io.extra, p)
}

class Periphery(implicit val p: Parameters) extends Module
    with HasTopLevelParameters {
  val io = new Bundle {
    val mem_in  = Vec(nMemChannels, new ClientUncachedTileLinkIO()(outermostParams)).flip
    val clients_out = Vec(p(NExternalClients), new ClientUncachedTileLinkIO()(innerParams))
    val mmio_in = if (exportMMIO) Some(new ClientUncachedTileLinkIO()(outermostMMIOParams).flip) else None
    val mem_axi = Vec(nMemAXIChannels, new NastiIO)
    val mem_ahb = Vec(nMemAHBChannels, new HastiMasterIO)
    val mem_tl  = Vec(nMemTLChannels,  new ClientUncachedTileLinkIO()(outermostParams))
    val bus_axi = Vec(p(NExtBusAXIChannels), new NastiIO).flip
    val mmio_axi = Vec(p(NExtMMIOAXIChannels), new NastiIO)
    val mmio_ahb = Vec(p(NExtMMIOAHBChannels), new HastiMasterIO)
    val mmio_tl  = Vec(p(NExtMMIOTLChannels),  new ClientUncachedTileLinkIO()(outermostMMIOParams))
    val extra = p(ExtraTopPorts)(p)
  }

  require(io.clients_out.size <= 1)

  if (io.clients_out.size > 0) {
    val conv = Module(new TileLinkIONastiIOConverter)
    val arb = Module(new NastiArbiter(p(NExtBusAXIChannels)))
    arb.io.master <> io.bus_axi
    conv.io.nasti <> conv.io.tl
    io.clients_out.head <> conv.io.tl
  }

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
        require(false, "Unconnected external MMIO port")
      }
    }
  }

  def buildMMIONetwork(implicit p: Parameters) = {
    val extAddrMap = p(GlobalAddrMap).subMap("io:ext")

    val mmioNetwork = Module(new TileLinkRecursiveInterconnect(1, extAddrMap))
    mmioNetwork.io.in.head <> io.mmio_in.get

    for (device <- p(ExtraDevices)) {
      device.builder(mmioNetwork.port(device.addrMapEntry.name), io.extra, p)
    }

    val ext = p(ExtMMIOPorts).map(
      port => TileLinkWidthAdapter(mmioNetwork.port(port.name), "MMIO_Outermost"))
    connectExternalMMIO(ext)(outermostMMIOParams)
  }

  if (exportMMIO) {
    buildMMIONetwork(p.alterPartial({case TLId => "L2toMMIO"}))
  }

  for ((nasti, tl) <- io.mem_axi zip io.mem_in) {
    TopUtils.connectTilelinkNasti(nasti, tl)(outermostParams)
    // Memory cache type should be normal non-cacheable bufferable
    // TODO why is this happening here?  Would 0000 (device) be OK instead?
    nasti.ar.bits.cache := UInt("b0011")
    nasti.aw.bits.cache := UInt("b0011")
  }

  // Abuse the fact that zip takes the shorter of the two lists
  for ((ahb, tl) <- io.mem_ahb zip io.mem_in) {
    val bridge = Module(new AHBBridge(false)) // no atomics
    ahb <> bridge.io.ahb
    bridge.io.tl <> tl
  }

  for ((mem_tl, tl) <- io.mem_tl zip io.mem_in) {
    TopUtils.connectTilelink(mem_tl, tl)
  }
}
