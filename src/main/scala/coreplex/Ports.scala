// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util._

/** Specifies the size and width of external memory ports */
case class MasterPortParams(base: Long, size: Long, beatBytes: Int, idBits: Int)
case object ExtMem extends Field[MasterPortParams]
case object ExtBus extends Field[MasterPortParams]

/** Specifies the width of external slave ports */
case class SlavePortParams(beatBytes: Int, idBits: Int, sourceBits: Int)
case object ExtIn extends Field[SlavePortParams]

///// The following traits add ports to the sytem, in some cases converting to different interconnect standards

/** Adds a port to the system intended to master an AXI4 DRAM controller. */
trait HasMasterAXI4MemPort extends HasMemoryBus {
  val module: HasMasterAXI4MemPortModuleImp

  private val params = p(ExtMem)
  private val device = new MemoryDevice

  val mem_axi4 = AXI4BlindOutputNode(Seq.tabulate(nMemoryChannels) { channel =>
    val base = AddressSet(params.base, params.size-1)
    val filter = AddressSet(channel * cacheBlockBytes, ~((nMemoryChannels-1) * cacheBlockBytes))

    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address       = base.intersect(filter).toList,
        resources     = device.reg,
        regionType    = RegionType.UNCACHED,   // cacheable
        executable    = true,
        supportsWrite = TransferSizes(1, cacheBlockBytes),
        supportsRead  = TransferSizes(1, cacheBlockBytes),
        interleavedId = Some(0))),             // slave does not interleave read responses
      beatBytes = params.beatBytes)
  })

  val converter = LazyModule(new TLToAXI4(params.beatBytes))
  val trim = LazyModule(new AXI4IdIndexer(params.idBits))
  val yank = LazyModule(new AXI4UserYanker)
  val buffer = LazyModule(new AXI4Buffer)

  memBuses.map(_.outwardNode).foreach { case mbus =>
    converter.node := mbus
    trim.node := converter.node
    yank.node := trim.node
    buffer.node := yank.node
    mem_axi4 := buffer.node
  }
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasMasterAXI4MemPortBundle {
  implicit val p: Parameters
  val mem_axi4: HeterogeneousBag[AXI4Bundle]
  val nMemoryChannels: Int
  def connectSimAXIMem(dummy: Int = 1) = {
    if (nMemoryChannels > 0) Module(LazyModule(new SimAXIMem(nMemoryChannels)).module).io.axi4 <> mem_axi4
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasMasterAXI4MemPortModuleImp extends LazyMultiIOModuleImp with HasMasterAXI4MemPortBundle {
  val outer: HasMasterAXI4MemPort
  val mem_axi4 = IO(outer.mem_axi4.bundleOut)
  val nMemoryChannels = outer.nMemoryChannels
}

/** Adds a AXI4 port to the system intended to master an MMIO device bus */
trait HasMasterAXI4MMIOPort extends HasSystemBus {
  private val config = p(ExtBus)
  private val device = new SimpleBus("mmio", Nil)
  val mmio_axi4 = AXI4BlindOutputNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address       = List(AddressSet(BigInt(config.base), config.size-1)),
      resources     = device.ranges,
      executable    = true,                  // Can we run programs on this memory?
      supportsWrite = TransferSizes(1, 256), // The slave supports 1-256 byte transfers
      supportsRead  = TransferSizes(1, 256))),
    beatBytes = config.beatBytes)))

  mmio_axi4 :=
    AXI4Buffer()(
    AXI4UserYanker()(
    AXI4Deinterleaver(sbus.blockBytes)(
    AXI4IdIndexer(config.idBits)(
    TLToAXI4(config.beatBytes)(
    sbus.outwardWWNode)))))
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasMasterAXI4MMIOPortBundle {
  implicit val p: Parameters
  val mmio_axi4: HeterogeneousBag[AXI4Bundle]
  def connectSimAXIMMIO(dummy: Int = 1) {
    Module(LazyModule(new SimAXIMem(1, 4096)).module).io.axi4 <> mmio_axi4
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasMasterAXI4MMIOPortModuleImp extends LazyMultiIOModuleImp with HasMasterAXI4MMIOPortBundle {
  val outer: HasMasterAXI4MMIOPort
  val mmio_axi4 = IO(outer.mmio_axi4.bundleOut)
}

/** Adds an AXI4 port to the system intended to be a slave on an MMIO device bus */
trait HasSlaveAXI4Port extends HasSystemBus {
  private val config = p(ExtIn)
  val l2FrontendAXI4Node = AXI4BlindInputNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "AXI4 periphery",
      id   = IdRange(0, 1 << config.idBits))))))

  private val fifoBits = 1
  sbus.inwardFIFONode(TLFIFOFixer.all) :=
    TLWidthWidget(config.beatBytes)(
    AXI4ToTL()(
    AXI4UserYanker(Some(1 << (config.sourceBits - fifoBits - 1)))(
    AXI4Fragmenter()(
    AXI4IdIndexer(fifoBits)(
    l2FrontendAXI4Node)))))
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasSlaveAXI4PortBundle {
  implicit val p: Parameters
  val l2_frontend_bus_axi4: HeterogeneousBag[AXI4Bundle]
  def tieOffAXI4SlavePort(dummy: Int = 1) {
    l2_frontend_bus_axi4.foreach { l2_axi4 =>
      l2_axi4.ar.valid := Bool(false)
      l2_axi4.aw.valid := Bool(false)
      l2_axi4.w .valid := Bool(false)
      l2_axi4.r .ready := Bool(true)
      l2_axi4.b .ready := Bool(true)
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasSlaveAXI4PortModuleImp extends LazyMultiIOModuleImp with HasSlaveAXI4PortBundle {
  val outer: HasSlaveAXI4Port
  val l2_frontend_bus_axi4 = IO(outer.l2FrontendAXI4Node.bundleIn)
}

/** Adds a TileLink port to the system intended to master an MMIO device bus */
trait HasMasterTLMMIOPort extends HasSystemBus {
  private val params = p(ExtBus)
  private val device = new SimpleBus("mmio", Nil)
  val mmio_tl = TLBlindOutputNode(Seq(TLManagerPortParameters(
    managers = Seq(TLManagerParameters(
      address            = List(AddressSet(BigInt(params.base), params.size-1)),
      resources          = device.ranges,
      executable         = true,
      supportsGet        = TransferSizes(1, sbus.blockBytes),
      supportsPutFull    = TransferSizes(1, sbus.blockBytes),
      supportsPutPartial = TransferSizes(1, sbus.blockBytes))),
    beatBytes = params.beatBytes)))

  mmio_tl :=
    TLBuffer()(
    TLSourceShrinker(1 << params.idBits)(
    sbus.outwardWWNode))
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasMasterTLMMIOPortBundle {
  implicit val p: Parameters
  val mmio_tl: HeterogeneousBag[TLBundle]
  def tieOffTLMMIO(dummy: Int = 1) {
    mmio_tl.foreach { tl =>
      tl.a.ready := Bool(true)
      tl.b.valid := Bool(false)
      tl.c.ready := Bool(true)
      tl.d.valid := Bool(false)
      tl.e.ready := Bool(true)
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasMasterTLMMIOPortModuleImp extends LazyMultiIOModuleImp with HasMasterTLMMIOPortBundle {
  val outer: HasMasterTLMMIOPort
  val mmio_tl = IO(outer.mmio_tl.bundleOut)
}

/** Adds an TL port to the system intended to be a slave on an MMIO device bus.
  * NOTE: this port is NOT allowed to issue Acquires.
  */
trait HasSlaveTLPort extends HasSystemBus {
  private val params = p(ExtIn)
  val l2FrontendTLNode = TLBlindInputNode(Seq(TLClientPortParameters(
    clients = Seq(TLClientParameters(
      name     = "Front Port (TL)",
      sourceId = IdRange(0, 1 << params.idBits))))))

  sbus.inwardFIFONode :=
    TLSourceShrinker(1 << params.sourceBits)(
    TLWidthWidget(params.beatBytes)(
    l2FrontendTLNode))
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasSlaveTLPortBundle {
  implicit val p: Parameters
  val l2_frontend_bus_tl: HeterogeneousBag[TLBundle]
  def tieOffSlaveTLPort(dummy: Int = 1) {
    l2_frontend_bus_tl.foreach { tl =>
      tl.a.valid := Bool(false)
      tl.b.ready := Bool(true)
      tl.c.valid := Bool(false)
      tl.d.ready := Bool(true)
      tl.e.valid := Bool(false)
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasSlaveTLPortModuleImp extends LazyMultiIOModuleImp with HasSlaveTLPortBundle {
  val outer: HasSlaveTLPort
  val l2_frontend_bus_tl = IO(outer.l2FrontendTLNode.bundleIn)
}

/** Inter-System Port (ISP): 
  * Adds a pair of Asynchronous TL ports to the system, used to peer with other TL-based coreplexes
  */
trait HasISPPort extends HasSystemBus {
  val module: HasISPPortModule

  // TODO: use ChipLink instead of AsyncTileLink
  val isp_in = TLAsyncInputNode()
  val isp_out = TLAsyncOutputNode()

  private val out_xbar = LazyModule(new TLXbar)
  private val out_nums = LazyModule(new TLNodeNumberer)
  private val out_async = LazyModule(new TLAsyncCrossingSource)
  out_xbar.node :=* sbus.outwardSplitNode
  out_nums.node :*= out_xbar.node
  out_async.node :*= out_nums.node
  isp_out :*= out_async.node

  private val in_async = LazyModule(new TLAsyncCrossingSink)
  in_async.node :=* isp_in
  sbus.inwardBufNode :=* in_async.node
}

trait HasISPPortBundle {
  val isp_in: HeterogeneousBag[TLAsyncBundle]
  val isp_out: HeterogeneousBag[TLAsyncBundle]
}

trait HasISPPortModule extends LazyMultiIOModuleImp with HasISPPortBundle {
  val outer: HasISPPort
  val isp_in = IO(outer.isp_in.bundleIn)
  val isp_out = IO(outer.isp_out.bundleOut)
}

/** Memory with AXI port for use in elaboratable test harnesses. */
class SimAXIMem(channels: Int, forceSize: BigInt = 0)(implicit p: Parameters) extends LazyModule {
  val config = p(ExtMem)
  val totalSize = if (forceSize > 0) forceSize else BigInt(config.size)
  val size = totalSize / channels
  require(totalSize % channels == 0)

  val node = AXI4BlindInputNode(Seq.fill(channels) {
    AXI4MasterPortParameters(Seq(AXI4MasterParameters(
      name = "dut",
      id   = IdRange(0, 1 << config.idBits)
    )))
  })

  for (i <- 0 until channels) {
    val sram = LazyModule(new AXI4RAM(AddressSet(0, size-1), beatBytes = config.beatBytes))
    sram.node := AXI4Buffer()(AXI4Fragmenter()(node))
  }

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val axi4 = node.bundleIn
    }
  }
}
