// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util._

/** Specifies the size and width of external memory ports */
case class MasterPortParams(
  base: BigInt,
  size: BigInt,
  beatBytes: Int,
  idBits: Int,
  maxXferBytes: Int = 256,
  executable: Boolean = true)
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

  val mem_axi4 = AXI4SlaveNode(Seq.tabulate(nMemoryChannels) { channel =>
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

  val converter = LazyModule(new TLToAXI4())
  val trim = LazyModule(new AXI4IdIndexer(params.idBits))
  val yank = LazyModule(new AXI4UserYanker)
  val buffer = LazyModule(new AXI4Buffer)

  memBuses.map(_.toDRAMController).foreach { case node =>
    mem_axi4 := buffer.node := yank.node := trim.node := converter.node := node
  }
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasMasterAXI4MemPortBundle {
  implicit val p: Parameters
  val mem_axi4: HeterogeneousBag[AXI4Bundle]
  val nMemoryChannels: Int
  def connectSimAXIMem(dummy: Int = 1) = {
    if (nMemoryChannels > 0)  {
      val mem = LazyModule(new SimAXIMem(nMemoryChannels))
      Module(mem.module).io.axi4 <> mem_axi4
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasMasterAXI4MemPortModuleImp extends LazyModuleImp with HasMasterAXI4MemPortBundle {
  val outer: HasMasterAXI4MemPort
  val mem_axi4 = IO(HeterogeneousBag.fromNode(outer.mem_axi4.in))
  (mem_axi4 zip outer.mem_axi4.in) foreach { case (i, (o, _)) => i <> o }
  val nMemoryChannels = outer.nMemoryChannels
}

/** Adds a AXI4 port to the system intended to master an MMIO device bus */
trait HasMasterAXI4MMIOPort extends HasSystemBus {
  private val params = p(ExtBus)
  private val device = new SimpleBus("mmio", Nil)
  val mmio_axi4 = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address       = AddressSet.misaligned(params.base, params.size),
      resources     = device.ranges,
      executable    = params.executable,
      supportsWrite = TransferSizes(1, params.maxXferBytes),
      supportsRead  = TransferSizes(1, params.maxXferBytes))),
    beatBytes = params.beatBytes)))

  (mmio_axi4
    := AXI4Buffer()
    := AXI4UserYanker()
    := AXI4Deinterleaver(sbus.blockBytes)
    := AXI4IdIndexer(params.idBits)
    := TLToAXI4()
    := sbus.toFixedWidthPorts)
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasMasterAXI4MMIOPortBundle {
  implicit val p: Parameters
  val mmio_axi4: HeterogeneousBag[AXI4Bundle]
  def connectSimAXIMMIO(dummy: Int = 1) {
    val mmio_mem = LazyModule(new SimAXIMem(1, 4096))
    Module(mmio_mem.module).io.axi4 <> mmio_axi4
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasMasterAXI4MMIOPortModuleImp extends LazyModuleImp with HasMasterAXI4MMIOPortBundle {
  val outer: HasMasterAXI4MMIOPort
  val mmio_axi4 = IO(HeterogeneousBag.fromNode(outer.mmio_axi4.in))
  (mmio_axi4 zip outer.mmio_axi4.in) foreach { case (i, (o, _)) => i <> o }
}

/** Adds an AXI4 port to the system intended to be a slave on an MMIO device bus */
trait HasSlaveAXI4Port extends HasSystemBus {
  private val params = p(ExtIn)
  val l2FrontendAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "AXI4 periphery",
      id   = IdRange(0, 1 << params.idBits))))))

  private val fifoBits = 1
  (sbus.fromSyncPorts()
    := TLWidthWidget(params.beatBytes)
    := AXI4ToTL()
    := AXI4UserYanker(Some(1 << (params.sourceBits - fifoBits - 1)))
    := AXI4Fragmenter()
    := AXI4IdIndexer(fifoBits)
    := l2FrontendAXI4Node)
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
trait HasSlaveAXI4PortModuleImp extends LazyModuleImp with HasSlaveAXI4PortBundle {
  val outer: HasSlaveAXI4Port
  val l2_frontend_bus_axi4 = IO(HeterogeneousBag.fromNode(outer.l2FrontendAXI4Node.out).flip)
  (outer.l2FrontendAXI4Node.out zip l2_frontend_bus_axi4) foreach { case ((i, _), o) => i <> o }
}

/** Adds a TileLink port to the system intended to master an MMIO device bus */
trait HasMasterTLMMIOPort extends HasSystemBus {
  private val params = p(ExtBus)
  private val device = new SimpleBus("mmio", Nil)
  val mmio_tl = TLManagerNode(Seq(TLManagerPortParameters(
    managers = Seq(TLManagerParameters(
      address            = AddressSet.misaligned(params.base, params.size),
      resources          = device.ranges,
      executable         = params.executable,
      supportsGet        = TransferSizes(1, sbus.blockBytes),
      supportsPutFull    = TransferSizes(1, sbus.blockBytes),
      supportsPutPartial = TransferSizes(1, sbus.blockBytes))),
    beatBytes = params.beatBytes)))

  mmio_tl := TLBuffer() := TLSourceShrinker(1 << params.idBits) := sbus.toFixedWidthPorts
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
trait HasMasterTLMMIOPortModuleImp extends LazyModuleImp with HasMasterTLMMIOPortBundle {
  val outer: HasMasterTLMMIOPort
  val mmio_tl = IO(HeterogeneousBag.fromNode(outer.mmio_tl.in))
  (mmio_tl zip outer.mmio_tl.in) foreach { case (i, (o, _)) => i <> o }
}

/** Adds an TL port to the system intended to be a slave on an MMIO device bus.
  * NOTE: this port is NOT allowed to issue Acquires.
  */
trait HasSlaveTLPort extends HasSystemBus {
  private val params = p(ExtIn)
  val l2FrontendTLNode = TLClientNode(Seq(TLClientPortParameters(
    clients = Seq(TLClientParameters(
      name     = "Front Port (TL)",
      sourceId = IdRange(0, 1 << params.idBits))))))

  sbus.fromSyncPorts() := TLSourceShrinker(1 << params.sourceBits) := TLWidthWidget(params.beatBytes) := l2FrontendTLNode
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
trait HasSlaveTLPortModuleImp extends LazyModuleImp with HasSlaveTLPortBundle {
  val outer: HasSlaveTLPort
  val l2_frontend_bus_tl = IO(HeterogeneousBag.fromNode(outer.l2FrontendTLNode.out).flip)
  (outer.l2FrontendTLNode.in zip l2_frontend_bus_tl) foreach { case ((i, _), o) => i <> o }
}

/** Memory with AXI port for use in elaboratable test harnesses. */
class SimAXIMem(channels: Int, forceSize: BigInt = 0)(implicit p: Parameters) extends LazyModule {
  val config = p(ExtMem)
  val totalSize = if (forceSize > 0) forceSize else config.size
  val size = totalSize / channels
  require(totalSize % channels == 0)

  val node = AXI4MasterNode(Seq.fill(channels) {
    AXI4MasterPortParameters(Seq(AXI4MasterParameters(
      name = "dut",
      id   = IdRange(0, 1 << config.idBits)
    )))
  })

  for (i <- 0 until channels) {
    val sram = LazyModule(new AXI4RAM(AddressSet(0, size-1), beatBytes = config.beatBytes))
    sram.node := AXI4Buffer() := AXI4Fragmenter() := node
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val axi4 = HeterogeneousBag.fromNode(node.out).flip
    })
    (node.out zip io.axi4) foreach { case ((i, _), o) => i <> o }
  }
}
