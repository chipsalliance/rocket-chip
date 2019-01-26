// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

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

/** Specifies the width of external slave ports */
case class SlavePortParams(beatBytes: Int, idBits: Int, sourceBits: Int)
case class MemoryPortParams(master: MasterPortParams, nMemoryChannels: Int)

case object ExtMem extends Field[Option[MemoryPortParams]](None)
case object ExtBus extends Field[Option[MasterPortParams]](None)
case object ExtIn extends Field[Option[SlavePortParams]](None)

///// The following traits add ports to the sytem, in some cases converting to different interconnect standards

/** Adds a port to the system intended to master an AXI4 DRAM controller. */
trait CanHaveMasterAXI4MemPort { this: BaseSubsystem =>
  val module: CanHaveMasterAXI4MemPortModuleImp

  val memAXI4Node = p(ExtMem).map { case MemoryPortParams(memPortParams, nMemoryChannels) =>
    val portName = "axi4"
    val device = new MemoryDevice

    val memAXI4Node = AXI4SlaveNode(Seq.tabulate(nMemoryChannels) { channel =>
      val base = AddressSet(memPortParams.base, memPortParams.size-1)
      val filter = AddressSet(channel * mbus.blockBytes, ~((nMemoryChannels-1) * mbus.blockBytes))

      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = base.intersect(filter).toList,
          resources     = device.reg,
          regionType    = RegionType.UNCACHED, // cacheable
          executable    = true,
          supportsWrite = TransferSizes(1, mbus.blockBytes),
          supportsRead  = TransferSizes(1, mbus.blockBytes),
          interleavedId = Some(0))), // slave does not interleave read responses
        beatBytes = memPortParams.beatBytes)
    })

    memAXI4Node := mbus.toDRAMController(Some(portName)) {
      AXI4UserYanker() := AXI4IdIndexer(memPortParams.idBits) := TLToAXI4()
    }

    memAXI4Node
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveMasterAXI4MemPortModuleImp extends LazyModuleImp {
  val outer: CanHaveMasterAXI4MemPort

  val mem_axi4 = outer.memAXI4Node.map(x => IO(HeterogeneousBag.fromNode(x.in)))
  (mem_axi4 zip outer.memAXI4Node) foreach { case (io, node) =>
    (io zip node.in).foreach { case (io, (bundle, _)) => io <> bundle }
  }

  def connectSimAXIMem() {
    (mem_axi4 zip outer.memAXI4Node).foreach { case (io, node) =>
      (io zip node.in).foreach { case (io, (_, edge)) =>
        val mem = LazyModule(new SimAXIMem(edge, size = p(ExtMem).get.master.size))
        Module(mem.module).io.axi4.head <> io
      }
    }
  }
}

/** Adds a AXI4 port to the system intended to master an MMIO device bus */
trait CanHaveMasterAXI4MMIOPort { this: BaseSubsystem =>
  private val mmioPortParamsOpt = p(ExtBus)
  private val portName = "mmio_port_axi4"
  private val device = new SimpleBus(portName.kebab, Nil)

  val mmioAXI4Node = AXI4SlaveNode(
    mmioPortParamsOpt.map(params =>
      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = AddressSet.misaligned(params.base, params.size),
          resources     = device.ranges,
          executable    = params.executable,
          supportsWrite = TransferSizes(1, params.maxXferBytes),
          supportsRead  = TransferSizes(1, params.maxXferBytes))),
        beatBytes = params.beatBytes)).toSeq)

  mmioPortParamsOpt.map { params =>
    mmioAXI4Node := sbus.toFixedWidthPort(Some(portName)) {
      (AXI4Buffer()
        := AXI4UserYanker()
        := AXI4Deinterleaver(sbus.blockBytes)
        := AXI4IdIndexer(params.idBits)
        := TLToAXI4())
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveMasterAXI4MMIOPortModuleImp extends LazyModuleImp {
  val outer: CanHaveMasterAXI4MMIOPort
  val mmio_axi4 = IO(HeterogeneousBag.fromNode(outer.mmioAXI4Node.in))

  (mmio_axi4 zip outer.mmioAXI4Node.in) foreach { case (io, (bundle, _)) => io <> bundle }

  def connectSimAXIMMIO() {
    (mmio_axi4 zip outer.mmioAXI4Node.in) foreach { case (io, (_, edge)) =>
      val mmio_mem = LazyModule(new SimAXIMem(edge, size = 4096))
      Module(mmio_mem.module).io.axi4.head <> io
    }
  }
}

/** Adds an AXI4 port to the system intended to be a slave on an MMIO device bus */
trait CanHaveSlaveAXI4Port { this: BaseSubsystem =>
  private val slavePortParamsOpt = p(ExtIn)
  private val portName = "slave_port_axi4"
  private val fifoBits = 1

  val l2FrontendAXI4Node = AXI4MasterNode(
    slavePortParamsOpt.map(params =>
      AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
          name = portName.kebab,
          id   = IdRange(0, 1 << params.idBits))))).toSeq)

  slavePortParamsOpt.map { params =>
    fbus.fromPort(Some(portName), buffer = BufferParams.default) {
      (TLWidthWidget(params.beatBytes)
        := AXI4ToTL()
        := AXI4UserYanker(Some(1 << (params.sourceBits - fifoBits - 1)))
        := AXI4Fragmenter()
        := AXI4IdIndexer(fifoBits))
    } := l2FrontendAXI4Node
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveSlaveAXI4PortModuleImp extends LazyModuleImp {
  val outer: CanHaveSlaveAXI4Port
  val l2_frontend_bus_axi4 = IO(HeterogeneousBag.fromNode(outer.l2FrontendAXI4Node.out).flip)
  (outer.l2FrontendAXI4Node.out zip l2_frontend_bus_axi4) foreach { case ((bundle, _), io) => bundle <> io }
}

/** Adds a TileLink port to the system intended to master an MMIO device bus */
trait CanHaveMasterTLMMIOPort { this: BaseSubsystem =>
  private val mmioPortParamsOpt = p(ExtBus)
  private val portName = "mmio_port_tl"
  private val device = new SimpleBus(portName.kebab, Nil)

  val mmioTLNode = TLManagerNode(
    mmioPortParamsOpt.map(params =>
      TLManagerPortParameters(
        managers = Seq(TLManagerParameters(
          address            = AddressSet.misaligned(params.base, params.size),
          resources          = device.ranges,
          executable         = params.executable,
          supportsGet        = TransferSizes(1, sbus.blockBytes),
          supportsPutFull    = TransferSizes(1, sbus.blockBytes),
          supportsPutPartial = TransferSizes(1, sbus.blockBytes))),
        beatBytes = params.beatBytes)).toSeq)

  mmioPortParamsOpt.map { params =>
    mmioTLNode := sbus.toFixedWidthPort(Some(portName)) {
      TLBuffer() := TLSourceShrinker(1 << params.idBits)
    }
  }
}


/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveMasterTLMMIOPortModuleImp extends LazyModuleImp {
  val outer: CanHaveMasterTLMMIOPort
  val mmio_tl = IO(HeterogeneousBag.fromNode(outer.mmioTLNode.in))
  (mmio_tl zip outer.mmioTLNode.in) foreach { case (io, (bundle, _)) => io <> bundle }
}

/** Adds an TL port to the system intended to be a slave on an MMIO device bus.
  * NOTE: this port is NOT allowed to issue Acquires.
  */
trait CanHaveSlaveTLPort { this: BaseSubsystem =>
  private val slavePortParamsOpt = p(ExtIn)
  private val portName = "slave_port_tl"

  val l2FrontendTLNode = TLClientNode(
    slavePortParamsOpt.map(params =>
      TLClientPortParameters(
        clients = Seq(TLClientParameters(
          name     = portName.kebab,
          sourceId = IdRange(0, 1 << params.idBits))))).toSeq)

  slavePortParamsOpt.map { params =>
    sbus.fromPort(Some(portName)) {
      TLSourceShrinker(1 << params.sourceBits) := TLWidthWidget(params.beatBytes)
    } := l2FrontendTLNode
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveSlaveTLPortModuleImp extends LazyModuleImp {
  val outer: CanHaveSlaveTLPort
  val l2_frontend_bus_tl = IO(HeterogeneousBag.fromNode(outer.l2FrontendTLNode.out).flip)
  (outer.l2FrontendTLNode.out zip l2_frontend_bus_tl) foreach { case ((bundle, _), io) => bundle <> io }
}

/** Memory with AXI port for use in elaboratable test harnesses. */
class SimAXIMem(edge: AXI4EdgeParameters, size: BigInt)(implicit p: Parameters) extends LazyModule {
  val node = AXI4MasterNode(List(edge.master))

  val sram = LazyModule(new AXI4RAM(AddressSet(0, size-1), beatBytes = edge.bundle.dataBits/8))
  sram.node := AXI4Buffer() := AXI4Fragmenter() := node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle { val axi4 = HeterogeneousBag.fromNode(node.out).flip })
    (node.out zip io.axi4) foreach { case ((bundle, _), io) => bundle <> io }
  }
}
