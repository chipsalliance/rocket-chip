// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.bundlebridge._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.amba.axi4.{
  AXI4SubordinateNode, AXI4SubordinatePortParameters, AXI4SubordinateParameters, AXI4UserYanker, AXI4Buffer,
  AXI4Deinterleaver, AXI4IdIndexer, AXI4ManagerNode, AXI4ManagerPortParameters, AXI4ToTL,
  AXI4Fragmenter, AXI4ManagerParameters
}
import freechips.rocketchip.diplomacy.{
  AddressSet, RegionType, TransferSizes, IdRange, BufferParams
}
import freechips.rocketchip.resources.{
  MemoryDevice, SimpleBus
}
import freechips.rocketchip.tilelink.{
  TLXbar, RegionReplicator, ReplicatedRegion, TLWidthWidget, TLFilter, TLToAXI4, TLBuffer,
  TLFIFOFixer, TLManagerPortParameters, TLManagerNode, TLManagerParameters, TLClientNode,
  TLSourceShrinker, TLClientParameters, TLClientPortParameters
}
import freechips.rocketchip.util.StringToAugmentedString

import freechips.rocketchip.tilelink.TLClockDomainCrossing
import freechips.rocketchip.tilelink.TLResetDomainCrossing

/** Specifies the size and width of external memory ports */
case class ClientPortParams(
  base: BigInt,
  size: BigInt,
  beatBytes: Int,
  idBits: Int,
  maxXferBytes: Int = 256,
  executable: Boolean = true)

/** Specifies the width of external manager ports */
case class ManagerPortParams(beatBytes: Int, idBits: Int, sourceBits: Int)
// if incohBase is set, creates an incoherent alias for the region that hangs off the sbus
case class MemoryPortParams(client: ClientPortParams, nMemoryChannels: Int, incohBase: Option[BigInt] = None)

case object ExtMem extends Field[Option[MemoryPortParams]](None)
case object ExtBus extends Field[Option[ClientPortParams]](None)
case object ExtIn extends Field[Option[ManagerPortParams]](None)

///// The following traits add ports to the sytem, in some cases converting to different interconnect standards

/** Adds a port to the system intended to manager an AXI4 DRAM controller. */
trait CanHaveManagerAXI4MemPort { this: BaseSubsystem =>
  private val memPortParamsOpt = p(ExtMem)
  private val portName = "axi4"
  private val device = new MemoryDevice
  private val idBits = memPortParamsOpt.map(_.client.idBits).getOrElse(1)
  private val mbus = tlBusWrapperLocationMap.get(MBUS).getOrElse(viewpointBus)

  val memAXI4Node = AXI4SubordinateNode(memPortParamsOpt.map({ case MemoryPortParams(memPortParams, nMemoryChannels, _) =>
    Seq.tabulate(nMemoryChannels) { channel =>
      val base = AddressSet.misaligned(memPortParams.base, memPortParams.size)
      val filter = AddressSet(channel * mbus.blockBytes, ~((nMemoryChannels-1) * mbus.blockBytes))

      AXI4SubordinatePortParameters(
        subordinates = Seq(AXI4SubordinateParameters(
          address       = base.flatMap(_.intersect(filter)),
          resources     = device.reg,
          regionType    = RegionType.UNCACHED, // cacheable
          executable    = true,
          supportsWrite = TransferSizes(1, mbus.blockBytes),
          supportsRead  = TransferSizes(1, mbus.blockBytes),
          interleavedId = Some(0))), // manager does not interleave read responses
        beatBytes = memPortParams.beatBytes)
    }
  }).toList.flatten)

  for (i <- 0 until memAXI4Node.portParams.size) {
    val mem_bypass_xbar = mbus { TLXbar() }

    // Create an incoherent alias for the AXI4 memory
    memPortParamsOpt.foreach(memPortParams => {
      memPortParams.incohBase.foreach(incohBase => {
        val cohRegion = AddressSet(0, incohBase-1)
        val incohRegion = AddressSet(incohBase, incohBase-1)
        val replicator = tlBusWrapperLocationMap(p(TLManagerViewpointLocated(location))) {
          val replicator = LazyModule(new RegionReplicator(ReplicatedRegion(cohRegion, cohRegion.widen(incohBase))))
          val prefixSource = BundleBridgeSource[UInt](() => UInt(1.W))
          replicator.prefix := prefixSource
          // prefix is unused for TL uncached, so this is ok
          InModuleBody { prefixSource.bundle := 0.U(1.W) }
          replicator
        }
        viewpointBus.coupleTo(s"memory_controller_bypass_port_named_$portName") {
          (mbus.crossIn(mem_bypass_xbar)(ValName("bus_xing"))(p(SbusToMbusXTypeKey))
            := TLWidthWidget(viewpointBus.beatBytes)
            := replicator.node
            := TLFilter(TLFilter.mSubtract(cohRegion))
            := TLFilter(TLFilter.mResourceRemover)
            := _
          )
        }
      })
    })

    mbus.coupleTo(s"memory_controller_port_named_$portName") {
      (memAXI4Node
        := AXI4UserYanker()
        := AXI4IdIndexer(idBits)
        := TLToAXI4()
        := TLWidthWidget(mbus.beatBytes)
        := mem_bypass_xbar
        := _
      )
    }
  }

  val mem_axi4 = InModuleBody { memAXI4Node.makeIOs() }
}

/** Adds a AXI4 port to the system intended to client an MMIO device bus */
trait CanHaveManagerAXI4MMIOPort { this: BaseSubsystem =>
  private val mmioPortParamsOpt = p(ExtBus)
  private val portName = "mmio_port_axi4"
  private val device = new SimpleBus(portName.kebab, Nil)

  val mmioAXI4Node = AXI4SubordinateNode(
    mmioPortParamsOpt.map(params =>
      AXI4SubordinatePortParameters(
        subordinates = Seq(AXI4SubordinateParameters(
          address       = AddressSet.misaligned(params.base, params.size),
          resources     = device.ranges,
          executable    = params.executable,
          supportsWrite = TransferSizes(1, params.maxXferBytes),
          supportsRead  = TransferSizes(1, params.maxXferBytes))),
        beatBytes = params.beatBytes)).toSeq)

  mmioPortParamsOpt.map { params =>
    viewpointBus.coupleTo(s"port_named_$portName") {
      (mmioAXI4Node
        := AXI4Buffer()
        := AXI4UserYanker()
        := AXI4Deinterleaver(viewpointBus.blockBytes)
        := AXI4IdIndexer(params.idBits)
        := TLToAXI4()
        := TLWidthWidget(viewpointBus.beatBytes)
        := _)
    }
  }

  val mmio_axi4 = InModuleBody { mmioAXI4Node.makeIOs() }
}

/** Adds an AXI4 port to the system intended to be a manager on an MMIO device bus */
trait CanHaveSubordinateAXI4Port { this: BaseSubsystem =>
  private val subordinatePortParamsOpt = p(ExtIn)
  private val portName = "subordinate_port_axi4"
  private val fifoBits = 1
  private val fbus = tlBusWrapperLocationMap.get(FBUS).getOrElse(viewpointBus)

  val l2FrontendAXI4Node = AXI4ManagerNode(
    subordinatePortParamsOpt.map(params =>
      AXI4ManagerPortParameters(
        managers = Seq(AXI4ManagerParameters(
          name = portName.kebab,
          id   = IdRange(0, 1 << params.idBits))))).toSeq)

  subordinatePortParamsOpt.map { params =>
    fbus.coupleFrom(s"port_named_$portName") {
      ( _
        := TLBuffer(BufferParams.default)
        := TLFIFOFixer(TLFIFOFixer.all)
        := TLWidthWidget(params.beatBytes)
        := AXI4ToTL()
        := AXI4UserYanker(Some(1 << (params.sourceBits - fifoBits - 1)))
        := AXI4Fragmenter()
        := AXI4IdIndexer(fifoBits)
        := l2FrontendAXI4Node )
    }
  }

  val l2_frontend_bus_axi4 = InModuleBody { l2FrontendAXI4Node.makeIOs() }
}

/** Adds a TileLink port to the system intended to client an MMIO device bus */
trait CanHaveClientTLMMIOPort { this: BaseSubsystem =>
  private val mmioPortParamsOpt = p(ExtBus)
  private val portName = "mmio_port_tl"
  private val device = new SimpleBus(portName.kebab, Nil)

  val mmioTLNode = TLManagerNode(
    mmioPortParamsOpt.map(params =>
      TLManagerPortParameters.v1(
        managers = Seq(TLManagerParameters.v1(
          address            = AddressSet.misaligned(params.base, params.size),
          resources          = device.ranges,
          executable         = params.executable,
          supportsGet        = TransferSizes(1, viewpointBus.blockBytes),
          supportsPutFull    = TransferSizes(1, viewpointBus.blockBytes),
          supportsPutPartial = TransferSizes(1, viewpointBus.blockBytes))),
        beatBytes = params.beatBytes)).toSeq)

  mmioPortParamsOpt.map { params =>
    viewpointBus.coupleTo(s"port_named_$portName") {
      (mmioTLNode
        := TLBuffer()
        := TLSourceShrinker(1 << params.idBits)
        := TLWidthWidget(viewpointBus.beatBytes)
        := _ )
    }
  }

  val mmio_tl = InModuleBody {
    mmioTLNode.out.foreach { case (_, edge) => println(edge.prettySourceMapping(s"TL MMIO Port")) }
    mmioTLNode.makeIOs()
  }
}

/** Adds an TL port to the system intended to be a manager on an MMIO device bus.
  * NOTE: this port is NOT allowed to issue Acquires.
  */
trait CanHaveManagerTLPort { this: BaseSubsystem =>
  private val managerPortParamsOpt = p(ExtIn)
  private val portName = "manager_port_tl"

  val l2FrontendTLNode = TLClientNode(
    managerPortParamsOpt.map(params =>
      TLClientPortParameters.v1(
        clients = Seq(TLClientParameters.v1(
          name     = portName.kebab,
          sourceId = IdRange(0, 1 << params.idBits))))).toSeq)

  managerPortParamsOpt.map { params =>
    viewpointBus.coupleFrom(s"port_named_$portName") {
      ( _
        := TLSourceShrinker(1 << params.sourceBits)
        := TLWidthWidget(params.beatBytes)
        := l2FrontendTLNode )
    }
  }

  val l2_frontend_bus_tl = InModuleBody { l2FrontendTLNode.makeIOs() }
}
