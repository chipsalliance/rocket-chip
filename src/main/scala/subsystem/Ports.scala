// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.Field
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
  private val memPortParamsOpt = p(ExtMem)
  private val portName = "axi4"
  private val device = new MemoryDevice
  private val idBits = memPortParamsOpt.map(_.master.idBits).getOrElse(1)
  val memAXI4Node = AXI4SlaveNode(memPortParamsOpt.map({ case MemoryPortParams(memPortParams, nMemoryChannels) =>
    Seq.tabulate(nMemoryChannels) { channel =>
      val base = AddressSet.misaligned(memPortParams.base, memPortParams.size)
      val filter = AddressSet(channel * mbus.blockBytes, ~((nMemoryChannels-1) * mbus.blockBytes))

      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = base.flatMap(_.intersect(filter)),
          resources     = device.reg,
          regionType    = RegionType.UNCACHED, // cacheable
          executable    = true,
          supportsWrite = TransferSizes(1, mbus.blockBytes),
          supportsRead  = TransferSizes(1, mbus.blockBytes),
          interleavedId = Some(0))), // slave does not interleave read responses
        beatBytes = memPortParams.beatBytes)
    }
  }).toList.flatten)

  mbus.coupleTo(s"memory_controller_port_named_$portName") {
    (memAXI4Node
      :*= AXI4UserYanker()
      :*= AXI4IdIndexer(idBits)
      :*= TLToAXI4()
      :*= TLWidthWidget(mbus.beatBytes)
      :*= _)
  }

  val mem_axi4 = InModuleBody { memAXI4Node.makeIOs() }
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
    sbus.coupleTo(s"port_named_$portName") {
      (mmioAXI4Node
        := AXI4Buffer()
        := AXI4UserYanker()
        := AXI4Deinterleaver(sbus.blockBytes)
        := AXI4IdIndexer(params.idBits)
        := TLToAXI4()
        := TLWidthWidget(sbus.beatBytes)
        := _)
    }
  }

  val mmio_axi4 = InModuleBody { mmioAXI4Node.makeIOs() }
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

/** Adds a TileLink port to the system intended to master an MMIO device bus */
trait CanHaveMasterTLMMIOPort { this: BaseSubsystem =>
  private val mmioPortParamsOpt = p(ExtBus)
  private val portName = "mmio_port_tl"
  private val device = new SimpleBus(portName.kebab, Nil)

  val mmioTLNode = TLManagerNode(
    mmioPortParamsOpt.map(params =>
      TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          address            = AddressSet.misaligned(params.base, params.size),
          resources          = device.ranges,
          executable         = params.executable,
          supportsGet        = TransferSizes(1, sbus.blockBytes),
          supportsPutFull    = TransferSizes(1, sbus.blockBytes),
          supportsPutPartial = TransferSizes(1, sbus.blockBytes))),
        beatBytes = params.beatBytes)).toSeq)

  mmioPortParamsOpt.map { params =>
    sbus.coupleTo(s"port_named_$portName") {
      (mmioTLNode
        := TLBuffer()
        := TLSourceShrinker(1 << params.idBits)
        := TLWidthWidget(sbus.beatBytes)
        := _ )
    }
  }

  val mmio_tl = InModuleBody {
    mmioTLNode.out.foreach { case (_, edge) => println(edge.prettySourceMapping(s"TL MMIO Port")) }
    mmioTLNode.makeIOs()
  }
}

/** Adds an TL port to the system intended to be a slave on an MMIO device bus.
  * NOTE: this port is NOT allowed to issue Acquires.
  */
trait CanHaveSlaveTLPort { this: BaseSubsystem =>
  private val slavePortParamsOpt = p(ExtIn)
  private val portName = "slave_port_tl"

  val l2FrontendTLNode = TLClientNode(
    slavePortParamsOpt.map(params =>
      TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          name     = portName.kebab,
          sourceId = IdRange(0, 1 << params.idBits))))).toSeq)

  slavePortParamsOpt.map { params =>
    sbus.coupleFrom(s"port_named_$portName") {
      ( _
        := TLSourceShrinker(1 << params.sourceBits)
        := TLWidthWidget(params.beatBytes)
        := l2FrontendTLNode )
    }
  }

  val l2_frontend_bus_tl = InModuleBody { l2FrontendTLNode.makeIOs() }
}
