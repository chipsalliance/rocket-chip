
// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.subsystem

import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.debug.{DebugModuleKey, DefaultDebugModuleParams, ExportDebug, JTAG, APB}
import freechips.rocketchip.devices.tilelink.{
  BuiltInErrorDeviceParams, BootROMLocated, BootROMParams, CLINTKey, DevNullDevice, CLINTParams, PLICKey, PLICParams, DevNullParams
}
import freechips.rocketchip.prci.{SynchronousCrossing, AsynchronousCrossing, RationalCrossing, ClockCrossingType}
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.diplomacy.nodes.MonitorsEnabled
import freechips.rocketchip.resources.{
  DTSModel, DTSCompat, DTSTimebase, BigIntHexContext
}
import freechips.rocketchip.tile.{
  MaxHartIdBits, RocketTileParams, BuildRoCC, AccumulatorExample, OpcodeSet, TranslatorExample, CharacterCountExample, BlackBoxExample
}
import freechips.rocketchip.util.{ClockGateModelFile, SystemFileName}
import scala.reflect.ClassTag

case object MaxXLen extends Field[Int]

class BaseSubsystemConfig extends Config ((site, here, up) => {
  // Tile parameters
  case MaxXLen => (site(PossibleTileLocations).flatMap(loc => site(TilesLocated(loc)))
    .map(_.tileParams.core.xLen) :+ 32).max
  case MaxHartIdBits => log2Up((site(PossibleTileLocations).flatMap(loc => site(TilesLocated(loc)))
      .map(_.tileParams.tileId) :+ 0).max+1)
  // Interconnect parameters
  case SystemBusKey => SystemBusParams(
    beatBytes = 8,
    blockBytes = site(CacheBlockBytes))
  case ControlBusKey => PeripheryBusParams(
    beatBytes = 8,
    blockBytes = site(CacheBlockBytes),
    dtsFrequency = Some(100000000), // Default to 100 MHz cbus clock
    errorDevice = Some(BuiltInErrorDeviceParams(
      errorParams = DevNullParams(List(AddressSet(0x3000, 0xfff)), maxAtomic=8, maxTransfer=4096))))
  case PeripheryBusKey => PeripheryBusParams(
    beatBytes = 8,
    blockBytes = site(CacheBlockBytes),
    dtsFrequency = Some(100000000)) // Default to 100 MHz pbus clock
  case MemoryBusKey => MemoryBusParams(
    beatBytes = 8,
    blockBytes = site(CacheBlockBytes))
  case FrontBusKey => FrontBusParams(
    beatBytes = 8,
    blockBytes = site(CacheBlockBytes))
  // Additional device Parameters
  case BootROMLocated(InSubsystem) => Seq(BootROMParams(contentFileName = SystemFileName("./bootrom/bootrom.img")))
  case HasTilesExternalResetVectorKey => false
  case DebugModuleKey => Some(DefaultDebugModuleParams(64))
  case CLINTKey => Some(CLINTParams())
  case PLICKey => Some(PLICParams())
  case TilesLocated(InSubsystem) => Nil
  case PossibleTileLocations => Seq(InSubsystem)
})

/* Composable partial function Configs to set individual parameters */

class WithJustOneBus extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => List(
    JustOneBusTopologyParams(sbus = site(SystemBusKey))
  )
})

class WithIncoherentBusTopology extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => List(
    JustOneBusTopologyParams(sbus = site(SystemBusKey)),
    HierarchicalBusTopologyParams(
      pbus = site(PeripheryBusKey),
      fbus = site(FrontBusKey),
      cbus = site(ControlBusKey),
      xTypes = SubsystemCrossingParams(
        sbusToCbusXType = site(SbusToCbusXTypeKey),
        cbusToPbusXType = site(CbusToPbusXTypeKey),
        fbusToSbusXType = site(FbusToSbusXTypeKey)),
      driveClocksFromSBus = site(DriveClocksFromSBus)))
})

class WithCoherentBusTopology extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => List(
    JustOneBusTopologyParams(sbus = site(SystemBusKey)),
    HierarchicalBusTopologyParams(
      pbus = site(PeripheryBusKey),
      fbus = site(FrontBusKey),
      cbus = site(ControlBusKey),
      xTypes = SubsystemCrossingParams(
        sbusToCbusXType = site(SbusToCbusXTypeKey),
        cbusToPbusXType = site(CbusToPbusXTypeKey),
        fbusToSbusXType = site(FbusToSbusXTypeKey)),
      driveClocksFromSBus = site(DriveClocksFromSBus)),
    CoherentBusTopologyParams(
      mbus = site(MemoryBusKey),
      coherence = site(SubsystemBankedCoherenceKey),
      sbusToMbusXType = site(SbusToMbusXTypeKey),
      driveMBusClockFromSBus = site(DriveClocksFromSBus)))
})

class WithCluster(
  clusterId: Int,
  location: HierarchicalLocation = InSubsystem,
  crossing: RocketCrossingParams = RocketCrossingParams() // TODO make this not rocket
) extends Config((site, here, up) => {
  case ClustersLocated(`location`) => up(ClustersLocated(location)) :+ ClusterAttachParams(
    ClusterParams(clusterId = clusterId),
    crossing)
  case TLNetworkTopologyLocated(InCluster(`clusterId`)) => List(
    ClusterBusTopologyParams(
      clusterId = clusterId,
      csbus = site(SystemBusKey),
      ccbus = site(ControlBusKey).copy(errorDevice = None),
      coherence = site(ClusterBankedCoherenceKey(clusterId))
    )
  )
  case PossibleTileLocations => up(PossibleTileLocations) :+ InCluster(clusterId)
})

class WithClusterBanks(clusterId: Int, nBanks: Int = 1) extends Config((site, here, up) => {
  case ClusterBankedCoherenceKey(`clusterId`) => up(ClusterBankedCoherenceKey(clusterId)).copy(nBanks=nBanks)
})

class WithNBanks(n: Int) extends Config((site, here, up) => {
  case SubsystemBankedCoherenceKey => up(SubsystemBankedCoherenceKey).copy(nBanks = n)
})

class WithNTrackersPerBank(n: Int) extends Config((site, here, up) => {
  case BroadcastKey => up(BroadcastKey).copy(nTrackers = n)
})

class WithCacheBlockBytes(linesize: Int) extends Config((site, here, up) => {
  case CacheBlockBytes => linesize
})

class WithBufferlessBroadcastHub extends Config((site, here, up) => {
  case BroadcastKey => up(BroadcastKey).copy(bufferless = true)
})

class TileAttachConfig[T <: CanAttachTile](f: T => T, locationOpt: Option[HierarchicalLocation], tileIdOpt: Seq[Int])(implicit tag: ClassTag[T])
  extends Config((site, here, up) => {
    val partialFn: PartialFunction[CanAttachTile, CanAttachTile] = { case tp: T => if (tileIdOpt.contains(tp.tileParams.tileId) || tileIdOpt.isEmpty) f(tp) else tp }
    val alterFn: CanAttachTile => CanAttachTile = x => partialFn.applyOrElse(x, identity[CanAttachTile])
    locationOpt match {
      case Some(loc) => { case TilesLocated(`loc`) => up(TilesLocated(loc)) map { alterFn(_) } }
      case None      => { case TilesLocated(loc)   => up(TilesLocated(loc)) map { alterFn(_) } }
    }
  }) {
  // The default constructor applies the modification to all locations
  def this(f: T => T)(implicit tag: ClassTag[T]) = this(f, None, Nil)
  // The atLocation method applies the modification to only the provided location
  def atLocation(loc: HierarchicalLocation) = new TileAttachConfig(f, Some(loc), tileIdOpt)
  // The atTileIds method applies the modification only to specified tileIds
  def atTileIds(ids: Int*) = new TileAttachConfig(f, locationOpt, tileIdOpt ++ ids)
}

class WithRoccExample extends Config((site, here, up) => {
  case BuildRoCC => List(
    (p: Parameters) => {
        val accumulator = LazyModule(new AccumulatorExample(OpcodeSet.custom0, n = 4)(p))
        accumulator
    },
    (p: Parameters) => {
        val translator = LazyModule(new TranslatorExample(OpcodeSet.custom1)(p))
        translator
    },
    (p: Parameters) => {
        val counter = LazyModule(new CharacterCountExample(OpcodeSet.custom2)(p))
        counter
    },
    (p: Parameters) => {
      val blackbox = LazyModule(new BlackBoxExample(OpcodeSet.custom3, "RoccBlackBox")(p))
      blackbox
    })
})

/**
 * WARNING!!! IGNORE AT YOUR OWN PERIL!!!
 *
 * There is a very restrictive set of conditions under which the stateless
 * bridge will function properly. There can only be a single tile. This tile
 * MUST use the blocking data cache (L1D_MSHRS == 0) and MUST NOT have an
 * uncached channel capable of writes (i.e. a RoCC accelerator).
 *
 * This is because the stateless bridge CANNOT generate probes, so if your
 * system depends on coherence between channels in any way,
 * DO NOT use this configuration.
 */
class WithIncoherentTiles extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location)) map {
    case tp: RocketTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      master = tp.crossingParams.master match {
        case x: HierarchicalElementMasterPortParams => x.copy(cork = Some(true))
        case _ => throw new Exception("Unrecognized type for RocketCrossingParams.master")
      }))
    case t => t
  }
  case SubsystemBankedCoherenceKey => up(SubsystemBankedCoherenceKey).copy(
    coherenceManager = CoherenceManagerWrapper.incoherentManager
  )
})

class WithBootROMFile(bootROMFile: String) extends Config((site, here, up) => {
  case BootROMLocated(x) => up(BootROMLocated(x)).map(_.copy(contentFileName=SystemFileName(bootROMFile)))
})

class WithClockGateModel(file: String = "/vsrc/EICG_wrapper.v") extends Config((site, here, up) => {
  case ClockGateModelFile => Some(file)
})

class WithEdgeDataBits(dataBits: Int) extends Config((site, here, up) => {
  case MemoryBusKey => up(MemoryBusKey).copy(beatBytes = dataBits/8)
  case ExtIn => up(ExtIn).map(_.copy(beatBytes = dataBits/8))
})

class WithJtagDTM extends Config ((site, here, up) => {
  case ExportDebug => up(ExportDebug).copy(protocols = Set(JTAG))
})

class WithDebugAPB extends Config ((site, here, up) => {
  case ExportDebug => up(ExportDebug).copy(protocols = Set(APB))
})


class WithDebugSBA extends Config ((site, here, up) => {
  case DebugModuleKey => up(DebugModuleKey).map(_.copy(hasBusMaster = true))
})

class WithNBitPeripheryBus(nBits: Int) extends Config ((site, here, up) => {
  case PeripheryBusKey => up(PeripheryBusKey).copy(beatBytes = nBits/8)
})

class WithoutTLMonitors extends Config ((site, here, up) => {
  case MonitorsEnabled => false
})

class WithNExtTopInterrupts(nExtInts: Int) extends Config((site, here, up) => {
  case NExtTopInterrupts => nExtInts
})

class WithNMemoryChannels(n: Int) extends Config((site, here, up) => {
  case ExtMem => up(ExtMem).map(_.copy(nMemoryChannels = n))
})

class WithExtMemSize(n: BigInt) extends Config((site, here, up) => {
  case ExtMem => up(ExtMem).map(x => x.copy(master = x.master.copy(size = n)))
})

class WithExtMemSbusBypass(base: BigInt = x"10_0000_0000") extends Config((site, here, up) => {
  case ExtMem => up(ExtMem).map(x => x.copy(incohBase = Some(base)))
})

class WithDTS(model: String, compat: Seq[String]) extends Config((site, here, up) => {
  case DTSModel => model
  case DTSCompat => compat
})

class WithTimebase(hertz: BigInt) extends Config((site, here, up) => {
  case DTSTimebase => hertz
})

class WithDefaultMemPort extends Config((site, here, up) => {
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
                      base = x"8000_0000",
                      size = x"1000_0000",
                      beatBytes = site(MemoryBusKey).beatBytes,
                      idBits = 4), 1))
})

class WithCustomMemPort (base_addr: BigInt, base_size: BigInt, data_width: Int, id_bits: Int, maxXferBytes: Int) extends Config((site, here, up) => {
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
                      base = base_addr,
                      size = base_size,
                      beatBytes = data_width/8,
                      idBits = id_bits, 
                      maxXferBytes = maxXferBytes), 1))
})

class WithNoMemPort extends Config((site, here, up) => {
  case ExtMem => None
})

class WithDefaultMMIOPort extends Config((site, here, up) => {
  case ExtBus => Some(MasterPortParams(
                      base = x"6000_0000",
                      size = x"2000_0000",
                      beatBytes = site(MemoryBusKey).beatBytes,
                      idBits = 4))
})

class WithCustomMMIOPort (base_addr: BigInt, base_size: BigInt, data_width: Int, id_bits: Int, maxXferBytes: Int) extends Config((site, here, up) => {
  case ExtBus => Some(MasterPortParams(
                      base = base_addr,
                      size = base_size,
                      beatBytes = data_width/8,
                      idBits = id_bits, 
                      maxXferBytes = maxXferBytes))
})

class WithNoMMIOPort extends Config((site, here, up) => {
  case ExtBus => None
})

class WithDefaultSlavePort extends Config((site, here, up) => {
  case ExtIn  => Some(SlavePortParams(beatBytes = 8, idBits = 8, sourceBits = 4))
})

class WithCustomSlavePort (data_width: Int, id_bits: Int) extends Config((site, here, up) => {
  case ExtIn  => Some(SlavePortParams(beatBytes = data_width/8, idBits = id_bits, sourceBits = 4))
})

class WithNoSlavePort extends Config((site, here, up) => {
  case ExtIn => None
})

/**
  * Mixins to specify crossing types between the 5 traditional TL buses
  *
  * Note: these presuppose the legacy connections between buses and set
  * parameters in SubsystemCrossingParams; they may not be resuable in custom
  * topologies (but you can specify the desired crossings in your topology).
  *
  * @param xType The clock crossing type
  */

class WithSbusToMbusCrossingType(xType: ClockCrossingType) extends Config((site, here, up) => {
  case SbusToMbusXTypeKey => xType
})
class WithSbusToCbusCrossingType(xType: ClockCrossingType) extends Config((site, here, up) => {
  case SbusToCbusXTypeKey => xType
})
class WithCbusToPbusCrossingType(xType: ClockCrossingType) extends Config((site, here, up) => {
  case CbusToPbusXTypeKey => xType
})
class WithFbusToSbusCrossingType(xType: ClockCrossingType) extends Config((site, here, up) => {
  case FbusToSbusXTypeKey => xType
})

/**
  * Mixins to set the dtsFrequency field of BusParams -- these will percolate its way
  * up the diplomatic graph to the clock sources.
  */
class WithPeripheryBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case PeripheryBusKey => up(PeripheryBusKey).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})
class WithMemoryBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case MemoryBusKey => up(MemoryBusKey).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})
class WithSystemBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case SystemBusKey => up(SystemBusKey).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})
class WithFrontBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case FrontBusKey => up(FrontBusKey).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})
class WithControlBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case ControlBusKey => up(ControlBusKey).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})

/** Under the default multi-bus topologies, this leaves bus ClockSinks undriven by the topology itself */
class WithDontDriveBusClocksFromSBus extends Config((site, here, up) => {
  case DriveClocksFromSBus => false
})

class WithCloneCluster(
  clusterId: Int,
  cloneClusterId: Int = 0,
  location: HierarchicalLocation = InSubsystem,
  cloneLocation: HierarchicalLocation = InSubsystem
) extends Config((site, here, up) => {
  case ClustersLocated(`location`) => {
    val prev = up(ClustersLocated(location))
    val clusterAttachParams = up(ClustersLocated(cloneLocation)).find(_.clusterParams.clusterId == cloneClusterId)
      .get.asInstanceOf[ClusterAttachParams]
    prev :+ CloneClusterAttachParams(
      cloneClusterId,
      clusterAttachParams.copy(clusterParams = clusterAttachParams.clusterParams.copy(clusterId = clusterId))
    )
  }
  case TLNetworkTopologyLocated(InCluster(`clusterId`)) => site(TLNetworkTopologyLocated(InCluster(cloneClusterId)))
  case PossibleTileLocations => up(PossibleTileLocations) :+ InCluster(clusterId)
})
