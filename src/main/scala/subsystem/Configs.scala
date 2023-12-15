// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.subsystem

import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

class BaseSubsystemConfig extends Config ((site, here, up) => {
  // Tile parameters
  case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
  case XLen => 64 // Applies to all cores
  case MaxHartIdBits => log2Up((site(PossibleTileLocations).flatMap(loc => site(TilesLocated(loc)))
      .map(_.tileParams.tileId) :+ 0).max+1)
  // Interconnect parameters
  case SystemBusKey => SystemBusParams(
    beatBytes = site(XLen)/8,
    blockBytes = site(CacheBlockBytes))
  case ControlBusKey => PeripheryBusParams(
    beatBytes = site(XLen)/8,
    blockBytes = site(CacheBlockBytes),
    dtsFrequency = Some(100000000), // Default to 100 MHz cbus clock
    errorDevice = Some(BuiltInErrorDeviceParams(
      errorParams = DevNullParams(List(AddressSet(0x3000, 0xfff)), maxAtomic=site(XLen)/8, maxTransfer=4096))))
  case PeripheryBusKey => PeripheryBusParams(
    beatBytes = site(XLen)/8,
    blockBytes = site(CacheBlockBytes),
    dtsFrequency = Some(100000000)) // Default to 100 MHz pbus clock
  case MemoryBusKey => MemoryBusParams(
    beatBytes = site(XLen)/8,
    blockBytes = site(CacheBlockBytes))
  case FrontBusKey => FrontBusParams(
    beatBytes = site(XLen)/8,
    blockBytes = site(CacheBlockBytes))
  // Additional device Parameters
  case BootROMLocated(InSubsystem) => Some(BootROMParams(contentFileName = "./bootrom/bootrom.img"))
  case HasTilesExternalResetVectorKey => false
  case DebugModuleKey => Some(DefaultDebugModuleParams(site(XLen)))
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

class WithNBigCores(
  n: Int,
  location: HierarchicalLocation,
  crossing: RocketCrossingParams,
) extends Config((site, here, up) => {
  case TilesLocated(`location`) => {
    val prev = up(TilesLocated(`location`), site)
    val idOffset = up(NumTiles)
    val big = RocketTileParams(
      core   = RocketCoreParams(mulDiv = Some(MulDivParams(
        mulUnroll = 8,
        mulEarlyOut = true,
        divEarlyOut = true))),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        blockBytes = site(CacheBlockBytes))))
    List.tabulate(n)(i => RocketTileAttachParams(
      big.copy(tileId = i + idOffset),
      crossing
    )) ++ prev
  }
  case NumTiles => up(NumTiles) + n
}) {
  def this(n: Int, location: HierarchicalLocation = InSubsystem) = this(n, location, RocketCrossingParams(
    master = HierarchicalElementMasterPortParams.locationDefault(location),
    slave = HierarchicalElementSlavePortParams.locationDefault(location),
    mmioBaseAddressPrefixWhere = location match {
      case InSubsystem => CBUS
      case InCluster(clusterId) => CCBUS(clusterId)
    }
  ))
}



class WithNMedCores(
  n: Int,
  crossing: RocketCrossingParams = RocketCrossingParams(),
) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => {
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = up(NumTiles)
    val med = RocketTileParams(
      core = RocketCoreParams(fpu = None),
      btb = None,
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 1,
        nTLBSets = 1,
        nTLBWays = 4,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 1,
        nTLBSets = 1,
        nTLBWays = 4,
        blockBytes = site(CacheBlockBytes))))
    List.tabulate(n)(i => RocketTileAttachParams(
      med.copy(tileId = i + idOffset),
      crossing
    )) ++ prev
  }
  case NumTiles => up(NumTiles) + n
})

class WithNSmallCores(
  n: Int,
  crossing: RocketCrossingParams = RocketCrossingParams()
) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => {
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = up(NumTiles)
    val small = RocketTileParams(
      core = RocketCoreParams(useVM = false, fpu = None),
      btb = None,
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 1,
        nTLBSets = 1,
        nTLBWays = 4,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 1,
        nTLBSets = 1,
        nTLBWays = 4,
        blockBytes = site(CacheBlockBytes))))
    List.tabulate(n)(i => RocketTileAttachParams(
      small.copy(tileId = i + idOffset),
      crossing
    )) ++ prev
  }
  case NumTiles => up(NumTiles) + n
})

class With1TinyCore extends Config((site, here, up) => {
  case XLen => 32
  case TilesLocated(InSubsystem) => {
    val tiny = RocketTileParams(
      core = RocketCoreParams(
        useVM = false,
        fpu = None,
        mulDiv = Some(MulDivParams(mulUnroll = 8))),
      btb = None,
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 256, // 16Kb scratchpad
        nWays = 1,
        nTLBSets = 1,
        nTLBWays = 4,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes),
        scratch = Some(0x80000000L))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 1,
        nTLBSets = 1,
        nTLBWays = 4,
        blockBytes = site(CacheBlockBytes)))
    )
    List(RocketTileAttachParams(
      tiny,
      RocketCrossingParams(
        crossingType = SynchronousCrossing(),
        master = HierarchicalElementMasterPortParams())
    ))
  }
  case NumTiles => 1
  case ClustersLocated(_) => Nil
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
  case SubsystemBankedCoherenceKey => up(SubsystemBankedCoherenceKey, site).copy(nBanks = n)
})

class WithNTrackersPerBank(n: Int) extends Config((site, here, up) => {
  case BroadcastKey => up(BroadcastKey, site).copy(nTrackers = n)
})

// This is the number of icache sets for all Rocket tiles
class WithL1ICacheSets(sets: Int) extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(nSets = sets))))
    case t => t
  }
})

// This is the number of icache sets for all Rocket tiles
class WithL1DCacheSets(sets: Int) extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(nSets = sets))))
    case t => t
  }
})

class WithL1ICacheWays(ways: Int) extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(nWays = ways))))
    case t => t
  }
})

class WithL1DCacheWays(ways: Int) extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(nWays = ways))))
    case t => t
  }
})


class WithRocketCacheRowBits(n: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(rowBits = n)),
      icache = tp.tileParams.icache.map(_.copy(rowBits = n))))
    case t => t
  }
})

class WithCacheBlockBytes(linesize: Int) extends Config((site, here, up) => {
  case CacheBlockBytes => linesize
})

class WithBufferlessBroadcastHub extends Config((site, here, up) => {
  case BroadcastKey => up(BroadcastKey, site).copy(bufferless = true)
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
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      master = tp.crossingParams.master match {
        case x: HierarchicalElementMasterPortParams => x.copy(cork = Some(true))
        case _ => throw new Exception("Unrecognized type for RocketCrossingParams.master")
      }))
    case t => t
  }
  case SubsystemBankedCoherenceKey => up(SubsystemBankedCoherenceKey, site).copy(
    coherenceManager = CoherenceManagerWrapper.incoherentManager
  )
})

class WithRV32 extends Config((site, here, up) => {
  case XLen => 32
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(
        fpu = tp.tileParams.core.fpu.map(_.copy(fLen = 32)),
        mulDiv = Some(MulDivParams(mulUnroll = 8)))))
    case t => t
  }
})

class WithFP16 extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(
        fpu = tp.tileParams.core.fpu.map(_.copy(minFLen = 16))
      )
    ))
    case t => t
  }
})

class WithNonblockingL1(nMSHRs: Int) extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(nMSHRs = nMSHRs))))
    case t => t
  }
})

class WithNBreakpoints(hwbp: Int) extends Config ((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(nBreakpoints = hwbp)))
    case t => t
  }
})

class WithHypervisor(hext: Boolean = true) extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(useHypervisor = hext)))
    case t => t
  }
})

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

class WithDefaultBtb extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      btb = Some(BTBParams())))
    case t => t
  }
})

class WithFastMulDiv extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(mulDiv = Some(
        MulDivParams(mulUnroll = 8, mulEarlyOut = (site(XLen) > 32), divEarlyOut = true)))))
    case t => t
  }
})

class WithoutMulDiv extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(mulDiv = None)))
    case t => t
  }
})

class WithoutFPU extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(fpu = None)))
    case t => t
  }
})

class WithFPUWithoutDivSqrt extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(fpu = tp.tileParams.core.fpu.map(_.copy(divSqrt = false)))))
    case t => t
  }
})

class WithRocketDebugROB(enable: Boolean = true) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(debugROB = enable)
    ))
  }
})

class WithRocketCease(enable: Boolean = true) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(haveCease = enable)
    ))
  }
})

class WithNoSimulationTimeout extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(haveSimTimeout = false)))
    case t => t
  }
})

class WithBootROMFile(bootROMFile: String) extends Config((site, here, up) => {
  case BootROMLocated(x) => up(BootROMLocated(x), site).map(_.copy(contentFileName = bootROMFile))
})

class WithClockGateModel(file: String = "/vsrc/EICG_wrapper.v") extends Config((site, here, up) => {
  case ClockGateModelFile => Some(file)
})

class WithSynchronousRocketTiles extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      crossingType = SynchronousCrossing()))
    case t => t
  }
})

class WithAsynchronousRocketTiles(depth: Int, sync: Int) extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      crossingType = AsynchronousCrossing()))
    case t => t
  }
})

class WithRationalRocketTiles extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      crossingType = RationalCrossing()))
    case t => t
  }
})

class WithEdgeDataBits(dataBits: Int) extends Config((site, here, up) => {
  case MemoryBusKey => up(MemoryBusKey, site).copy(beatBytes = dataBits/8)
  case ExtIn => up(ExtIn, site).map(_.copy(beatBytes = dataBits/8))
})

class WithJtagDTM extends Config ((site, here, up) => {
  case ExportDebug => up(ExportDebug, site).copy(protocols = Set(JTAG))
})

class WithDebugAPB extends Config ((site, here, up) => {
  case ExportDebug => up(ExportDebug, site).copy(protocols = Set(APB))
})


class WithDebugSBA extends Config ((site, here, up) => {
  case DebugModuleKey => up(DebugModuleKey, site).map(_.copy(hasBusMaster = true))
})

class WithNBitPeripheryBus(nBits: Int) extends Config ((site, here, up) => {
  case PeripheryBusKey => up(PeripheryBusKey, site).copy(beatBytes = nBits/8)
})

class WithoutTLMonitors extends Config ((site, here, up) => {
  case MonitorsEnabled => false
})

class WithNExtTopInterrupts(nExtInts: Int) extends Config((site, here, up) => {
  case NExtTopInterrupts => nExtInts
})

class WithNMemoryChannels(n: Int) extends Config((site, here, up) => {
  case ExtMem => up(ExtMem, site).map(_.copy(nMemoryChannels = n))
})

class WithExtMemSize(n: BigInt) extends Config((site, here, up) => {
  case ExtMem => up(ExtMem, site).map(x => x.copy(master = x.master.copy(size = n)))
})

class WithExtMemSbusBypass(base: BigInt = x"10_0000_0000") extends Config((site, here, up) => {
  case ExtMem => up(ExtMem, site).map(x => x.copy(incohBase = Some(base)))
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

class WithNoMMIOPort extends Config((site, here, up) => {
  case ExtBus => None
})

class WithDefaultSlavePort extends Config((site, here, up) => {
  case ExtIn  => Some(SlavePortParams(beatBytes = 8, idBits = 8, sourceBits = 4))
})

class WithNoSlavePort extends Config((site, here, up) => {
  case ExtIn => None
})

class WithScratchpadsBaseAddress(address: BigInt) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(
        _.copy(scratch = Some(address))
      )
    ))
    case t => t
  }
})

class WithScratchpadsOnly extends Config((site, here, up) => {
  case TilesLocated(location) => up(TilesLocated(location), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(useVM = false),
      dcache = tp.tileParams.dcache.map(_.copy(
        nSets = 256, // 16Kb scratchpad
        nWays = 1,
        scratch = Some(0x80000000L)))))
    case t => t
  }
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
  case PeripheryBusKey => up(PeripheryBusKey, site).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})
class WithMemoryBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case MemoryBusKey => up(MemoryBusKey, site).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})
class WithSystemBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case SystemBusKey => up(SystemBusKey, site).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})
class WithFrontBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case FrontBusKey => up(FrontBusKey, site).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})
class WithControlBusFrequency(freqMHz: Double) extends Config((site, here, up) => {
  case ControlBusKey => up(ControlBusKey, site).copy(dtsFrequency = Some(BigInt((freqMHz * 1e6).round)))
})

/** Under the default multi-bus topologies, this leaves bus ClockSinks undriven by the topology itself */
class WithDontDriveBusClocksFromSBus extends Config((site, here, up) => {
  case DriveClocksFromSBus => false
})

class WithCloneRocketTiles(
  n: Int = 1,
  cloneTileId: Int = 0,
  location: HierarchicalLocation = InSubsystem,
  cloneLocation: HierarchicalLocation = InSubsystem
) extends Config((site, here, up) => {
  case TilesLocated(`location`) => {
    val prev = up(TilesLocated(location), site)
    val idOffset = up(NumTiles)
    val tileAttachParams = up(TilesLocated(cloneLocation)).find(_.tileParams.tileId == cloneTileId)
      .get.asInstanceOf[RocketTileAttachParams]
    (0 until n).map { i =>
      CloneTileAttachParams(cloneTileId, tileAttachParams.copy(
        tileParams = tileAttachParams.tileParams.copy(tileId = i + idOffset)
      ))
    } ++ prev
  }
  case NumTiles => up(NumTiles) + n
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
