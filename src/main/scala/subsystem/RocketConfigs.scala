package freechips.rocketchip.subsystem
// TODO move to freechips.rocketchip.rocket

import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.prci.{SynchronousCrossing, AsynchronousCrossing, RationalCrossing, ClockCrossingType}
import freechips.rocketchip.rocket.{PgLevels, RocketCoreParams, MulDivParams, DCacheParams, ICacheParams, BTBParams, DebugROBParams}
import freechips.rocketchip.tile.{
  XLen, MaxHartIdBits, RocketTileParams, BuildRoCC, AccumulatorExample, OpcodeSet, TranslatorExample, CharacterCountExample, BlackBoxExample
}

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

class WithL1ICacheECC(dataECC: String, tagECC: String) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(dataECC = Some(dataECC), tagECC = Some(tagECC)))))
    case t => t
  }
})

class WithL1DCacheECC(dataECC: String, tagECC: String) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(dataECC = Some(dataECC), tagECC = Some(tagECC), dataECCBytes=8))))
    case t => t
  }
})

class WithL1ICacheTLBSets(tlbsets: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(nTLBSets = tlbsets))))
    case t => t
  }
})

class WithL1DCacheTLBSets(tlbsets: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(nTLBSets = tlbsets))))
    case t => t
  }
})

class WithL1ICacheTLBWays(tlbways: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(nTLBWays = tlbways))))
    case t => t
  }
})

class WithL1DCacheTLBWays(tlbways: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(nTLBWays = tlbways))))
    case t => t
  }
})

class WithL1ICacheTLBBasePageSectors(pagesectors: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(nTLBBasePageSectors = pagesectors))))
    case t => t
  }
})

class WithL1DCacheTLBBasePageSectors(pagesectors: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(nTLBBasePageSectors = pagesectors))))
    case t => t
  }
})

class WithL1ICacheTLBSuperpages(superpages: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(nTLBSuperpages = superpages))))
    case t => t
  }
})

class WithL1DCacheTLBSuperpages(superpages: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(nTLBSuperpages = superpages))))
    case t => t
  }
})

class WithRocketICacheRowBits(n: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(rowBits = n))))
    case t => t
  }
})

class WithRocketDCacheRowBits(n: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(rowBits = n))))
    case t => t
  }
})

class WithL1ICacheBlockBytes(bytes: Int = 64) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      icache = tp.tileParams.icache.map(_.copy(blockBytes = bytes))))
    case t => t
  }
})

class WithL1DCacheBlockBytes(bytes: Int = 64) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(blockBytes = bytes))))
    case t => t
  }
})

class WithoutVM extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(useVM = false)))
    case t => t
  }
})

class WithCFlushEnabled extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(haveCFlush = true)))
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

class WithNoBtb extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      btb = None))
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

class WithCustomFastMulDiv(mUnroll: Int = 8, mEarlyOut: Boolean = true, dUnroll: Int = 1, dEarlyOut: Boolean = true, dEarlyOutGranularity: Int = 1) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(mulDiv = Some(
        MulDivParams(mulUnroll = mUnroll, mulEarlyOut = mEarlyOut, divUnroll = dUnroll, divEarlyOut = dEarlyOut, divEarlyOutGranularity = dEarlyOutGranularity)))))
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

class WithBEU(addr: BigInt) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(beuAddr = Some(addr)))
    case t => t
  }
})

class WithRocketTileCDC(crossingType: ClockCrossingType = SynchronousCrossing()) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      crossingType = crossingType
    ))
    case other => other
  }
})

class WithSeperateClockReset extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      forceSeparateClockReset = true
    ))
    case other => other
  }
})


class WithRocketDebugROB(enable: Boolean = true, size: Int = 0) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams if (enable) =>
      tp.copy(tileParams = tp.tileParams.copy(
        core = tp.tileParams.core.copy(debugROB = Some(DebugROBParams(size)))
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

class WithCoreClockGatingEnabled extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(clockGate = true)
    ))
    case t => t
  }
})

class WithDCacheClockGatingEnabled extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      dcache = tp.tileParams.dcache.map(_.copy(clockGate = true))))
    case t => t
  }
})

class WithNoSimulationTimeout extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(haveSimTimeout = false)))
    case t => t
  }
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

