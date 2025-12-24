package freechips.rocketchip.rocket

import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.prci.{SynchronousCrossing, AsynchronousCrossing, RationalCrossing, ClockCrossingType}
import freechips.rocketchip.subsystem.{TilesLocated, NumTiles, HierarchicalLocation, RocketCrossingParams, SystemBusKey, CacheBlockBytes, RocketTileAttachParams, InSubsystem, InCluster, HierarchicalElementMasterPortParams, HierarchicalElementSlavePortParams, CBUS, CCBUS, ClustersLocated, TileAttachConfig, CloneTileAttachParams}
import freechips.rocketchip.tile.{RocketTileParams, RocketTileBoundaryBufferParams, FPUParams}
import freechips.rocketchip.util.{RationalDirection, Flexible}
import scala.reflect.ClassTag

// All the user-level bells and whistles
class WithNHugeCores(
  n: Int,
  location: HierarchicalLocation,
  crossing: RocketCrossingParams,
) extends Config((site, here, up) => {
  case TilesLocated(`location`) => {
    val prev = up(TilesLocated(`location`))
    val idOffset = up(NumTiles)
    val big = RocketTileParams(
      core   = RocketCoreParams(
        mulDiv = Some(MulDivParams(
          mulUnroll = 8,
          mulEarlyOut = true,
          divEarlyOut = true,
        )),
        useZba = true,
        useZbb = true,
        useZbs = true,
        fpu = Some(FPUParams(minFLen = 16))),
      dcache = Some(DCacheParams(
        nSets = 64,
        nWays = 8,
        rowBits = site(SystemBusKey).beatBits,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        nSets = 64,
        nWays = 8,
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

class WithNBigCores(
  n: Int,
  location: HierarchicalLocation,
  crossing: RocketCrossingParams,
) extends Config((site, here, up) => {
  case TilesLocated(`location`) => {
    val prev = up(TilesLocated(`location`))
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
    val prev = up(TilesLocated(InSubsystem))
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
    val prev = up(TilesLocated(InSubsystem))
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
  case TilesLocated(InSubsystem) => {
    val tiny = RocketTileParams(
      core = RocketCoreParams(
        xLen = 32,
        pgLevels = 2, // sv32
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

class RocketTileAttachConfig(f: RocketTileAttachParams => RocketTileAttachParams) extends TileAttachConfig[RocketTileAttachParams](f)

class RocketTileConfig(f: RocketTileParams => RocketTileParams) extends RocketTileAttachConfig(tp => tp.copy(
  tileParams = f(tp.tileParams)
))

class RocketCrossingConfig(f: RocketCrossingParams => RocketCrossingParams) extends RocketTileAttachConfig(tp => tp.copy(
  crossingParams = f(tp.crossingParams)
))

class RocketCoreConfig(f: RocketCoreParams => RocketCoreParams) extends RocketTileConfig(tp => tp.copy(
  core = f(tp.core)
))

class RocketICacheConfig(f: ICacheParams => ICacheParams) extends RocketTileConfig(tp => tp.copy(
  icache = tp.icache.map(ic => f(ic))
))

class RocketDCacheConfig(f: DCacheParams => DCacheParams) extends RocketTileConfig(tp => tp.copy(
  dcache = tp.dcache.map(dc => f(dc))
))

class WithL1ICacheSets(sets: Int)                      extends RocketICacheConfig(_.copy(nSets=sets))
class WithL1ICacheWays(ways: Int)                      extends RocketICacheConfig(_.copy(nWays=ways))
class WithL1ICacheECC(dataECC: String, tagECC: String) extends RocketICacheConfig(_.copy(dataECC = Some(dataECC), tagECC = Some(tagECC)))
class WithL1ICacheRowBits(n: Int)                      extends RocketICacheConfig(_.copy(rowBits = n))
class WithL1ICacheTLBSets(sets: Int)                   extends RocketICacheConfig(_.copy(nTLBSets = sets))
class WithL1ICacheTLBWays(ways: Int)                   extends RocketICacheConfig(_.copy(nTLBWays = ways))
class WithL1ICacheTLBBasePageSectors(sectors: Int)     extends RocketICacheConfig(_.copy(nTLBBasePageSectors = sectors))
class WithL1ICacheTLBSuperpages(superpages: Int)       extends RocketICacheConfig(_.copy(nTLBSuperpages = superpages))
class WithL1ICacheBlockBytes(bytes: Int = 64)          extends RocketICacheConfig(_.copy(blockBytes = bytes))

class WithL1DCacheSets(sets: Int)                      extends RocketDCacheConfig(_.copy(nSets=sets))
class WithL1DCacheWays(ways: Int)                      extends RocketDCacheConfig(_.copy(nWays=ways))
class WithL1DCacheECC(dataECC: String, tagECC: String) extends RocketDCacheConfig(_.copy(dataECC = Some(dataECC), tagECC = Some(tagECC)))
class WithL1DCacheRowBits(n: Int)                      extends RocketDCacheConfig(_.copy(rowBits = n))
class WithL1DCacheTLBSets(sets: Int)                   extends RocketDCacheConfig(_.copy(nTLBSets = sets))
class WithL1DCacheTLBWays(ways: Int)                   extends RocketDCacheConfig(_.copy(nTLBWays = ways))
class WithL1DCacheTLBBasePageSectors(sectors: Int)     extends RocketDCacheConfig(_.copy(nTLBBasePageSectors = sectors))
class WithL1DCacheTLBSuperpages(superpages: Int)       extends RocketDCacheConfig(_.copy(nTLBSuperpages = superpages))
class WithL1DCacheBlockBytes(bytes: Int = 64)          extends RocketDCacheConfig(_.copy(blockBytes = bytes))
class WithL1DCacheNonblocking(nMSHRs: Int)             extends RocketDCacheConfig(_.copy(nMSHRs = nMSHRs))
class WithL1DCacheClockGating                          extends RocketDCacheConfig(_.copy(clockGate = true))
class WithL1DCacheDTIMAddress(address: BigInt)         extends RocketDCacheConfig(_.copy(scratch = Some(address)))

class WithScratchpadsOnly extends RocketTileConfig(tp => tp.copy(
  core = tp.core.copy(useVM = false),
  dcache = tp.dcache.map(_.copy(
    nSets = 256, // 16Kb scratchpad
    nWays = 1,
    scratch = Some(0x80000000L)))
))

class WithCacheRowBits(n: Int) extends RocketTileConfig(tp => tp.copy(
  dcache = tp.dcache.map(_.copy(rowBits = n)),
  icache = tp.icache.map(_.copy(rowBits = n))
))

class WithBEU(addr: BigInt) extends RocketTileConfig(_.copy(beuAddr = Some(addr)))
class WithBoundaryBuffers(buffers: Option[RocketTileBoundaryBufferParams] = Some(RocketTileBoundaryBufferParams(true))) extends RocketTileConfig(_.copy(boundaryBuffers = buffers))

class WithRV32 extends RocketCoreConfig(c => c.copy(
  xLen = 32,
  pgLevels = 2, // sv32
  fpu = c.fpu.map(_.copy(fLen = 32)),
  mulDiv = Some(MulDivParams(mulUnroll = 8))
))

class WithoutVM                                           extends RocketCoreConfig(_.copy(useVM = false))
class WithCFlushEnabled                                   extends RocketCoreConfig(_.copy(haveCFlush = true))
class WithNBreakpoints(hwbp: Int)                         extends RocketCoreConfig(_.copy(nBreakpoints = hwbp))
class WithHypervisor(hext: Boolean = true)                extends RocketCoreConfig(_.copy(useHypervisor = hext))
class WithCease(enable: Boolean = true)                   extends RocketCoreConfig(_.copy(haveCease = enable))
class WithCoreClockGatingEnabled                          extends RocketCoreConfig(_.copy(clockGate = true))
class WithPgLevels(n: Int)                                extends RocketCoreConfig(_.copy(pgLevels = n))
class WithZba                                             extends RocketCoreConfig(_.copy(useZba = true))
class WithZbb                                             extends RocketCoreConfig(_.copy(useZbb = true))
class WithZbs                                             extends RocketCoreConfig(_.copy(useZbs = true))
class WithB                                               extends RocketCoreConfig(_.copy(useZba = true, useZbb = true, useZbs = true))
class WithSV48                                            extends WithPgLevels(4)
class WithSV39                                            extends WithPgLevels(3)

// Simulation-only configs
class WithNoSimulationTimeout extends RocketCoreConfig(_.copy(haveSimTimeout = false))
class WithDebugROB(enable: Boolean = true, size: Int = 0) extends RocketCoreConfig(_.copy(debugROB = Option.when(enable)(DebugROBParams(size))))

// FPU configs
class WithoutFPU            extends RocketCoreConfig(_.copy(fpu = None))
class WithFP16              extends RocketCoreConfig(c => c.copy(fpu = c.fpu.map(_.copy(minFLen = 16))))
class WithFPUWithoutDivSqrt extends RocketCoreConfig(c => c.copy(fpu = c.fpu.map(_.copy(divSqrt = false))))

// mul-div configs
class WithFastMulDiv extends RocketCoreConfig(c => c.copy(mulDiv = Some(
  MulDivParams(mulUnroll = 8, mulEarlyOut = c.xLen > 32, divEarlyOut = true)
)))
class WithCustomFastMulDiv(mUnroll: Int = 8, mEarlyOut: Boolean = true, dUnroll: Int = 1, dEarlyOut: Boolean = true, dEarlyOutGranularity: Int = 1) extends RocketCoreConfig(_.copy(mulDiv = Some(
  MulDivParams(mulUnroll = mUnroll, mulEarlyOut = mEarlyOut, divUnroll = dUnroll, divEarlyOut = dEarlyOut, divEarlyOutGranularity = dEarlyOutGranularity)
)))
class WithoutMulDiv extends RocketCoreConfig(_.copy(mulDiv = None))

// Branch-prediction configs
class WithDefaultBtb extends RocketTileConfig(t => t.copy(btb = Some(BTBParams())))
class WithNoBtb      extends RocketTileConfig(_.copy(btb = None))

// Tile CDC configs
class WithCDC(crossingType: ClockCrossingType = SynchronousCrossing()) extends RocketCrossingConfig(_.copy(crossingType = crossingType))
class WithSeperateClockReset                                           extends RocketCrossingConfig(_.copy(forceSeparateClockReset = true))
class WithSynchronousCDCs                                              extends WithCDC(SynchronousCrossing())
class WithAsynchronousCDCs(depth: Int, sync: Int)                      extends WithCDC(AsynchronousCrossing(depth, sync))
class WithRationalCDCs(direction: RationalDirection = Flexible)        extends WithCDC(RationalCrossing(direction))



class WithCloneRocketTiles(
  n: Int = 1,
  cloneTileId: Int = 0,
  location: HierarchicalLocation = InSubsystem,
  cloneLocation: HierarchicalLocation = InSubsystem
) extends Config((site, here, up) => {
  case TilesLocated(`location`) => {
    val prev = up(TilesLocated(location))
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

