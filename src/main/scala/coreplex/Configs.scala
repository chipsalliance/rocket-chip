// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket._
import tile._
import uncore.converters._
import uncore.devices._
import uncore.tilelink2._
import uncore.util._
import util._

class BaseCoreplexConfig extends Config ((site, here, up) => {
  case PAddrBits => 32
  case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
  case ASIdBits => 0
  case XLen => 64 // Applies to all cores
  case BuildCore => (p: Parameters) => new Rocket()(p)
  case RocketCrossing => Synchronous
  case RocketTilesKey =>  Nil
  case DMKey => new DefaultDebugModuleConfig(site(XLen))
  case NTiles => site(RocketTilesKey).size
  case CBusConfig => TLBusConfig(beatBytes = site(XLen)/8)
  case L1toL2Config => TLBusConfig(beatBytes = site(XLen)/8) // increase for more PCIe bandwidth
  case BootROMFile => "./bootrom/bootrom.img"
  case BroadcastConfig => BroadcastConfig()
  case BankedL2Config => BankedL2Config()
  case CacheBlockBytes => 64
})

class WithNBigCores(n: Int) extends Config((site, here, up) => {
  case RocketTilesKey => {
    val big = RocketTileParams(
      core   = RocketCoreParams(mulDiv = Some(MulDivParams(
        mulUnroll = 8,
        mulEarlyOut = true,
        divEarlyOut = true))),
      dcache = Some(DCacheParams(
        rowBits = site(L1toL2Config).beatBytes*8,
        nMSHRs  = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(L1toL2Config).beatBytes*8,
        blockBytes = site(CacheBlockBytes))))
    List.fill(n)(big) ++ up(RocketTilesKey, site)
  }
})

class WithNSmallCores(n: Int) extends Config((site, here, up) => {
  case RocketTilesKey => {
    val small = RocketTileParams(
      core = RocketCoreParams(useVM = false, fpu = None),
      btb = None,
      dcache = Some(DCacheParams(
        rowBits = site(L1toL2Config).beatBytes*8,
        nSets = 64,
        nWays = 1,
        nTLBEntries = 4,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(L1toL2Config).beatBytes*8,
        nSets = 64,
        nWays = 1,
        nTLBEntries = 4,
        blockBytes = site(CacheBlockBytes))))
    List.fill(n)(small) ++ up(RocketTilesKey, site)
  }
})

class WithNBanksPerMemChannel(n: Int) extends Config((site, here, up) => {
  case BankedL2Config => up(BankedL2Config, site).copy(nBanksPerChannel = n)
})

class WithNTrackersPerBank(n: Int) extends Config((site, here, up) => {
  case BroadcastConfig => up(BroadcastConfig, site).copy(nTrackers = n)
})

// This is the number of icache sets for all Rocket tiles
class WithL1ICacheSets(sets: Int) extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(icache = r.icache.map(_.copy(nSets = sets))) }
})

// This is the number of icache sets for all Rocket tiles
class WithL1DCacheSets(sets: Int) extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(dcache = r.dcache.map(_.copy(nSets = sets))) }
})

class WithL1ICacheWays(ways: Int) extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(icache = r.icache.map(_.copy(nWays = ways)))
  }
})

class WithL1DCacheWays(ways: Int) extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(dcache = r.dcache.map(_.copy(nWays = ways)))
  }
})

class WithCacheBlockBytes(linesize: Int) extends Config((site, here, up) => {
  case CacheBlockBytes => linesize
})

class WithL2Cache extends Config(Parameters.empty) // TODO: re-add L2
class WithL2Capacity(size_kb: Int) extends Config(Parameters.empty) // TODO: re-add L2
class WithNL2Ways(n: Int) extends Config(Parameters.empty) // TODO: re-add L2

class WithBufferlessBroadcastHub extends Config((site, here, up) => {
  case BroadcastConfig => up(BroadcastConfig, site).copy(bufferless = true)
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
class WithStatelessBridge extends Config((site, here, up) => {
  case BankedL2Config => up(BankedL2Config, site).copy(coherenceManager = { case (q, _) =>
    implicit val p = q
    val cork = LazyModule(new TLCacheCork(unsafe = true))
    val ww = LazyModule(new TLWidthWidget(p(L1toL2Config).beatBytes))
    ww.node :*= cork.node
    (cork.node, ww.node)
  })
})

class WithRV32 extends Config((site, here, up) => {
  case XLen => 32
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(
      mulDiv = Some(MulDivParams(mulUnroll = 8)),
      fpu = r.core.fpu.map(_.copy(divSqrt = false))))
  }
})

class WithNonblockingL1(nMSHRs: Int) extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(dcache = r.dcache.map(_.copy(nMSHRs = nMSHRs)))
  }
})

class WithNBreakpoints(hwbp: Int) extends Config ((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(nBreakpoints = hwbp))
  }
})

class WithRoccExample extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    RoCCParams(
      opcodes = OpcodeSet.custom0,
      generator = (p: Parameters) => Module(new AccumulatorExample()(p))),
    RoCCParams(
      opcodes = OpcodeSet.custom1,
      generator = (p: Parameters) => Module(new TranslatorExample()(p)),
      nPTWPorts = 1),
    RoCCParams(
      opcodes = OpcodeSet.custom2,
      generator = (p: Parameters) => Module(new CharacterCountExample()(p))))

  case RoccMaxTaggedMemXacts => 1
})

class WithDefaultBtb extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(btb = Some(BTBParams()))
  }
})

class WithFastMulDiv extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(mulDiv = Some(
      MulDivParams(mulUnroll = 8, mulEarlyOut = (site(XLen) > 32), divEarlyOut = true)
  )))}
})

class WithoutMulDiv extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(mulDiv = None))
  }
})

class WithoutFPU extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(fpu = None))
  }
})

class WithFPUWithoutDivSqrt extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(fpu = r.core.fpu.map(_.copy(divSqrt = false))))
  }
})

class WithBootROMFile(bootROMFile: String) extends Config((site, here, up) => {
  case BootROMFile => bootROMFile
})

class WithSynchronousRocketTiles extends Config((site, here, up) => {
  case RocketCrossing => Synchronous
})

class WithAynchronousRocketTiles(depth: Int, sync: Int) extends Config((site, here, up) => {
  case RocketCrossing => Asynchronous(depth, sync)
})

class WithRationalRocketTiles extends Config((site, here, up) => {
  case RocketCrossing => Rational
})
