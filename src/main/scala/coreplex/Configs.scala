// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket._
import uncore.converters._
import uncore.devices._
import uncore.tilelink2._
import uncore.util._
import util._

class BaseCoreplexConfig extends Config ((site, here, up) => {
  //Memory Parameters
  case PAddrBits => 32
  case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
  case ASIdBits => 7
  //Rocket Core Constants (for all cores)
  case UseVM => true
  case UseUser => false
  case UseDebug => true
  case XLen => 64
  case BuildCore => (p: Parameters) => new Rocket()(p)
  case RocketCrossing => Synchronous
  case RocketTilesParameters => List(
    RocketTileConfig(
      dcache = DCacheConfig(rowBits = site(L1toL2Config).beatBytes*8, nMSHRs  = 2),
      icache = ICacheConfig(rowBits = site(L1toL2Config).beatBytes*8))
  )
  //Uncore Paramters
  case DMKey => new DefaultDebugModuleConfig(site(NTiles), site(XLen))
  case NTiles => site(RocketTilesParameters).size
  case CBusConfig => TLBusConfig(beatBytes = site(XLen)/8)
  case L1toL2Config => TLBusConfig(beatBytes = site(XLen)/8) // increase for more PCIe bandwidth
  case BootROMFile => "./bootrom/bootrom.img"
  case BroadcastConfig => BroadcastConfig()
  case BankedL2Config => BankedL2Config()
  case CacheBlockBytes => 64
})

class WithNCores(n: Int) extends Config((site, here, up) => {
  case RocketTilesParameters => List.fill(n)(up(RocketTilesParameters, site).head)
})

class WithNSmallCores(n: Int) extends Config((site, here, up) => {
  case UseVM => false
  case RocketTilesParameters => {
    val small = RocketTileParameters(
      core = RocketCoreParameters(
        fpuConfig = None,
        mulDivConfig = Some(MulDivParameters())),
      btb = BTBParameters(nEntries = 0),
      dcache = DCacheParameters(nSets = 64, nWays = 1, nTLBEntries = 4, nMSHRs = 0),
      icache = ICacheParameters(nSets = 64, nWays = 1, nTLBEntries = 4))
    List.fill(n)(small) :+ up(RocketTilesParameters, site)
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
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(icache = r.icache.copy(nSets = sets)) }
})

// This is the number of icache sets for all Rocket tiles
class WithL1DCacheSets(sets: Int) extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(dcache = r.dcache.copy(nSets = sets)) }
})

class WithL1ICacheWays(ways: Int) extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(icache = r.icache.copy(nWays = ways)) }
})

class WithL1DCacheWays(ways: Int) extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(dcache = r.dcache.copy(nWays = ways)) }
})

class WithCacheBlockBytes(linesize: Int) extends Config((site, here, up) => {
  case CacheBlockBytes => linesize
})

class WithDataScratchpad(n: Int) extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(dcache = r.dcache.copy(nSets = n / site(CacheBlockBytes))) }
  case DataScratchpadSize => n
})

// TODO: re-add L2
class WithL2Cache extends Config((site, here, up) => {
  case CacheName("L2") => DCacheConfig(
    nSets         = 1024,
    nWays         = 1,
    rowBits       = site(L1toL2Config).beatBytes*8,
    nTLBEntries   = 0,
    cacheIdBits   = 1,
    splitMetadata = false)
})

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
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(dcache = r.dcache.copy(nMSHRs = 0)) }
})

class WithL2Capacity(size_kb: Int) extends Config(Parameters.empty) // TODO

class WithNL2Ways(n: Int) extends Config((site, here, up) => {
  case CacheName("L2") => up(CacheName("L2"), site).copy(nWays = n)
})

class WithRV32 extends Config((site, here, up) => {
  case XLen => 32
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(fpu = r.fpu.copy(divSqrt = false)) }
})

class WithBlockingL1 extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(dcache = r.dcache.copy(nMSHRs = 0)) }
})

class WithRoccExample extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    RoccParameters(
      opcodes = OpcodeSet.custom0,
      generator = (p: Parameters) => Module(new AccumulatorExample()(p))),
    RoccParameters(
      opcodes = OpcodeSet.custom1,
      generator = (p: Parameters) => Module(new TranslatorExample()(p)),
      nPTWPorts = 1),
    RoccParameters(
      opcodes = OpcodeSet.custom2,
      generator = (p: Parameters) => Module(new CharacterCountExample()(p))))

  case RoccMaxTaggedMemXacts => 1
})

class WithDefaultBtb extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(btb = BTBConfig()) }
})

class WithFastMulDiv extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(core = r.core.copy(mulDivConfig = Some(
    MulDivConfig(mulUnroll = 8, mulEarlyOut = (site(XLen) > 32), divEarlyOut = true)
  )))}
})

class WithoutMulDiv extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(core = r.core.copy(mulDivConfig = None)) }
})

class WithoutFPU extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(core = r.core.copy(fpuConfig = None)) }
})

class WithFPUWithoutDivSqrt extends Config((site, here, up) => {
  case RocketTilesParameters => up(RocketTilesParameters, site) map { r => r.copy(core = r.core.copy(fpuConfig = r.core.fpu.copy(divSqrt = false))) }
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
