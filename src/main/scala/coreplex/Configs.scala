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
  //L1InstCache
  case CacheName("L1I") => CacheConfig(
    rowBits = site(L1toL2Config).beatBytes*8,
    latency = 1)
  case BtbKey => BtbParameters()
  //L1DataCache
  case CacheName("L1D") => CacheConfig(
  case DCacheKey(_) => DCacheConfig(
    rowBits = site(L1toL2Config).beatBytes*8,
    nMSHRs  = 2)
  case DataScratchpadSize => 0
  //Rocket Core Constants
  case UseVM => true
  case UseUser => false
  case UseDebug => true
  case XLen => 64
  //Tile Constants
  case BuildRoCC => Nil
  case BuildCore => (c: RocketConfig, p: Parameters) => new Rocket(c)(p)
  //Uncore Paramters
  case DMKey => new DefaultDebugModuleConfig(site(NTiles), site(XLen))
  case RocketTileConfigs => List(RocketTileConfig())
  case NTiles => site(RocketTileConfigs).size
  case CBusConfig => TLBusConfig(beatBytes = site(XLen)/8)
  case L1toL2Config => TLBusConfig(beatBytes = site(XLen)/8) // increase for more PCIe bandwidth
  case BootROMFile => "./bootrom/bootrom.img"
  case BroadcastConfig => BroadcastConfig()
  case BankedL2Config => BankedL2Config()
  case CacheBlockBytes => 64
})

class WithNCores(n: Int) extends Config((site, here, up) => {
  case RocketConfigs => List.fill(n-1)(RocketConfig()) :+ up(RocketTileConfigs, site)
})

class WithNSmallCores(n: Int) extends Config((site, here, up) => {
  val small = RocketTileConfig(
    core = RocketCoreConfig(
      fpuCOnfig = None,
      mulDivConfig = Some(MulDivConfig()))
    btb = BtbParameters(nEntries = 0)
  case CacheName("L1D") => up(CacheName("L1D"), site).copy(nSets = 64, nWays = 1, nTLBEntries = 4)
  case CacheName("L1I") => up(CacheName("L1I"), site).copy(nSets = 64, nWays = 1, nTLBEntries = 4)
  case DCacheKey => up(DCacheKey, site).copy(nMSHRs = 0)
  case Rocket

class WithNBanksPerMemChannel(n: Int) extends Config((site, here, up) => {
  case BankedL2Config => up(BankedL2Config, site).copy(nBanksPerChannel = n)
})

class WithNTrackersPerBank(n: Int) extends Config((site, here, up) => {
  case BroadcastConfig => up(BroadcastConfig, site).copy(nTrackers = n)
})

// This is the number of sets **per way**
class WithL1ICacheSets(sets: Int) extends Config((site, here, up) => {
  case CacheName("L1I") => up(CacheName("L1I"), site).copy(nSets = sets)
})

// This is the number of sets **per way**
class WithL1DCacheSets(sets: Int) extends Config((site, here, up) => {
  case CacheName("L1D") => up(CacheName("L1D"), site).copy(nSets = sets)
})

class WithL1ICacheWays(ways: Int) extends Config((site, here, up) => {
  case CacheName("L1I") => up(CacheName("L1I"), site).copy(nWays = ways)
})

class WithL1DCacheWays(ways: Int) extends Config((site, here, up) => {
  case CacheName("L1D") => up(CacheName("L1D"), site).copy(nWays = ways)
})

class WithCacheBlockBytes(linesize: Int) extends Config((site, here, up) => {
  case CacheBlockBytes => linesize
})

class WithDataScratchpad(n: Int) extends Config((site, here, up) => {
  case DataScratchpadSize => n
  case CacheName("L1D") => up(CacheName("L1D"), site).copy(nSets = n / site(CacheBlockBytes))
})

// TODO: re-add L2
class WithL2Cache extends Config((site, here, up) => {
  case CacheName("L2") => CacheConfig(
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
/* !!! FIXME
    case BankedL2Config => up(BankedL2Config, site).copy(coherenceManager = { case (_, _) =>
      val pass = LazyModule(new TLBuffer(0)(site))
      (pass.node, pass.node)
    })
*/
  case DCacheKey => up(DCacheKey, site).copy(nMSHRs = 0)
})

class WithL2Capacity(size_kb: Int) extends Config(Parameters.empty) // TODO

class WithNL2Ways(n: Int) extends Config((site, here, up) => {
  case CacheName("L2") => up(CacheName("L2"), site).copy(nWays = n)
})

class WithRV32 extends Config((site, here, up) => {
  case XLen => 32
  case FPUKey => Some(FPUConfig(divSqrt = false))
})

class WithBlockingL1 extends Config((site, here, up) => {
  case DCacheKey => up(DCacheKey, site).copy(nMSHRs = 0)
})

class WithSmallCores extends Config((site, here, up) => {
  case MulDivKey => Some(MulDivConfig())
  case FPUKey => None
  case UseVM => false
  case BtbKey => BtbParameters(nEntries = 0)
  case CacheName("L1D") => up(CacheName("L1D"), site).copy(nSets = 64, nWays = 1, nTLBEntries = 4)
  case CacheName("L1I") => up(CacheName("L1I"), site).copy(nSets = 64, nWays = 1, nTLBEntries = 4)
  case DCacheKey => up(DCacheKey, site).copy(nMSHRs = 0)
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
  case BtbKey => BtbParameters()
})

class WithFastMulDiv extends Config((site, here, up) => {
  case MulDivKey => Some(MulDivConfig(mulUnroll = 8, mulEarlyOut = (site(XLen) > 32), divEarlyOut = true))
})

class WithoutMulDiv extends Config((site, here, up) => {
  case MulDivKey => None
})

class WithoutFPU extends Config((site, here, up) => {
  case FPUKey => None
})

class WithFPUWithoutDivSqrt extends Config((site, here, up) => {
  case FPUKey => Some(FPUConfig(divSqrt = false))
})

class WithBootROMFile(bootROMFile: String) extends Config((site, here, up) => {
  case BootROMFile => bootROMFile
})
