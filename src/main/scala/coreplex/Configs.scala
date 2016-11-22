// See LICENSE for license details.

package coreplex

import Chisel._
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.converters._
import uncore.util._
import rocket._
import util._
import util.ConfigUtils._
import config._

class BaseCoreplexConfig extends Config (
  { (pname,site,here) =>
    lazy val innerDataBits = site(XLen)
    lazy val innerDataBeats = (8 * site(CacheBlockBytes)) / innerDataBits
    pname match {
      //Memory Parameters
      case PAddrBits => 32
      case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
      case ASIdBits => 7
      //Params used by all caches
      case CacheName("L1I") => CacheConfig(
        nSets         = 64,
        nWays         = 4,
        rowBits       = site(L1toL2Config).beatBytes*8,
        nTLBEntries   = 8,
        cacheIdBits   = 0,
        splitMetadata = false)
      case CacheName("L1D") => CacheConfig(
        nSets         = 64,
        nWays         = 4,
        rowBits       = site(L1toL2Config).beatBytes*8,
        nTLBEntries   = 8,
        cacheIdBits   = 0,
        splitMetadata = false)
      case ECCCode => None
      case Replacer => () => new RandomReplacement(site(site(CacheName)).nWays)
      //L1InstCache
      case BtbKey => BtbParameters()
      //L1DataCache
      case DCacheKey => DCacheConfig(nMSHRs = 2)
      case DataScratchpadSize => 0
      //L2 Memory System Params
      case AmoAluOperandBits => site(XLen)
      case NAcquireTransactors => 7
      case L2StoreDataQueueDepth => 1
      case L2DirectoryRepresentation => new NullRepresentation(site(NTiles))
      //Tile Constants
      case BuildRoCC => Nil
      case RoccNMemChannels => site(BuildRoCC).map(_.nMemChannels).foldLeft(0)(_ + _)
      case RoccNPTWPorts => site(BuildRoCC).map(_.nPTWPorts).foldLeft(0)(_ + _)
      //Rocket Core Constants
      case CoreInstBits => if (site(UseCompressed)) 16 else 32
      case FetchWidth => if (site(UseCompressed)) 2 else 1
      case RetireWidth => 1
      case UseVM => true
      case UseUser => false
      case UseDebug => true
      case NBreakpoints => 1
      case NPerfCounters => 0
      case NPerfEvents => 0
      case FastLoadWord => true
      case FastLoadByte => false
      case FastJAL => false
      case XLen => 64
      case FPUKey => Some(FPUConfig())
      case MulDivKey => Some(MulDivConfig(mulUnroll = 8, mulEarlyOut = (site(XLen) > 32), divEarlyOut = true))
      case UseAtomics => true
      case UseCompressed => true
      case DMKey => new DefaultDebugModuleConfig(site(NTiles), site(XLen))
      case NCustomMRWCSRs => 0
      case MtvecInit => Some(BigInt(0))
      case MtvecWritable => true
      //Uncore Paramters
      case LNEndpoints => site(TLKey(site(TLId))).nManagers + site(TLKey(site(TLId))).nClients
      case LNHeaderBits => log2Ceil(site(TLKey(site(TLId))).nManagers) +
                             log2Up(site(TLKey(site(TLId))).nClients)
      case CBusConfig => TLBusConfig(beatBytes = site(XLen)/8)
      case L1toL2Config => TLBusConfig(beatBytes = site(XLen)/8) // increase for more PCIe bandwidth
      case TLKey("L1toL2") => {
        val useMEI = site(NTiles) <= 1
        TileLinkParameters(
          coherencePolicy = (
            if (useMEI) new MEICoherence(site(L2DirectoryRepresentation))
            else new MESICoherence(site(L2DirectoryRepresentation))),
          nManagers = site(BankedL2Config).nBanks + 1 /* MMIO */,
          nCachingClients = 1,
          nCachelessClients = 1,
          maxClientXacts = max_int(
              // L1 cache
              site(DCacheKey).nMSHRs + 1 /* IOMSHR */,
              // RoCC
              if (site(BuildRoCC).isEmpty) 1 else site(RoccMaxTaggedMemXacts)),
          maxClientsPerPort = if (site(BuildRoCC).isEmpty) 1 else 2,
          maxManagerXacts = site(NAcquireTransactors) + 2,
          dataBeats = innerDataBeats,
          dataBits = site(CacheBlockBytes)*8)
      }
      case BootROMFile => "./bootrom/bootrom.img"
      case NTiles => 1
      case BroadcastConfig => BroadcastConfig()
      case BankedL2Config => BankedL2Config()
      case CacheBlockBytes => 64
      case CacheBlockOffsetBits => log2Up(here(CacheBlockBytes))
      case EnableL2Logging => false
      case _ => throw new CDEMatchError
    }
  }
)

class WithNCores(n: Int) extends Config(
  (pname,site,here) => pname match {
    case NTiles => n
    case _ => throw new CDEMatchError
  })

class WithNBanksPerMemChannel(n: Int) extends Config(
  (pname, site, here, up) => pname match {
    case BankedL2Config => up(BankedL2Config).copy(nBanksPerChannel = n)
    case _ => throw new CDEMatchError
  })

class WithNTrackersPerBank(n: Int) extends Config(
  (pname, site, here, up) => pname match {
    case BroadcastConfig => up(BroadcastConfig).copy(nTrackers = n)
    case _ => throw new CDEMatchError
  })

// This is the number of sets **per way**
class WithL1ICacheSets(sets: Int) extends Config(
  (pname, site, here, up) => pname match {
    case CacheName("L1I") => up(CacheName("L1I")).copy(nSets = sets)
    case _ => throw new CDEMatchError
  })

// This is the number of sets **per way**
class WithL1DCacheSets(sets: Int) extends Config(
  (pname, site, here, up) => pname match {
    case CacheName("L1D") => up(CacheName("L1D")).copy(nSets = sets)
    case _ => throw new CDEMatchError
  })

class WithL1ICacheWays(ways: Int) extends Config(
  (pname, site, here, up) => pname match {
    case CacheName("L1I") => up(CacheName("L1I")).copy(nWays = ways)
    case _ => throw new CDEMatchError
  })

class WithL1DCacheWays(ways: Int) extends Config(
  (pname, site, here, up) => pname match {
    case CacheName("L1D") => up(CacheName("L1D")).copy(nWays = ways)
    case _ => throw new CDEMatchError
  })

class WithCacheBlockBytes(linesize: Int) extends Config(
  (pname,site,here) => pname match {
    case CacheBlockBytes => linesize
    case _ => throw new CDEMatchError
  })

class WithDataScratchpad(n: Int) extends Config(
  (pname,site,here,up) => pname match {
    case DataScratchpadSize => n
    case CacheName("L1D") => up(CacheName("L1D")).copy(nSets = n / site(CacheBlockBytes))
    case _ => throw new CDEMatchError
  })

// TODO: re-add L2
class WithL2Cache extends Config(
  (pname,site,here) => pname match {
    case CacheName("L2") => CacheConfig(
      nSets         = 1024,
      nWays         = 1,
      rowBits       = site(L1toL2Config).beatBytes*8,
      nTLBEntries   = 0,
      cacheIdBits   = 1,
      splitMetadata = false)
    case _ => throw new CDEMatchError
  })

class WithBufferlessBroadcastHub extends Config(
  (pname, site, here, up) => pname match {
    case BroadcastConfig => up(BroadcastConfig).copy(bufferless = true)
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
class WithStatelessBridge extends Config(
  (pname,site,here,up) => pname match {
/* !!! FIXME
    case BankedL2Config => up(BankedL2Config).copy(coherenceManager = { case (_, _) =>
      val pass = LazyModule(new TLBuffer(0))
      (pass.node, pass.node)
    })
*/
    case DCacheKey => up(DCacheKey).copy(nMSHRs = 0)
    case _ => throw new CDEMatchError
  })

class WithPLRU extends Config(
  (pname, site, here) => pname match {
    case _ => throw new CDEMatchError
  })

class WithL2Capacity(size_kb: Int) extends Config(
  (pname,site,here) => pname match {
    case _ => throw new CDEMatchError
  })

class WithNL2Ways(n: Int) extends Config(
  (pname,site,here,up) => pname match {
    case CacheName("L2") => up(CacheName("L2")).copy(nWays = n)
  })

class WithRV32 extends Config(
  (pname,site,here) => pname match {
    case XLen => 32
    case FPUKey => Some(FPUConfig(divSqrt = false))
    case _ => throw new CDEMatchError
  })

class WithBlockingL1 extends Config(
  (pname,site,here,up) => pname match {
    case DCacheKey => up(DCacheKey).copy(nMSHRs = 0)
    case _ => throw new CDEMatchError
  })

class WithSmallCores extends Config(
  (pname,site,here,up) => pname match {
    case MulDivKey => Some(MulDivConfig())
    case FPUKey => None
    case UseVM => false
    case BtbKey => BtbParameters(nEntries = 0)
    case NAcquireTransactors => 2
    case CacheName("L1D") => up(CacheName("L1D")).copy(nSets = 64, nWays = 1, nTLBEntries = 4)
    case CacheName("L1I") => up(CacheName("L1I")).copy(nSets = 64, nWays = 1, nTLBEntries = 4)
    case DCacheKey => up(DCacheKey).copy(nMSHRs = 0)
    case _ => throw new CDEMatchError
  })

class WithRoccExample extends Config(
  (pname, site, here) => pname match {
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
    case _ => throw new CDEMatchError
  })

class WithDefaultBtb extends Config(
  (pname,site,here) => pname match {
    case BtbKey => BtbParameters()
    case _ => throw new CDEMatchError
  })

class WithFastMulDiv extends Config(
  (pname,site,here) => pname match {
    case MulDivKey => Some(MulDivConfig(mulUnroll = 8, mulEarlyOut = (site(XLen) > 32), divEarlyOut = true))
    case _ => throw new CDEMatchError
  })

class WithoutMulDiv extends Config(
  (pname, site, here) => pname match {
    case MulDivKey => None
    case _ => throw new CDEMatchError
  })

class WithoutFPU extends Config(
  (pname, site, here) => pname match {
    case FPUKey => None
    case _ => throw new CDEMatchError
  })

class WithFPUWithoutDivSqrt extends Config (
  (pname, site, here) => pname match {
    case FPUKey => Some(FPUConfig(divSqrt = false))
    case _ => throw new CDEMatchError
  })
