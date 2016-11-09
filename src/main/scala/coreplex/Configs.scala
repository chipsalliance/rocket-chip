// See LICENSE for license details.

package coreplex

import Chisel._
import junctions._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.converters._
import rocket._
import util._
import util.ConfigUtils._
import rocketchip.{GlobalAddrMap, NCoreplexExtClients}
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}

class BaseCoreplexConfig extends Config (
  topDefinitions = { (pname,site,here) => 
    type PF = PartialFunction[Any,Any]
    def findBy(sname:Any):Any = here[PF](site[Any](sname))(pname)
    lazy val innerDataBits = site(XLen)
    lazy val innerDataBeats = (8 * site(CacheBlockBytes)) / innerDataBits
    pname match {
      //Memory Parameters
      case PAddrBits => 32
      case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
      case ASIdBits => 7
      //Params used by all caches
      case NSets => findBy(CacheName)
      case NWays => findBy(CacheName)
      case RowBits => findBy(CacheName)
      case NTLBEntries => findBy(CacheName)
      case CacheIdBits => findBy(CacheName)
      case SplitMetadata => findBy(CacheName)
      case "L1I" => {
        case NSets => Knob("L1I_SETS") //64
        case NWays => Knob("L1I_WAYS") //4
        case RowBits => site(TLKey("L1toL2")).dataBitsPerBeat
        case NTLBEntries => 8
        case CacheIdBits => 0
        case SplitMetadata => false
      }:PF
      case "L1D" => {
        case NSets => Knob("L1D_SETS") //64
        case NWays => Knob("L1D_WAYS") //4
        case RowBits => site(TLKey("L1toL2")).dataBitsPerBeat
        case NTLBEntries => 8
        case CacheIdBits => 0
        case SplitMetadata => false
      }:PF
      case ECCCode => None
      case Replacer => () => new RandomReplacement(site(NWays))
      //L1InstCache
      case BtbKey => BtbParameters()
      //L1DataCache
      case DCacheKey => DCacheConfig(nMSHRs = site(Knob("L1D_MSHRS")))
      case DataScratchpadSize => 0
      //L2 Memory System Params
      case AmoAluOperandBits => site(XLen)
      case NAcquireTransactors => 7
      case L2StoreDataQueueDepth => 1
      case L2DirectoryRepresentation => new NullRepresentation(site(NTiles))
      case BuildL2CoherenceManager => (id: Int, p: Parameters) =>
        Module(new L2BroadcastHub()(p.alterPartial({
          case InnerTLId => "L1toL2"
          case OuterTLId => "L2toMC" })))
      case NCachedTileLinkPorts => 1
      case NUncachedTileLinkPorts => 1
      //Tile Constants
      case BuildTiles => {
        List.tabulate(site(NTiles)){ i => (r: Bool, p: Parameters) =>
          Module(new RocketTile(resetSignal = r)(p.alterPartial({
            case TileId => i
            case TLId => "L1toL2"
            case NUncachedTileLinkPorts => 1 + site(RoccNMemChannels)
          })))
        }
      }
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
      case TLKey("DefaultL1toL2") => {
        val useMEI = site(NTiles) <= 1 && site(NCachedTileLinkPorts) <= 1
        TileLinkParameters(
          coherencePolicy = (
            if (useMEI) new MEICoherence(site(L2DirectoryRepresentation))
            else new MESICoherence(site(L2DirectoryRepresentation))),
          nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels) + 1 /* MMIO */,
          nCachingClients = site(NCachedTileLinkPorts),
          nCachelessClients = site(NCoreplexExtClients) + site(NUncachedTileLinkPorts),
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
      case TLKey("L1toL2") => site(TLKey("DefaultL1toL2")).copy()
      case TLKey("DefaultL2toMC") =>
        TileLinkParameters(
          coherencePolicy = new MEICoherence(
            new NullRepresentation(site(NBanksPerMemoryChannel))),
          nManagers = 1,
          nCachingClients = site(NBanksPerMemoryChannel),
          nCachelessClients = 0,
          maxClientXacts = site(NAcquireTransactors) + 2,
          maxClientsPerPort = site(NBanksPerMemoryChannel),
          maxManagerXacts = 1,
          dataBeats = innerDataBeats,
          dataBits = site(CacheBlockBytes)*8)
      case TLKey("L2toMC") => site(TLKey("DefaultL2toMC")).copy()
      case TLKey("DefaultL2toMMIO") => {
        TileLinkParameters(
          coherencePolicy = new MICoherence(
            new NullRepresentation(site(NBanksPerMemoryChannel))),
          nManagers = 1,
          nCachingClients = 0,
          nCachelessClients = 1,
          maxClientXacts = 4,
          maxClientsPerPort = 1,
          maxManagerXacts = 1,
          dataBeats = innerDataBeats,
          dataBits = site(CacheBlockBytes) * 8)
      }
      case TLKey("L2toMMIO") => site(TLKey("DefaultL2toMMIO")).copy()
      case BootROMFile => "./bootrom/bootrom.img"
      case NTiles => 1
      case NBanksPerMemoryChannel => Knob("NBANKS_PER_MEM_CHANNEL")
      case BankIdLSB => 0
      case CacheBlockBytes => Dump("CACHE_BLOCK_BYTES", 64)
      case CacheBlockOffsetBits => log2Up(here(CacheBlockBytes))
      case EnableL2Logging => false
      case _ => throw new CDEMatchError
  }},
  knobValues = {
    case "NBANKS_PER_MEM_CHANNEL" => 1
    case "L1D_MSHRS" => 2
    case "L1D_SETS" => 64
    case "L1D_WAYS" => 4
    case "L1I_SETS" => 64
    case "L1I_WAYS" => 4
    case _ => throw new CDEMatchError
  }
)

class WithNCores(n: Int) extends Config(
  (pname,site,here) => pname match {
    case NTiles => n
  })

class WithNBanksPerMemChannel(n: Int) extends Config(
  knobValues = {
    case "NBANKS_PER_MEM_CHANNEL" => n
    case _ => throw new CDEMatchError
  })

class WithDataScratchpad(n: Int) extends Config(
  (pname,site,here) => pname match {
    case DataScratchpadSize => n
    case NSets if site(CacheName) == "L1D" => n / site(CacheBlockBytes)
    case _ => throw new CDEMatchError
  })

class WithL2Cache extends Config(
  (pname,site,here) => pname match {
    case "L2_CAPACITY_IN_KB" => Knob("L2_CAPACITY_IN_KB")
    case "L2Bank" => {
      case NSets => (((here[Int]("L2_CAPACITY_IN_KB")*1024) /
                        site(CacheBlockBytes)) /
                          (site(NBanksPerMemoryChannel)*site(NMemoryChannels))) /
                            site(NWays)
      case NWays => Knob("L2_WAYS")
      case RowBits => site(TLKey(site(TLId))).dataBitsPerBeat
      case CacheIdBits => log2Ceil(site(NMemoryChannels) * site(NBanksPerMemoryChannel))
      case SplitMetadata => Knob("L2_SPLIT_METADATA")
    }: PartialFunction[Any,Any] 
    case NAcquireTransactors => 2
    case NSecondaryMisses => 4
    case L2DirectoryRepresentation => new FullRepresentation(site(NTiles))
    case BuildL2CoherenceManager => (id: Int, p: Parameters) =>
      Module(new L2HellaCacheBank()(p.alterPartial({
        case CacheId => id
        case CacheName => "L2Bank"
        case InnerTLId => "L1toL2"
        case OuterTLId => "L2toMC"})))
    case L2Replacer => () => new SeqRandom(site(NWays))
    case _ => throw new CDEMatchError
  },
  knobValues = { case "L2_WAYS" => 8; case "L2_CAPACITY_IN_KB" => 2048; case "L2_SPLIT_METADATA" => false; case _ => throw new CDEMatchError }
)

class WithBufferlessBroadcastHub extends Config(
  (pname, site, here) => pname match {
    case BuildL2CoherenceManager => (id: Int, p: Parameters) =>
      Module(new BufferlessBroadcastHub()(p.alterPartial({
        case InnerTLId => "L1toL2"
        case OuterTLId => "L2toMC" })))
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
class WithStatelessBridge extends Config (
  topDefinitions = (pname, site, here) => pname match {
    case BuildL2CoherenceManager => (id: Int, p: Parameters) =>
      Module(new ManagerToClientStatelessBridge()(p.alterPartial({
        case InnerTLId => "L1toL2"
        case OuterTLId => "L2toMC" })))
  },
  knobValues = {
    case "L1D_MSHRS" => 0
    case _ => throw new CDEMatchError
  }
)

class WithPLRU extends Config(
  (pname, site, here) => pname match {
    case L2Replacer => () => new SeqPLRU(site(NSets), site(NWays))
    case _ => throw new CDEMatchError
  })

class WithL2Capacity(size_kb: Int) extends Config(
  knobValues = {
    case "L2_CAPACITY_IN_KB" => size_kb
    case _ => throw new CDEMatchError
  })

class WithNL2Ways(n: Int) extends Config(
  knobValues = {
    case "L2_WAYS" => n
    case _ => throw new CDEMatchError
  })

class WithRV32 extends Config(
  (pname,site,here) => pname match {
    case XLen => 32
    case FPUKey => Some(FPUConfig(divSqrt = false))
    case _ => throw new CDEMatchError
  }
)

class WithBlockingL1 extends Config (
  knobValues = {
    case "L1D_MSHRS" => 0
    case _ => throw new CDEMatchError
  }
)

class WithSmallCores extends Config (
  topDefinitions = { (pname,site,here) => pname match {
    case MulDivKey => Some(MulDivConfig())
    case FPUKey => None
    case UseVM => false
    case NTLBEntries => 4
    case BtbKey => BtbParameters(nEntries = 0)
    case NAcquireTransactors => 2
    case _ => throw new CDEMatchError
  }},
  knobValues = {
    case "L1D_SETS" => 64
    case "L1D_WAYS" => 1
    case "L1I_SETS" => 64
    case "L1I_WAYS" => 1
    case "L1D_MSHRS" => 0
    case _ => throw new CDEMatchError
  }
)

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

class WithSplitL2Metadata extends Config(
  knobValues = { case "L2_SPLIT_METADATA" => true; case _ => throw new CDEMatchError })
