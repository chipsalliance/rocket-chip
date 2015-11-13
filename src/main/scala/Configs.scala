// See LICENSE for license details.

package rocketchip

import Chisel._
import junctions._
import uncore._
import rocket._
import rocket.Util._
import zscale._
import groundtest._
import scala.math.max
import DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob}

class DefaultConfig extends Config (
  topDefinitions = { (pname,site,here) => 
    type PF = PartialFunction[Any,Any]
    def findBy(sname:Any):Any = here[PF](site[Any](sname))(pname)
    def genCsrAddrMap: AddrMap = {
      val csrSize = (1 << 12) * (site(XLen) / 8)
      val csrs = (0 until site(NTiles)).map{ i => 
        AddrMapEntry(s"csr$i", None, MemSize(csrSize, AddrMapConsts.RW))
      }
      val scrSize = site(HtifKey).nSCR * (site(XLen) / 8)
      val scr = AddrMapEntry("scr", None, MemSize(scrSize, AddrMapConsts.RW))
      new AddrMap(csrs :+ scr)
    }
    pname match {
      case HtifKey => HtifParameters(
                       width = Dump("HTIF_WIDTH", 16),
                       nSCR = 64,
                       offsetBits = site(CacheBlockOffsetBits),
                       nCores = site(NTiles))
      //Memory Parameters
      case PAddrBits => 32
      case PgIdxBits => 12
      case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
      case PgLevelBits => site(PgIdxBits) - log2Up(site(XLen)/8)
      case VPNBits => site(PgLevels) * site(PgLevelBits)
      case PPNBits => site(PAddrBits) - site(PgIdxBits)
      case VAddrBits => site(VPNBits) + site(PgIdxBits)
      case ASIdBits => 7
      case MIFTagBits => // Bits needed at the L2 agent
                         log2Up(site(NAcquireTransactors)+2) +
                         // Bits added by NASTI interconnect
                         log2Up(site(NMemoryChannels) * site(NBanksPerMemoryChannel) + 1)
      case MIFDataBits => 64
      case MIFAddrBits => site(PAddrBits) - site(CacheBlockOffsetBits)
      case MIFDataBeats => site(CacheBlockBytes) * 8 / site(MIFDataBits)
      case NastiKey => {
        Dump("MEM_STRB_BITS", site(MIFDataBits) / 8)
        NastiParameters(
          dataBits = Dump("MEM_DATA_BITS", site(MIFDataBits)),
          addrBits = Dump("MEM_ADDR_BITS", site(PAddrBits)),
          idBits = Dump("MEM_ID_BITS", site(MIFTagBits)))
      }
      //Params used by all caches
      case NSets => findBy(CacheName)
      case NWays => findBy(CacheName)
      case RowBits => findBy(CacheName)
      case NTLBEntries => findBy(CacheName)
      case "L1I" => {
        case NSets => Knob("L1I_SETS") //64
        case NWays => Knob("L1I_WAYS") //4
        case RowBits => 4*site(CoreInstBits)
        case NTLBEntries => 8
      }:PF
      case "L1D" => {
        case NSets => Knob("L1D_SETS") //64
        case NWays => Knob("L1D_WAYS") //4
        case RowBits => 2*site(CoreDataBits)
        case NTLBEntries => 8
      }:PF
      case ECCCode => None
      case Replacer => () => new RandomReplacement(site(NWays))
      case AmoAluOperandBits => site(XLen)
      //L1InstCache
      case BtbKey => BtbParameters()
      //L1DataCache
      case WordBits => site(XLen)
      case StoreDataQueueDepth => 17
      case ReplayQueueDepth => 16
      case NMSHRs => Knob("L1D_MSHRS")
      case NIOMSHRs => 1
      case LRSCCycles => 32 
      //L2 Memory System Params
      case NAcquireTransactors => 7
      case L2StoreDataQueueDepth => 1
      case L2DirectoryRepresentation => new NullRepresentation(site(NTiles))
      case BuildL2CoherenceManager => (p: Parameters) =>
        Module(new L2BroadcastHub()(p.alterPartial({
          case InnerTLId => "L1toL2"
          case OuterTLId => "L2toMC" })))
      //Tile Constants
      case BuildTiles => {
        TestGeneration.addSuites(rv64i.map(_("p")))
        TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64u.map(_(env))))
        TestGeneration.addSuites(if(site(NTiles) > 1) List(mtBmarks, bmarks) else List(bmarks))
        List.fill(site(NTiles)){ (r: Bool, p: Parameters) =>
          Module(new RocketTile(resetSignal = r)(p.alterPartial({case TLId => "L1toL2"})))
        }
      }
      case BuildRoCC => None
      case RoccNMemChannels => 1
      //Rocket Core Constants
      case CoreName => "Rocket"
      case FetchWidth => 1
      case RetireWidth => 1
      case UseVM => true
      case UsePerfCounters => true
      case FastLoadWord => true
      case FastLoadByte => false
      case FastMulDiv => true
      case XLen => 64
      case UseFPU => {
        val env = if(site(UseVM)) List("p","pt","v") else List("p","pt")
        if(site(FDivSqrt)) TestGeneration.addSuites(env.map(rv64uf))
        else TestGeneration.addSuites(env.map(rv64ufNoDiv))
        true
      }
      case FDivSqrt => true
      case SFMALatency => 2
      case DFMALatency => 3
      case CoreInstBits => 32
      case CoreDataBits => site(XLen)
      case NCustomMRWCSRs => 0
      //Uncore Paramters
      case RTCPeriod => 100 // gives 10 MHz RTC assuming 1 GHz uncore clock
      case LNEndpoints => site(TLKey(site(TLId))).nManagers + site(TLKey(site(TLId))).nClients
      case LNHeaderBits => log2Ceil(site(TLKey(site(TLId))).nManagers) +
                             log2Up(site(TLKey(site(TLId))).nClients)
      case TLKey("L1toL2") => 
        TileLinkParameters(
          coherencePolicy = new MESICoherence(site(L2DirectoryRepresentation)),
          nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels),
          nCachingClients = site(NTiles),
          nCachelessClients = 1 + site(NTiles) *
                                (1 + (if(site(BuildRoCC).isEmpty) 0 else site(RoccNMemChannels))),
          maxClientXacts = max(site(NMSHRs) + site(NIOMSHRs),
                               if(site(BuildRoCC).isEmpty) 1 else site(RoccMaxTaggedMemXacts)),
          maxClientsPerPort = if(site(BuildRoCC).isEmpty) 1 else 2,
          maxManagerXacts = site(NAcquireTransactors) + 2,
          dataBits = site(CacheBlockBytes)*8)
      case TLKey("L2toMC") => 
        TileLinkParameters(
          coherencePolicy = new MEICoherence(new NullRepresentation(site(NBanksPerMemoryChannel))),
          nManagers = 1,
          nCachingClients = site(NBanksPerMemoryChannel),
          nCachelessClients = 0,
          maxClientXacts = 1,
          maxClientsPerPort = site(NAcquireTransactors) + 2,
          maxManagerXacts = 1,
          dataBits = site(CacheBlockBytes)*8)
      case TLKey("Outermost") => site(TLKey("L2toMC")).copy(dataBeats = site(MIFDataBeats))
      case NTiles => Knob("NTILES")
      case NMemoryChannels => Dump("N_MEM_CHANNELS", 1)
      case NBanksPerMemoryChannel => Knob("NBANKS_PER_MEM_CHANNEL")
      case NOutstandingMemReqsPerChannel => site(NBanksPerMemoryChannel)*(site(NAcquireTransactors)+2)
      case BankIdLSB => 0
      case CacheBlockBytes => Dump("CACHE_BLOCK_BYTES", 64)
      case CacheBlockOffsetBits => log2Up(here(CacheBlockBytes))
      case UseBackupMemoryPort => true
      case MMIOBase => Dump("MEM_SIZE", BigInt(1 << 30)) // 1 GB
      case ExternalIOStart => 2 * site(MMIOBase)
      case GlobalAddrMap => AddrMap(
        AddrMapEntry("mem", None, MemChannels(site(MMIOBase), site(NMemoryChannels), AddrMapConsts.RWX)),
        AddrMapEntry("conf", None, MemSubmap(site(ExternalIOStart) - site(MMIOBase), genCsrAddrMap)),
        AddrMapEntry("io", Some(site(ExternalIOStart)), MemSize(2 * site(MMIOBase), AddrMapConsts.RW)))
  }},
  knobValues = {
    case "NTILES" => 1
    case "NBANKS_PER_MEM_CHANNEL" => 1
    case "L1D_MSHRS" => 2
    case "L1D_SETS" => 64
    case "L1D_WAYS" => 4
    case "L1I_SETS" => 64
    case "L1I_WAYS" => 4
  }
)
class DefaultVLSIConfig extends DefaultConfig
class DefaultCPPConfig extends DefaultConfig

class With2Cores extends Config(knobValues = { case "NTILES" => 2 })
class With4Cores extends Config(knobValues = { case "NTILES" => 4 })
class With8Cores extends Config(knobValues = { case "NTILES" => 8 })

class With2BanksPerMemChannel extends Config(knobValues = { case "NBANKS_PER_MEM_CHANNEL" => 2 })
class With4BanksPerMemChannel extends Config(knobValues = { case "NBANKS_PER_MEM_CHANNEL" => 4 })
class With8BanksPerMemChannel extends Config(knobValues = { case "NBANKS_PER_MEM_CHANNEL" => 8 })

class With2MemoryChannels extends Config(
  (pname,site,here) => pname match {
    case NMemoryChannels => Dump("N_MEM_CHANNELS", 2)
  }
)
class With4MemoryChannels extends Config(
  (pname,site,here) => pname match {
    case NMemoryChannels => Dump("N_MEM_CHANNELS", 4)
  }
)

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
    }: PartialFunction[Any,Any] 
    case NAcquireTransactors => 2
    case NSecondaryMisses => 4
    case L2DirectoryRepresentation => new FullRepresentation(site(NTiles))
    case BuildL2CoherenceManager => (p: Parameters) =>
      Module(new L2HellaCacheBank()(p.alterPartial({
         case CacheName => "L2Bank"
         case InnerTLId => "L1toL2"
         case OuterTLId => "L2toMC"})))
  },
  knobValues = { case "L2_WAYS" => 8; case "L2_CAPACITY_IN_KB" => 2048 }
)

class WithL2Capacity2048 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 2048 })
class WithL2Capacity1024 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 1024 })
class WithL2Capacity512 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 512 })
class WithL2Capacity256 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 256 })
class WithL2Capacity128 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 128 })
class WithL2Capacity64 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 64 })

class DefaultL2Config extends Config(new WithL2Cache ++ new DefaultConfig)
class DefaultL2VLSIConfig extends Config(new WithL2Cache ++ new DefaultVLSIConfig)
class DefaultL2CPPConfig extends Config(new WithL2Cache ++ new DefaultCPPConfig)
class DefaultL2FPGAConfig extends Config(new WithL2Capacity64 ++ new WithL2Cache ++ new DefaultFPGAConfig)

class WithZscale extends Config(
  (pname,site,here) => pname match {
    case BuildZscale => {
      TestGeneration.addSuites(List(rv32ui("p"), rv32um("p")))
      TestGeneration.addSuites(List(zscaleBmarks))
      (r: Bool, p: Parameters) => Module(new Zscale(r)(p))
    }
    case BootROMCapacity => Dump("BOOT_CAPACITY", 16*1024)
    case DRAMCapacity => Dump("DRAM_CAPACITY", 64*1024*1024)
  }
)

class ZscaleConfig extends Config(new WithZscale ++ new DefaultConfig)

class WithMemtest extends Config (
  (pname, site, here) => pname match {
    case TLKey("L1toL2") => 
      TileLinkParameters(
        coherencePolicy = new MESICoherence(site(L2DirectoryRepresentation)),
        nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels),
        nCachingClients = site(NGeneratorTiles),
        nCachelessClients = site(NGeneratorTiles) + 1,
        maxClientXacts = 1,
        maxClientsPerPort = site(NGeneratorsPerTile),
        maxManagerXacts = site(NAcquireTransactors) + 2,
        dataBits = site(CacheBlockBytes)*8)

    case NTiles => site(NGeneratorTiles)

    case NGeneratorTiles => 1
    case NGeneratorsPerTile => 1
    case GenerateUncached => true
    case GenerateCached => true
    case MaxGenerateRequests => 128
    case GeneratorStartAddress => 0

    case BuildTiles => {
      (0 until site(NTiles)).map { i =>
        (r: Bool, p: Parameters) => Module(
          new GeneratorTile(i, r)
            (p.alterPartial({case TLId => "L1toL2"})))
      }
    }
  })

class MemtestConfig extends Config(new WithMemtest ++ new DefaultConfig)
class MemtestL2Config extends Config(new WithMemtest ++ new DefaultL2Config)

class FPGAConfig extends Config (
  (pname,site,here) => pname match {
    case NAcquireTransactors => 4
    case UseBackupMemoryPort => false
  }
)

class DefaultFPGAConfig extends Config(new FPGAConfig ++ new DefaultConfig)

class SmallConfig extends Config (
    topDefinitions = { (pname,site,here) => pname match {
      case UseFPU => false
      case FastMulDiv => false
      case NTLBEntries => 4
      case BtbKey => BtbParameters(nEntries = 8)
    }},
  knobValues = {
    case "L1D_SETS" => 64
    case "L1D_WAYS" => 1
    case "L1I_SETS" => 64
    case "L1I_WAYS" => 1
  }
)

class DefaultFPGASmallConfig extends Config(new SmallConfig ++ new DefaultFPGAConfig)

class ExampleSmallConfig extends Config(new SmallConfig ++ new DefaultConfig)

class MultibankConfig extends Config(new With2BanksPerMemChannel ++ new DefaultConfig)
class MultibankL2Config extends Config(
  new With2BanksPerMemChannel ++ new WithL2Cache ++ new DefaultConfig)

class MultichannelConfig extends Config(new With2MemoryChannels ++ new DefaultConfig)
