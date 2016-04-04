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

object ConfigUtils {
  def max_int(values: Int*): Int = {
    values.reduce((a, b) => max(a, b))
  }
}
import ConfigUtils._

class DefaultConfig extends Config (
  topDefinitions = { (pname,site,here) => 
    type PF = PartialFunction[Any,Any]
    def findBy(sname:Any):Any = here[PF](site[Any](sname))(pname)
    def genCsrAddrMap: AddrMap = {
      val deviceTree = AddrMapEntry("devicetree", None, MemSize(1 << 15, AddrMapConsts.R))
      val csrSize = (1 << 12) * (site(XLen) / 8)
      val csrs = (0 until site(NTiles)).map{ i => 
        AddrMapEntry(s"csr$i", None, MemSize(csrSize, AddrMapConsts.RW))
      }
      val scrSize = site(HtifKey).nSCR * (site(XLen) / 8)
      val scr = AddrMapEntry("scr", None, MemSize(scrSize, AddrMapConsts.RW))
      new AddrMap(deviceTree +: csrs :+ scr)
    }
    def makeConfigString() = {
      val addrMap = new AddrHashMap(site(GlobalAddrMap), site(MMIOBase))
      val xLen = site(XLen)
      val res = new StringBuilder
      res append  "platform {\n"
      res append  "  vendor ucb;\n"
      res append  "  arch rocket;\n"
      res append  "};\n"
      res append  "ram {\n"
      res append  "  0 {\n"
      res append  "    addr 0;\n"
      res append s"    size 0x${site(MMIOBase).toString(16)};\n"
      res append  "  };\n"
      res append  "};\n"
      res append  "core {\n"
      for (i <- 0 until site(NTiles)) {
        val csrAddr = addrMap(s"conf:csr$i").start
        res append s"  $i {\n"
        res append  "    0 {\n"
        res append s"      isa rv$xLen;\n"
        res append s"      addr 0x${csrAddr.toString(16)};\n"
        res append  "    };\n"
        res append  "  };\n"
      }
      res append  "};\n"
      res append '\u0000'
      res.toString.getBytes
    }
    pname match {
      case HtifKey => HtifParameters(
                       width = Dump("HTIF_WIDTH", 16),
                       nSCR = 64,
                       csrDataBits = site(XLen),
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
      case MIFTagBits => Dump("MIF_TAG_BITS",
                         // Bits needed at the L2 agent
                         log2Up(site(NAcquireTransactors)+2) +
                         // Bits added by NASTI interconnect
                         log2Up(site(MaxBanksPerMemoryChannel)))
      case MIFDataBits => Dump("MIF_DATA_BITS", 64)
      case MIFAddrBits => Dump("MIF_ADDR_BITS",
                               site(PAddrBits) - site(CacheBlockOffsetBits))
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
      case CacheIdBits => findBy(CacheName)
      case SplitMetadata => findBy(CacheName)
      case ICacheBufferWays => Knob("L1I_BUFFER_WAYS")
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
      case AmoAluOperandBits => site(XLen)
      //L1InstCache
      case BtbKey => BtbParameters()
      //L1DataCache
      case WordBits => site(XLen)
      case StoreDataQueueDepth => 17
      case ReplayQueueDepth => 16
      case NMSHRs => Knob("L1D_MSHRS")
      case LRSCCycles => 32 
      //L2 Memory System Params
      case NAcquireTransactors => 7
      case L2StoreDataQueueDepth => 1
      case L2DirectoryRepresentation => new NullRepresentation(site(NTiles))
      case BuildL2CoherenceManager => (id: Int, p: Parameters) =>
        Module(new L2BroadcastHub()(p.alterPartial({
          case InnerTLId => "L1toL2"
          case OuterTLId => "L2toMC" })))
      //Tile Constants
      case BuildTiles => {
        val (rvi, rvu) =
          if (site(XLen) == 64) (rv64i, rv64u)
          else (rv32i, rv32u)
        TestGeneration.addSuites(rvi.map(_("p")))
        TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rvu.map(_(env))))
        TestGeneration.addSuite(bmarks)
        List.fill(site(NTiles)){ (r: Bool, p: Parameters) =>
          Module(new RocketTile(resetSignal = r)(p.alterPartial({case TLId => "L1toL2"})))
        }
      }
      case BuildRoCC => Nil
      case RoccNMemChannels => site(BuildRoCC).map(_.nMemChannels).foldLeft(0)(_ + _)
      case RoccNPTWPorts => site(BuildRoCC).map(_.nPTWPorts).foldLeft(0)(_ + _)
      case RoccNCSRs => site(BuildRoCC).map(_.csrs.size).foldLeft(0)(_ + _)
      case UseStreamLoopback => false
      case NDmaTransactors => 3
      case NDmaXacts => site(NDmaTransactors) * site(NTiles)
      case NDmaClients => site(NTiles)
      //Rocket Core Constants
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
      case ResetVector => BigInt(0x0)
      case MtvecInit => BigInt(0x8)
      case MtvecWritable => false
      //Uncore Paramters
      case RTCPeriod => 100 // gives 10 MHz RTC assuming 1 GHz uncore clock
      case LNEndpoints => site(TLKey(site(TLId))).nManagers + site(TLKey(site(TLId))).nClients
      case LNHeaderBits => log2Ceil(site(TLKey(site(TLId))).nManagers) +
                             log2Up(site(TLKey(site(TLId))).nClients)
      case ExtraL1Clients => 2 // RTC and HTIF
      case TLKey("L1toL2") => 
        TileLinkParameters(
          coherencePolicy = new MESICoherence(site(L2DirectoryRepresentation)),
          nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels) + 1,
          nCachingClients = site(NTiles),
          nCachelessClients = site(ExtraL1Clients) +
                              site(NTiles) *
                                (1 + (if(site(BuildRoCC).isEmpty) 0
                                      else site(RoccNMemChannels))),
          maxClientXacts = max_int(
              // L1 cache
              site(NMSHRs) + 1,
              // RoCC
              if (site(BuildRoCC).isEmpty) 1 else site(RoccMaxTaggedMemXacts),
              // RTC
              site(NTiles)),
          maxClientsPerPort = if (site(BuildRoCC).isEmpty) 1 else 2,
          maxManagerXacts = site(NAcquireTransactors) + 2,
          dataBits = site(CacheBlockBytes)*8)
      case TLKey("L2toMC") => 
        TileLinkParameters(
          coherencePolicy = new MEICoherence(
            new NullRepresentation(site(NBanksPerMemoryChannel))),
          nManagers = 1,
          nCachingClients = site(NBanksPerMemoryChannel),
          nCachelessClients = 0,
          maxClientXacts = 1,
          maxClientsPerPort = site(NAcquireTransactors) + 2,
          maxManagerXacts = 1,
          dataBits = site(CacheBlockBytes)*8)
      case TLKey("Outermost") => site(TLKey("L2toMC")).copy(
        maxClientXacts = site(NAcquireTransactors) + 2,
        maxClientsPerPort = site(MaxBanksPerMemoryChannel),
        dataBeats = site(MIFDataBeats))
      case TLKey("L2toMMIO") => {
        val addrMap = new AddrHashMap(site(GlobalAddrMap), site(MMIOBase))
        TileLinkParameters(
          coherencePolicy = new MICoherence(
            new NullRepresentation(site(NBanksPerMemoryChannel))),
          nManagers = addrMap.nEntries,
          nCachingClients = 0,
          nCachelessClients = 1,
          maxClientXacts = 4,
          maxClientsPerPort = 1,
          maxManagerXacts = 1,
          dataBits = site(CacheBlockBytes) * 8)
      }
      case TLKey("MMIO_Outermost") => site(TLKey("L2toMMIO")).copy(dataBeats = site(MIFDataBeats))
      case NTiles => Knob("NTILES")
      case NMemoryChannels => Dump("N_MEM_CHANNELS", 1)
      case NBanksPerMemoryChannel => Knob("NBANKS_PER_MEM_CHANNEL")
      case MemoryChannelMuxConfigs => Dump("MEMORY_CHANNEL_MUX_CONFIGS", List( site(NMemoryChannels) ))
      case MaxBanksPerMemoryChannel => site(NBanksPerMemoryChannel) * site(NMemoryChannels) / site(MemoryChannelMuxConfigs).sortWith{_ < _}(0)
      case NOutstandingMemReqsPerChannel => site(MaxBanksPerMemoryChannel)*(site(NAcquireTransactors)+2)
      case BankIdLSB => 0
      case CacheBlockBytes => Dump("CACHE_BLOCK_BYTES", 64)
      case CacheBlockOffsetBits => log2Up(here(CacheBlockBytes))
      case UseBackupMemoryPort => false
      case UseHtifClockDiv => true
      case MMIOBase => Dump("MEM_SIZE", BigInt(1L << 30)) // 1 GB
      case ConfigString => makeConfigString()
      case GlobalAddrMap => {
        AddrMap(
          AddrMapEntry("conf", None,
            MemSubmap(BigInt(1L << 30), genCsrAddrMap)),
          AddrMapEntry("devices", None,
            MemSubmap(BigInt(1L << 31), site(GlobalDeviceSet).getAddrMap)))
      }
      case GlobalDeviceSet => {
        val devset = new DeviceSet
        if (site(UseStreamLoopback)) {
          devset.addDevice("loopback", site(StreamLoopbackWidth) / 8, "stream")
        }
        devset
      }
  }},
  knobValues = {
    case "NTILES" => 1
    case "NBANKS_PER_MEM_CHANNEL" => 1
    case "L1D_MSHRS" => 2
    case "L1D_SETS" => 64
    case "L1D_WAYS" => 4
    case "L1I_SETS" => 64
    case "L1I_WAYS" => 4
    case "L1I_BUFFER_WAYS" => false
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
class With8MemoryChannels extends Config(
  (pname,site,here) => pname match {
    case NMemoryChannels => Dump("N_MEM_CHANNELS", 8)
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
  },
  knobValues = { case "L2_WAYS" => 8; case "L2_CAPACITY_IN_KB" => 2048; case "L2_SPLIT_METADATA" => false }
)

class WithPLRU extends Config(
  (pname, site, here) => pname match {
    case L2Replacer => () => new SeqPLRU(site(NSets), site(NWays))
  })

class WithL2Capacity2048 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 2048 })
class WithL2Capacity1024 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 1024 })
class WithL2Capacity512 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 512 })
class WithL2Capacity256 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 256 })
class WithL2Capacity128 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 128 })
class WithL2Capacity64 extends Config(knobValues = { case "L2_CAPACITY_IN_KB" => 64 })

class With1L2Ways extends Config(knobValues = { case "L2_WAYS" => 1 })
class With2L2Ways extends Config(knobValues = { case "L2_WAYS" => 2 })
class With4L2Ways extends Config(knobValues = { case "L2_WAYS" => 4 })

class DefaultL2Config extends Config(new WithL2Cache ++ new DefaultConfig)
class DefaultL2VLSIConfig extends Config(new WithL2Cache ++ new DefaultVLSIConfig)
class DefaultL2CPPConfig extends Config(new WithL2Cache ++ new DefaultCPPConfig)
class DefaultL2FPGAConfig extends Config(new WithL2Capacity64 ++ new WithL2Cache ++ new DefaultFPGAConfig)

class PLRUL2Config extends Config(new WithPLRU ++ new DefaultL2Config)

class WithZscale extends Config(
  (pname,site,here) => pname match {
    case XLen => 32
    case UseFPU => false
    case BuildZscale => {
      TestGeneration.addSuites(List(rv32ui("p"), rv32um("p")))
      TestGeneration.addSuites(List(zscaleBmarks))
      (r: Bool, p: Parameters) => Module(new Zscale(r)(p))
    }
    case BootROMCapacity => Dump("BOOT_CAPACITY", 16*1024)
    case DRAMCapacity => Dump("DRAM_CAPACITY", 64*1024*1024)
  }
)

class WithRV32 extends Config(
  (pname,site,here) => pname match {
    case XLen => 32
    case UseVM => false
    case UseFPU => false
  }
)

class ZscaleConfig extends Config(new WithZscale ++ new DefaultConfig)

class FPGAConfig extends Config (
  (pname,site,here) => pname match {
    case NAcquireTransactors => 4
    case UseHtifClockDiv => false
  }
)

class DefaultFPGAConfig extends Config(new FPGAConfig ++ new DefaultConfig)

class SmallConfig extends Config (
    topDefinitions = { (pname,site,here) => pname match {
      case UseFPU => false
      case FastMulDiv => false
      case NTLBEntries => 4
      case BtbKey => BtbParameters(nEntries = 0)
      case StoreDataQueueDepth => 2
      case ReplayQueueDepth => 2
      case NAcquireTransactors => 2
    }},
  knobValues = {
    case "L1D_SETS" => 64
    case "L1D_WAYS" => 1
    case "L1I_SETS" => 64
    case "L1I_WAYS" => 1
    case "L1D_MSHRS" => 1
  }
)

class DefaultFPGASmallConfig extends Config(new SmallConfig ++ new DefaultFPGAConfig)

class DefaultRV32Config extends Config(new SmallConfig ++ new WithRV32 ++ new DefaultConfig)

class ExampleSmallConfig extends Config(new SmallConfig ++ new DefaultConfig)

class DualBankConfig extends Config(new With2BanksPerMemChannel ++ new DefaultConfig)
class DualBankL2Config extends Config(
  new With2BanksPerMemChannel ++ new WithL2Cache ++ new DefaultConfig)

class DualChannelConfig extends Config(new With2MemoryChannels ++ new DefaultConfig)
class DualChannelL2Config extends Config(
  new With2MemoryChannels ++ new WithL2Cache ++ new DefaultConfig)

class DualChannelDualBankConfig extends Config(
  new With2MemoryChannels ++ new With2BanksPerMemChannel ++ new DefaultConfig)
class DualChannelDualBankL2Config extends Config(
  new With2MemoryChannels ++ new With2BanksPerMemChannel ++
  new WithL2Cache ++ new DefaultConfig)

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
  })

class RoccExampleConfig extends Config(new WithRoccExample ++ new DefaultConfig)

class WithDmaController extends Config(
  (pname, site, here) => pname match {
    case BuildRoCC => Seq(
        RoccParameters(
          opcodes = OpcodeSet.custom2,
          generator = (p: Parameters) => Module(new DmaController()(p)),
          nPTWPorts = 1,
          csrs = Seq.range(
            DmaCtrlRegNumbers.CSR_BASE,
            DmaCtrlRegNumbers.CSR_END)))
    case RoccMaxTaggedMemXacts => 1
  })

class WithStreamLoopback extends Config(
  (pname, site, here) => pname match {
    case UseStreamLoopback => true
    case StreamLoopbackSize => 128
    case StreamLoopbackWidth => 64
  })

class DmaControllerConfig extends Config(new WithDmaController ++ new WithStreamLoopback ++ new DefaultL2Config)
class DmaControllerFPGAConfig extends Config(new WithDmaController ++ new WithStreamLoopback ++ new DefaultFPGAConfig)

class SmallL2Config extends Config(
  new With2MemoryChannels ++ new With4BanksPerMemChannel ++
  new WithL2Capacity256 ++ new DefaultL2Config)

class SingleChannelBenchmarkConfig extends Config(new WithL2Capacity256 ++ new DefaultL2Config)
class DualChannelBenchmarkConfig extends Config(new With2MemoryChannels ++ new SingleChannelBenchmarkConfig)
class QuadChannelBenchmarkConfig extends Config(new With4MemoryChannels ++ new SingleChannelBenchmarkConfig)
class OctoChannelBenchmarkConfig extends Config(new With8MemoryChannels ++ new SingleChannelBenchmarkConfig)

class EightChannelVLSIConfig extends Config(new With8MemoryChannels ++ new DefaultVLSIConfig)

class WithOneOrMaxChannels extends Config(
  (pname, site, here) => pname match {
    case MemoryChannelMuxConfigs => Dump("MEMORY_CHANNEL_MUX_CONFIGS", List(1, site(NMemoryChannels)))
  }
)
class OneOrEightChannelBenchmarkConfig extends Config(new WithOneOrMaxChannels ++ new With8MemoryChannels ++ new SingleChannelBenchmarkConfig)
class OneOrEightChannelVLSIConfig extends Config(new WithOneOrMaxChannels ++ new EightChannelVLSIConfig)

class SimulateBackupMemConfig extends Config(){ Dump("MEM_BACKUP_EN", true) }
class BackupMemVLSIConfig extends Config(new SimulateBackupMemConfig ++ new DefaultVLSIConfig)
class OneOrEightChannelBackupMemVLSIConfig extends Config(new WithOneOrMaxChannels ++ new With8MemoryChannels ++ new BackupMemVLSIConfig)

class WithSplitL2Metadata extends Config(knobValues = { case "L2_SPLIT_METADATA" => true })
class SplitL2MetadataTestConfig extends Config(new WithSplitL2Metadata ++ new DefaultL2Config)

class DualCoreConfig extends Config(new With2Cores ++ new DefaultConfig)
