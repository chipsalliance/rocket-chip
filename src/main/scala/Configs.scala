// See LICENSE for license details.

package rocketchip

import Chisel._
import junctions._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.converters._
import rocket._
import rocket.Util._
import groundtest._
import scala.math.max
import scala.collection.mutable.ListBuffer
import DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}

object ConfigUtils {
  def max_int(values: Int*): Int = {
    values.reduce((a, b) => max(a, b))
  }
}
import ConfigUtils._

class BaseConfig extends Config (
  topDefinitions = { (pname,site,here) => 
    type PF = PartialFunction[Any,Any]
    def findBy(sname:Any):Any = here[PF](site[Any](sname))(pname)
    lazy val internalIOAddrMap: AddrMap = {
      val entries = collection.mutable.ArrayBuffer[AddrMapEntry]()
      entries += AddrMapEntry("debug", MemSize(4096, MemAttr(AddrMapProt.RWX)))
      entries += AddrMapEntry("bootrom", MemSize(4096, MemAttr(AddrMapProt.RX)))
      entries += AddrMapEntry("plic", MemRange(0x40000000, 0x4000000, MemAttr(AddrMapProt.RW)))
      entries += AddrMapEntry("prci", MemSize(0x4000000, MemAttr(AddrMapProt.RW)))
      new AddrMap(entries)
    }
    lazy val globalAddrMap = {
      val memBase = 0x80000000L
      val memSize = 0x10000000L
      val io = new AddrMap(AddrMapEntry("int", internalIOAddrMap) +: site(ExtMMIOPorts).entries)
      val addrMap = AddrMap(
        AddrMapEntry("io", io),
        AddrMapEntry("mem", MemRange(memBase, memSize, MemAttr(AddrMapProt.RWX, true))))

      Dump("MEM_BASE", addrMap("mem").start)
      Dump("MEM_SIZE", memSize)
      addrMap
    }
    def makeConfigString() = {
      val addrMap = globalAddrMap
      val plicAddr = addrMap("io:int:plic").start
      val prciAddr = addrMap("io:int:prci").start
      val plicInfo = site(PLICKey)
      val xLen = site(XLen)
      val res = new StringBuilder
      res append  "plic {\n"
      res append s"  priority 0x${plicAddr.toString(16)};\n"
      res append s"  pending 0x${(plicAddr + plicInfo.pendingBase).toString(16)};\n"
      res append s"  ndevs ${plicInfo.nDevices};\n"
      res append  "};\n"
      res append  "rtc {\n"
      res append s"  addr 0x${(prciAddr + PRCI.time).toString(16)};\n"
      res append  "};\n"
      res append  "ram {\n"
      res append  "  0 {\n"
      res append s"    addr 0x${addrMap("mem").start.toString(16)};\n"
      res append s"    size 0x${addrMap("mem").size.toString(16)};\n"
      res append  "  };\n"
      res append  "};\n"
      res append  "core {\n"
      for (i <- 0 until site(NTiles)) {
        val isa = s"rv${site(XLen)}im${if (site(UseAtomics)) "a" else ""}${if (site(UseFPU)) "fd" else ""}"
        res append s"  $i {\n"
        res append  "    0 {\n"
        res append s"      isa $isa;\n"
        res append s"      timecmp 0x${(prciAddr + PRCI.timecmp(i)).toString(16)};\n"
        res append s"      ipi 0x${(prciAddr + PRCI.msip(i)).toString(16)};\n"
        res append s"      plic {\n"
        res append s"        m {\n"
        res append s"         ie 0x${(plicAddr + plicInfo.enableAddr(i, 'M')).toString(16)};\n"
        res append s"         thresh 0x${(plicAddr + plicInfo.threshAddr(i, 'M')).toString(16)};\n"
        res append s"         claim 0x${(plicAddr + plicInfo.claimAddr(i, 'M')).toString(16)};\n"
        res append s"        };\n"
        if (site(UseVM)) {
          res append s"        s {\n"
          res append s"         ie 0x${(plicAddr + plicInfo.enableAddr(i, 'S')).toString(16)};\n"
          res append s"         thresh 0x${(plicAddr + plicInfo.threshAddr(i, 'S')).toString(16)};\n"
          res append s"         claim 0x${(plicAddr + plicInfo.claimAddr(i, 'S')).toString(16)};\n"
          res append s"        };\n"
        }
        res append s"      };\n"
        res append  "    };\n"
        res append  "  };\n"
      }
      res append  "};\n"
      res append '\u0000'
      res.toString.getBytes
    }
    lazy val innerDataBits = 64
    lazy val innerDataBeats = (8 * site(CacheBlockBytes)) / innerDataBits
    pname match {
      //Memory Parameters
      case PAddrBits => 32
      case PgIdxBits => 12
      case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
      case PgLevelBits => site(PgIdxBits) - log2Up(site(XLen)/8)
      case VPNBits => site(PgLevels) * site(PgLevelBits)
      case PPNBits => site(PAddrBits) - site(PgIdxBits)
      case VAddrBits => site(VPNBits) + site(PgIdxBits)
      case ASIdBits => 7
      case MIFTagBits => Dump("MIF_TAG_BITS", 5)
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
      case NCachedTileLinkPorts => 1
      case NUncachedTileLinkPorts => 1
      //Tile Constants
      case BuildTiles => {
        val (rvi, rvu) =
          if (site(XLen) == 64) ((if (site(UseVM)) rv64i else rv64pi), rv64u)
          else ((if (site(UseVM)) rv32i else rv32pi), rv32u)
        TestGeneration.addSuites(rvi.map(_("p")))
        TestGeneration.addSuites((if(site(UseVM)) List("v") else List()).flatMap(env => rvu.map(_(env))))
        TestGeneration.addSuite(if (site(UseVM)) benchmarks else emptyBmarks)
        List.fill(site(NTiles)){ (r: Bool, p: Parameters) =>
          Module(new RocketTile(resetSignal = r)(p.alterPartial({
            case TLId => "L1toL2"
            case NUncachedTileLinkPorts => 1 + site(RoccNMemChannels)
          })))
        }
      }
      case BuildRoCC => Nil
      case RoccNMemChannels => site(BuildRoCC).map(_.nMemChannels).foldLeft(0)(_ + _)
      case RoccNPTWPorts => site(BuildRoCC).map(_.nPTWPorts).foldLeft(0)(_ + _)
      case RoccNCSRs => site(BuildRoCC).map(_.csrs.size).foldLeft(0)(_ + _)
      //Rocket Core Constants
      case FetchWidth => if (site(UseCompressed)) 2 else 1
      case RetireWidth => 1
      case UseVM => true
      case UseUser => true
      case UseDebug => true
      case AsyncDebugBus => false
      case NBreakpoints => 1
      case UsePerfCounters => true
      case FastLoadWord => true
      case FastLoadByte => false
      case MulUnroll => 8
      case DivEarlyOut => true
      case XLen => 64
      case UseFPU => {
        val env = if(site(UseVM)) List("p","v") else List("p")
        TestGeneration.addSuite(rv32udBenchmarks)
        if(site(FDivSqrt)) {
          TestGeneration.addSuites(env.map(rv64uf))
          TestGeneration.addSuites(env.map(rv64ud))
        } else {
          TestGeneration.addSuites(env.map(rv64ufNoDiv))
          TestGeneration.addSuites(env.map(rv64udNoDiv))
        }
        true
      }
      case UseAtomics => {
        val env = if(site(UseVM)) List("p","v") else List("p")
        TestGeneration.addSuites(env.map(if (site(XLen) == 64) rv64ua else rv32ua))
        true
      }
      case UseCompressed => {
        val env = if(site(UseVM)) List("p","v") else List("p")
        TestGeneration.addSuites(env.map(if (site(XLen) == 64) rv64uc else rv32uc))
        true
      }
      case NExtInterrupts => 2
      case AsyncMMIOChannels => false
      case ExtMMIOPorts => AddrMap()
/*
        AddrMap(
          AddrMapEntry("cfg", MemRange(0x50000000L, 0x04000000L, MemAttr(AddrMapProt.RW))),
          AddrMapEntry("ext", MemRange(0x60000000L, 0x20000000L, MemAttr(AddrMapProt.RWX))))
*/
      case NExtMMIOAXIChannels => 0
      case NExtMMIOAHBChannels => 0
      case NExtMMIOTLChannels  => 0
      case AsyncBusChannels => false
      case NExtBusAXIChannels => 0
      case PLICKey => PLICConfig(site(NTiles), site(UseVM), site(NExtInterrupts), 0)
      case DMKey => new DefaultDebugModuleConfig(site(NTiles), site(XLen))
      case FDivSqrt => true
      case SFMALatency => 2
      case DFMALatency => 3
      case CoreInstBits => if (site(UseCompressed)) 16 else 32
      case CoreDataBits => site(XLen)
      case NCustomMRWCSRs => 0
      case ResetVector => BigInt(0x1000)
      case MtvecInit => BigInt(0x1010)
      case MtvecWritable => true
      //Uncore Paramters
      case RTCPeriod => 100 // gives 10 MHz RTC assuming 1 GHz uncore clock
      case LNEndpoints => site(TLKey(site(TLId))).nManagers + site(TLKey(site(TLId))).nClients
      case LNHeaderBits => log2Ceil(site(TLKey(site(TLId))).nManagers) +
                             log2Up(site(TLKey(site(TLId))).nClients)
      case HastiId => "Ext"
      case HastiKey("TL") =>
        HastiParameters(
          addrBits = site(PAddrBits),
          dataBits = site(TLKey(site(TLId))).dataBits / site(TLKey(site(TLId))).dataBeats)
      case HastiKey("Ext") =>
        HastiParameters(
          addrBits = site(PAddrBits),
          dataBits = site(XLen))
      case TLKey("L1toL2") => {
        val useMEI = site(NTiles) <= 1 && site(NCachedTileLinkPorts) <= 1
        TileLinkParameters(
          coherencePolicy = (
            if (useMEI) new MEICoherence(site(L2DirectoryRepresentation))
            else new MESICoherence(site(L2DirectoryRepresentation))),
          nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels) + 1 /* MMIO */,
          nCachingClients = site(NCachedTileLinkPorts),
          nCachelessClients = site(NExtBusAXIChannels) + site(NUncachedTileLinkPorts),
          maxClientXacts = max_int(
              // L1 cache
              site(NMSHRs) + 1 /* IOMSHR */,
              // RoCC
              if (site(BuildRoCC).isEmpty) 1 else site(RoccMaxTaggedMemXacts)),
          maxClientsPerPort = if (site(BuildRoCC).isEmpty) 1 else 2,
          maxManagerXacts = site(NAcquireTransactors) + 2,
          dataBeats = innerDataBeats,
          dataBits = site(CacheBlockBytes)*8)
      }
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
          dataBeats = innerDataBeats,
          dataBits = site(CacheBlockBytes)*8)
      case TLKey("Outermost") => site(TLKey("L2toMC")).copy(
        maxClientXacts = site(NAcquireTransactors) + 2,
        maxClientsPerPort = site(NBanksPerMemoryChannel),
        dataBeats = site(MIFDataBeats))
      case TLKey("L2toMMIO") => {
        TileLinkParameters(
          coherencePolicy = new MICoherence(
            new NullRepresentation(site(NBanksPerMemoryChannel))),
          nManagers = globalAddrMap.subMap("io").flatten.size,
          nCachingClients = 0,
          nCachelessClients = 1,
          maxClientXacts = 4,
          maxClientsPerPort = 1,
          maxManagerXacts = 1,
          dataBeats = innerDataBeats,
          dataBits = site(CacheBlockBytes) * 8)
      }
      case TLKey("MMIO_Outermost") => site(TLKey("L2toMMIO")).copy(dataBeats = site(MIFDataBeats))
      case NTiles => Knob("NTILES")
      case AsyncMemChannels => false
      case NMemoryChannels => Dump("N_MEM_CHANNELS", 1)
      case TMemoryChannels => BusType.AXI
      case NBanksPerMemoryChannel => Knob("NBANKS_PER_MEM_CHANNEL")
      case NOutstandingMemReqsPerChannel => site(NBanksPerMemoryChannel)*(site(NAcquireTransactors)+2)
      case BankIdLSB => 0
      case CacheBlockBytes => Dump("CACHE_BLOCK_BYTES", 64)
      case CacheBlockOffsetBits => log2Up(here(CacheBlockBytes))
      case ConfigString => makeConfigString()
      case GlobalAddrMap => globalAddrMap
      case EnableL2Logging => false
      case ExportGroundTestStatus => false
      case _ => throw new CDEMatchError
  }},
  knobValues = {
    case "NTILES" => 1
    case "NBANKS_PER_MEM_CHANNEL" => 1
    case "L1D_MSHRS" => 2
    case "L1D_SETS" => 64
    case "L1D_WAYS" => 4
    case "L1I_SETS" => 64
    case "L1I_WAYS" => 4
    case _ => throw new CDEMatchError
  }
)
class DefaultConfig extends Config(new WithBlockingL1 ++ new BaseConfig)

class WithNCores(n: Int) extends Config(
  knobValues = { case"NTILES" => n; case _ => throw new CDEMatchError })

class WithNBanksPerMemChannel(n: Int) extends Config(
  knobValues = {
    case "NBANKS_PER_MEM_CHANNEL" => n;
    case _ => throw new CDEMatchError
  })

class WithNMemoryChannels(n: Int) extends Config(
  (pname,site,here) => pname match {
    case NMemoryChannels => Dump("N_MEM_CHANNELS", n)
    case _ => throw new CDEMatchError
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

class DefaultL2Config extends Config(new WithL2Cache ++ new BaseConfig)
class DefaultL2FPGAConfig extends Config(
  new WithL2Capacity(64) ++ new WithL2Cache ++ new DefaultFPGAConfig)

class DefaultBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new BaseConfig)

class PLRUL2Config extends Config(new WithPLRU ++ new DefaultL2Config)

class WithRV32 extends Config(
  (pname,site,here) => pname match {
    case XLen => 32
    case UseVM => false
    case UseUser => false
    case UseAtomics => false
    case UseFPU => false
    case _ => throw new CDEMatchError
  }
)

class FPGAConfig extends Config (
  (pname,site,here) => pname match {
    case NAcquireTransactors => 4
    case ExportGroundTestStatus => true
    case _ => throw new CDEMatchError
  }
)

class WithBlockingL1 extends Config (
  knobValues = {
    case "L1D_MSHRS" => 0
    case _ => throw new CDEMatchError
  }
)

class WithAHB extends Config(
  (pname, site, here) => pname match {
    case TMemoryChannels     => BusType.AHB
    case NExtMMIOAHBChannels => 1
  })

class WithTL extends Config(
  (pname, site, here) => pname match {
    case TMemoryChannels     => BusType.TL
    case NExtMMIOTLChannels  => 1
  })

class DefaultFPGAConfig extends Config(new FPGAConfig ++ new BaseConfig)

class WithSmallCores extends Config (
    topDefinitions = { (pname,site,here) => pname match {
      case UseFPU => false
      case MulUnroll => 1
      case DivEarlyOut => false  
      case NTLBEntries => 4
      case BtbKey => BtbParameters(nEntries = 0)
      case StoreDataQueueDepth => 2
      case ReplayQueueDepth => 2
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

class DefaultFPGASmallConfig extends Config(new WithSmallCores ++ new DefaultFPGAConfig)
class DefaultSmallConfig extends Config(new WithSmallCores ++ new BaseConfig)
class DefaultRV32Config extends Config(new WithRV32 ++ new DefaultSmallConfig)

class DualBankConfig extends Config(
  new WithNBanksPerMemChannel(2) ++ new BaseConfig)
class DualBankL2Config extends Config(
  new WithNBanksPerMemChannel(2) ++ new WithL2Cache ++ new BaseConfig)

class DualChannelConfig extends Config(new WithNMemoryChannels(2) ++ new BaseConfig)
class DualChannelL2Config extends Config(
  new WithNMemoryChannels(2) ++ new WithL2Cache ++ new BaseConfig)

class DualChannelDualBankConfig extends Config(
  new WithNMemoryChannels(2) ++
  new WithNBanksPerMemChannel(2) ++ new BaseConfig)
class DualChannelDualBankL2Config extends Config(
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(2) ++
  new WithL2Cache ++ new BaseConfig)

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

class RoccExampleConfig extends Config(new WithRoccExample ++ new BaseConfig)

class WithMIFDataBits(n: Int) extends Config(
  (pname, site, here) => pname match {
    case MIFDataBits => Dump("MIF_DATA_BITS", n)
  })

class MIF128BitConfig extends Config(
  new WithMIFDataBits(128) ++ new BaseConfig)
class MIF32BitConfig extends Config(
  new WithMIFDataBits(32) ++ new BaseConfig)

class WithStreamLoopback extends Config(
  (pname, site, here) => pname match {
    case UseStreamLoopback => true
    case StreamLoopbackSize => 128
    case StreamLoopbackWidth => 64
    case _ => throw new CDEMatchError
  })

class SmallL2Config extends Config(
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new WithL2Capacity(256) ++ new DefaultL2Config)

class SingleChannelBenchmarkConfig extends Config(new WithL2Capacity(256) ++ new DefaultL2Config)
class DualChannelBenchmarkConfig extends Config(new WithNMemoryChannels(2) ++ new SingleChannelBenchmarkConfig)
class QuadChannelBenchmarkConfig extends Config(new WithNMemoryChannels(4) ++ new SingleChannelBenchmarkConfig)
class OctoChannelBenchmarkConfig extends Config(new WithNMemoryChannels(8) ++ new SingleChannelBenchmarkConfig)

class EightChannelConfig extends Config(new WithNMemoryChannels(8) ++ new BaseConfig)

class WithSplitL2Metadata extends Config(knobValues = { case "L2_SPLIT_METADATA" => true; case _ => throw new CDEMatchError })
class SplitL2MetadataTestConfig extends Config(new WithSplitL2Metadata ++ new DefaultL2Config)

class DualCoreConfig extends Config(
  new WithNCores(2) ++ new WithL2Cache ++ new BaseConfig)

class TinyConfig extends Config(
  new WithRV32 ++ new WithSmallCores ++
  new WithStatelessBridge ++ new BaseConfig)
