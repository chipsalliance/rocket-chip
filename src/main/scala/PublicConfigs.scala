// See LICENSE for license details.

package rocketchip

import Chisel._
import uncore._
import rocket._
import rocket.Util._

class DefaultConfig extends ChiselConfig (
  topDefinitions = { (pname,site,here) => 
    type PF = PartialFunction[Any,Any]
    def findBy(sname:Any):Any = here[PF](site[Any](sname))(pname)
    pname match {
      //RocketChip Parameters
      case BuildTile => (r:Bool) => {new RocketTile(resetSignal = r)}
      //HTIF Parameters
      case HTIFWidth => Dump("HTIF_WIDTH", 16)
      case HTIFNSCR => 64
      case HTIFOffsetBits => site(CacheBlockOffsetBits)
      case HTIFNCores => site(NTiles)
      //Memory Parameters
      case PAddrBits => 32
      case VAddrBits => 43
      case PgIdxBits => 13
      case ASIdBits => 7
      case PermBits => 6
      case PPNBits => site(PAddrBits) - site(PgIdxBits)
      case VPNBits => site(VAddrBits) - site(PgIdxBits)
      case MIFTagBits => Dump("MEM_TAG_BITS", 5)
      case MIFDataBits => Dump("MEM_DATA_BITS", 128)
      case MIFAddrBits => Dump("MEM_ADDR_BITS", site(PAddrBits) - site(CacheBlockOffsetBits))
      case MIFDataBeats => site(TLDataBits)/site(MIFDataBits)
      //Params used by all caches
      case NSets => findBy(CacheName)
      case NWays => findBy(CacheName)
      case RowBits => findBy(CacheName)
      case BlockOffBits => findBy(CacheName)
      case "L1I" => {
        case NSets => Knob("L1I_SETS") //128
        case NWays => Knob("L1I_WAYS") //2
        case RowBits => 4*site(CoreInstBits)
        case BlockOffBits => log2Up(site(TLDataBits)/8)
      }:PF
      case "L1D" => {
        case NSets => Knob("L1D_SETS") //128
        case NWays => Knob("L1D_WAYS") //4
        case RowBits => 2*site(CoreDataBits)
        case BlockOffBits => log2Up(site(TLDataBits)/8)
      }:PF
      case "L2" => {
        case NSets => 512 
        case NWays => 8
        case RowBits => site(TLDataBits)
        case BlockOffBits => 0
      }:PF
      case ECCCode => None
      case WordBits => site(XprLen)
      case Replacer => () => new RandomReplacement(site(NWays))
      //L1InstCache
      case NITLBEntries => 8
      case NBTBEntries => 62
      case NRAS => 2
      //L1DataCache
      case NDTLBEntries => 8
      case StoreDataQueueDepth => 17
      case ReplayQueueDepth => 16
      case NMSHRs => Knob("L1D_MSHRS")
      case LRSCCycles => 32 
      //L2CacheParams
      case NReleaseTransactors => Knob("L2_REL_XACTS")
      case NAcquireTransactors => Knob("L2_ACQ_XACTS")
      case NClients => site(NTiles) + 1
      //Tile Constants
      case BuildRoCC => None
      case NDCachePorts => 2 + (if(site(BuildRoCC).isEmpty) 0 else 1) 
      case NTilePorts => 2 + (if(site(BuildRoCC).isEmpty) 0 else 1)
      case NPTWPorts => 2 + (if(site(BuildRoCC).isEmpty) 0 else 3)
      //Rocket Core Constants
      case RetireWidth => 1
      case UseVM => true
      case FastLoadWord => true
      case FastLoadByte => false
      case FastMulDiv => true
      case XprLen => 64
      case NMultXpr => 32
      case BuildFPU => Some(() => Module(new FPU))
      case SFMALatency => 2
      case DFMALatency => 3
      case CoreInstBits => 32
      case CoreDataBits => site(XprLen)
      case CoreDCacheReqTagBits => 7 + log2Up(here(NDCachePorts))
      //Uncore Paramters
      case LNMasters => site(NBanks)
      case LNClients => site(NTiles)+1
      case LNEndpoints => site(LNMasters) + site(LNClients)
      case TLId => "inner"
      case TLCoherence => site(Coherence)
      case TLAddrBits => site(PAddrBits) - site(CacheBlockOffsetBits)
      case TLMasterXactIdBits => site(TLId) match {
        case "inner" => log2Up(site(NReleaseTransactors)+site(NAcquireTransactors))
        case "outer" => 1
      }
      case TLClientXactIdBits => site(TLId) match {
        case "inner" => log2Up(site(NMSHRs))+log2Up(site(NTilePorts))
        case "outer" => log2Up(site(NReleaseTransactors)+site(NAcquireTransactors))
      }
      case TLDataBits => site(CacheBlockBytes)*8
      case TLWriteMaskBits => 6
      case TLWordAddrBits  => 3
      case TLAtomicOpBits  => 4
      case NTiles => Knob("NTILES")
      case NBanks => Knob("NBANKS")
      case NOutstandingMemReqs => 2*site(NBanks)*(site(NReleaseTransactors)+site(NAcquireTransactors))
      case BankIdLSB => 5
      case CacheBlockBytes => 64
      case CacheBlockOffsetBits => log2Up(here(CacheBlockBytes))
      case UseBackupMemoryPort => true
      case BuildCoherenceMaster => (id: Int) => {
          Module(new L2CoherenceAgent(id, "inner", "outer"), { case CacheName => "L2" })
      }
      case Coherence => new MSICoherence(() => new NullRepresentation)
  }},
  knobValues = {
    case "NTILES" => 1
    case "NBANKS" => 1
    case "L2_REL_XACTS" => 1
    case "L2_ACQ_XACTS" => 7
    case "L1D_MSHRS" => 2
    case "L1D_SETS" => 128
    case "L1D_WAYS" => 4
    case "L1I_SETS" => 128
    case "L1I_WAYS" => 2
  }
)
class DefaultVLSIConfig extends DefaultConfig
class DefaultCPPConfig extends DefaultConfig

class FPGAConfig extends ChiselConfig (
  (pname,site,here) => pname match {
    case UseBackupMemoryPort => false
  }
)

class DefaultFPGAConfig extends ChiselConfig(new FPGAConfig ++ new DefaultConfig)

class SmallConfig extends ChiselConfig (
    topDefinitions = { (pname,site,here) => pname match {
      case BuildFPU => None
      case FastMulDiv => false
      case NITLBEntries => 4
      case NBTBEntries => 8
      case NDTLBEntries => 4
    }},
  knobValues = {
    case "L1D_SETS" => 64
    case "L1D_WAYS" => 1
    case "L1I_SETS" => 64
    case "L1I_WAYS" => 1
  }
)

class DefaultFPGASmallConfig extends ChiselConfig(new SmallConfig ++ new DefaultFPGAConfig)

class ExampleSmallConfig extends ChiselConfig(new SmallConfig ++ new DefaultConfig)
