package referencechip

import Chisel._
import uncore._
import rocket._
import rocket.Util._

class DefaultVLSIConfig extends DefaultConfig
class DefaultFPGAConfig extends DefaultConfig
class DefaultCPPConfig extends DefaultConfig
class DefaultConfig extends ChiselConfig {
  val top:World.TopDefs = {
    (pname,site,here) => pname match {
      //DesignSpaceConstants 
      case "NTILES" => 1
      case "NBANKS" => 1
      case "HTIF_WIDTH" => 16
      case "ENABLE_SHARING" => true
      case "ENABLE_CLEAN_EXCLUSIVE" => true
      case "USE_DRAMSIDE_LLC" => true
      case "NL2_REL_XACTS" => 1
      case "NL2_ACQ_XACTS" => 7
      case "NMSHRS" => 2
      //Coherence
      case Coherence => {
          val dir = new FullRepresentation(site[Int]("NTILES")+1)
          if(site[Boolean]("ENABLE_SHARING")) {
            if(site[Boolean]("ENABLE_CLEAN_EXCLUSIVE")) new MESICoherence(dir)
            else new MSICoherence(dir)
          } else {
            if(site[Boolean]("ENABLE_CLEAN_EXCLUSIVE")) new MEICoherence(dir)
            else new MICoherence(dir)
          }
        }
      //Rocket Constants
      // Number of ports into D$: 1 from core, 1 from PTW, maybe 1 from RoCC
      case NDCachePorts => 2 + (if(site(BuildRoCC).isEmpty) 0 else 1) 
      // Number of ports to outer memory system from tile: 1 from I$, 1 from D$, maybe 1 from Rocc
      case NTilePorts => 2 + (if(site(BuildRoCC).isEmpty) 0 else 1)
      case BuildRoCC => None
      case RetireWidth => 1
      case UseVM => true
      case FastLoadWord => true
      case FastLoadByte => false
      case FastMulDiv => true
      case DcacheReqTagBits => 7 + log2Up(here(NDCachePorts))
      case XprLen => 64
      case NXpr => 32
      case NXprBits => log2Up(here(NXpr))
      case BuildFPU => Some(() => new FPU)
      case FPUParams => Alter({ 
          case SFMALatency => 2
          case DFMALatency => 3
        })
      case RocketDCacheParams => Alter({
          //L1 Specific
          case StoreDataQueueDepth => 17
          case ReplayQueueDepth => 16
          case NMSHRs => site[Int]("NMSHRS")
          case NTLBEntries => 8
          case CoreReqTagBits => site(DcacheReqTagBits)
          case CoreDataBits => site(XprLen)
          case RowWords => 2
          case ECCCode => new IdentityCode
          //From uncore/cache.scala
          case NSets => 128
          case NWays => 4
          case IsDM => here(NWays) == 1
          case OffBits => log2Up(site(TLDataBits))
          case IdxBits => log2Up(here(NSets))
          case UntagBits => here(OffBits) + here(IdxBits)
          case TagBits => here(PAddrBits) - here(UntagBits)
          case WayBits => log2Up(here(NWays))
          case Replacer => () => new RandomReplacement(site(NWays))
          case RowBits => here(RowWords)*here(CoreDataBits)
          case WordBits => here(CoreDataBits)
          case RefillCycles => site(TLDataBits)/here(RowBits)
          //Derived
          case MaxAddrBits => math.max(site(PPNBits),site(VPNBits)+1) + site(PgIdxBits)
          case CoreDataBytes => here(CoreDataBits)/8
          case WordOffBits => log2Up(here(CoreDataBytes))
          case RowBytes => here(RowWords)*here(CoreDataBytes)
          case RowOffBits => log2Up(here(RowBytes))
          case DoNarrowRead => here(CoreDataBits)*here(NWays) % here(RowBits) == 0
          case EncDataBits => here(ECCCode).width(here(CoreDataBits))
          case EncRowBits => here(RowWords)*here(EncDataBits)
          case LRSCCycles => 32 
        })
      case RocketFrontendParams => Alter({
          case InstBytes => 4
          case RowBytes => 16
          case NTLBEntries => 8
          case ECCCode => new IdentityCode
          //From uncore/cache.scala
          case NSets => 128
          case NWays => 2
          case IsDM => here(NWays) == 1
          case OffBits => log2Up(site(TLDataBits)/8)
          case IdxBits => log2Up(here(NSets))
          case UntagBits => here(OffBits) + here(IdxBits)
          case TagBits => here(PAddrBits) - here(UntagBits)
          case WayBits => log2Up(here(NWays))
          case Replacer => () => new RandomReplacement(site(NWays))
          case RowBits => here(RowBytes)*8
          case RefillCycles => site(TLDataBits)/here(RowBits)
          case RowOffBits => log2Up(here(RowBytes))
        })
      case CoreBTBParams => Alter({
          case Entries => 62
          case NRAS => 2
          case MatchBits => site(PgIdxBits)
          case Pages => ((1 max(log2Up(site(Entries))))+1)/2*2 //TODO PARAMS no here?
          // control logic assumes 2 divides pages
          case OpaqueBits => log2Up(here(Entries))
          case NBHT => 1 << log2Up(site(Entries)*2) //TODO PARAMS no here?
        })
      //MemoryConstants
      case "CACHE_DATA_SIZE_IN_BYTES" => 1 << 6
      case "OFFSET_BITS" => log2Up(here[Int]("CACHE_DATA_SIZE_IN_BYTES"))
      case PAddrBits => 32
      case VAddrBits => 43
      case PgIdxBits => 13
      case ASIdBits => 7
      case PermBits => 6
      case PPNBits => here(PAddrBits) - here(PgIdxBits)
      case VPNBits => here(VAddrBits) - here(PgIdxBits)
      case MIFTagBits => 5
      case MIFDataBits => 128
      case MIFAddrBits => here(PAddrBits) - here[Int]("OFFSET_BITS")
      case MIFDataBeats => 4
      //Uncore Constants
      case TileLinkL1Params => Alter({
          case LNMasters => site[Int]("NBANKS")
          case LNClients => site[Int]("NTILES")+1
          case LNEndpoints => site[Int]("NBANKS") + site[Int]("NTILES")+1 // TODO PARAMS why broken?: site(LNMasters) +site(LNClients)
          case TLCoherence => site(Coherence)
          case TLAddrBits => site(PAddrBits) - site[Int]("OFFSET_BITS")
          case TLMasterXactIdBits => log2Up(site[Int]("NL2_REL_XACTS")+site[Int]("NL2_ACQ_XACTS"))
          case TLClientXactIdBits => 2*log2Up(site[Int]("NMSHRS")*site[Int]("NTILES")+1)
          case TLDataBits => site[Int]("CACHE_DATA_SIZE_IN_BYTES")*8
          case TLWriteMaskBits => 6
          case TLWordAddrBits  => 3
          case TLAtomicOpBits  => 4
        })
      case L2HellaCacheParams => Alter({
          case NReleaseTransactors =>  site[Int]("NL2_REL_XACTS")
          case NAcquireTransactors =>  site[Int]("NL2_ACQ_XACTS")
          case NTransactors => here(NReleaseTransactors) + here(NAcquireTransactors)
          case NClients => site[Int]("NTILES") + 1
          case NSets => 512
          case NWays => 8
          case IsDM => here(NWays) == 1
          case OffBits => 0
          case IdxBits => log2Up(here(NSets))
          case UntagBits => here(OffBits) + here(IdxBits)
          case TagBits => here(PAddrBits) - here(UntagBits)
          case WayBits => log2Up(here(NWays))
          case Replacer => () => new RandomReplacement(site(NWays))
          case RowBits => site(TLDataBits)
          case WordBits => 64
          case RefillCycles => site(TLDataBits)/here(RowBits)
        })
      case NTiles => here[Int]("NTILES")
      case NBanks => here[Int]("NBANKS")
      case BankIdLSB => 5
      case BuildDRAMSideLLC => () => {
        val refill = site(TLDataBits)/site(MIFDataBits)
        if(site[Boolean]("USE_DRAMSIDE_LLC")) {
          val tag = Mem(Bits(width = 152), 512, seqRead = true)
          val data = Mem(Bits(width = 64), 4096, seqRead = true)
          Module(new DRAMSideLLC(sets=512, ways=8, outstanding=16, 
            refill_cycles=refill, tagLeaf=tag, dataLeaf=data))
        } else { Module(new DRAMSideLLCNull(16, refill)) }
      }
      case BuildCoherentMaster => (id: Int) => {
        if(!site[Boolean]("USE_DRAMSIDE_LLC")) { 
          Module(new L2CoherenceAgent(id), here(L2HellaCacheParams))
        } else {
          Module(new L2HellaCache(id), here(L2HellaCacheParams))
        }
      }
      //HTIF Constants
      case HTIFWidth => 16
      case HTIFNSCR => 64
      case HTIFOffsetBits => here[Int]("OFFSET_BITS")
      case HTIFNCores => here[Int]("NTILES")
    }
  }
}

case object NTiles extends Field[Int]
case object NBanks extends Field[Int]
case object BankIdLSB extends Field[Int]
case object TileLinkL1Params extends Field[PF]
case object L2HellaCacheParams extends Field[PF]
case object BuildDRAMSideLLC extends Field[() => DRAMSideLLCLike]
case object BuildCoherentMaster extends Field[Int => CoherenceAgent]
case object Coherence extends Field[CoherencePolicyWithUncached]

class OuterMemorySystem extends Module
{
  val io = new Bundle {
    val tiles = Vec.fill(params(NTiles)){new TileLinkIO}.flip
    val htif = (new TileLinkIO).flip
    val incoherent = Vec.fill(params(LNClients)){Bool()}.asInput
    val mem = new MemIO
    val mem_backup = new MemSerializedIO(params(HTIFWidth))
    val mem_backup_en = Bool(INPUT)
  }

  val refill_cycles = params(TLDataBits)/params(MIFDataBits)
  val llc = params(BuildDRAMSideLLC)()
  val masterEndpoints = (0 until params(NBanks)).map(params(BuildCoherentMaster))

  val net = Module(new ReferenceChipCrossbarNetwork)
  net.io.clients zip (io.tiles :+ io.htif) map { case (net, end) => net <> end }
  net.io.masters zip (masterEndpoints.map(_.io.inner)) map { case (net, end) => net <> end }
  masterEndpoints.map{ _.io.incoherent zip io.incoherent map { case (m, c) => m := c } }

  val conv = Module(new MemIOUncachedTileLinkIOConverter(2))
  if(params(NBanks) > 1) {
    val arb = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(params(NBanks)))
    arb.io.in zip masterEndpoints.map(_.io.outer) map { case (arb, cache) => arb <> cache }
    conv.io.uncached <> arb.io.out
  } else {
    conv.io.uncached <> masterEndpoints.head.io.outer
  }
  llc.io.cpu.req_cmd <> Queue(conv.io.mem.req_cmd)
  llc.io.cpu.req_data <> Queue(conv.io.mem.req_data, refill_cycles)
  conv.io.mem.resp <> llc.io.cpu.resp

  // mux between main and backup memory ports
  val mem_serdes = Module(new MemSerdes(params(HTIFWidth)))
  val mem_cmdq = Module(new Queue(new MemReqCmd, 2))
  mem_cmdq.io.enq <> llc.io.mem.req_cmd
  mem_cmdq.io.deq.ready := Mux(io.mem_backup_en, mem_serdes.io.wide.req_cmd.ready, io.mem.req_cmd.ready)
  io.mem.req_cmd.valid := mem_cmdq.io.deq.valid && !io.mem_backup_en
  io.mem.req_cmd.bits := mem_cmdq.io.deq.bits
  mem_serdes.io.wide.req_cmd.valid := mem_cmdq.io.deq.valid && io.mem_backup_en
  mem_serdes.io.wide.req_cmd.bits := mem_cmdq.io.deq.bits

  val mem_dataq = Module(new Queue(new MemData, refill_cycles))
  mem_dataq.io.enq <> llc.io.mem.req_data
  mem_dataq.io.deq.ready := Mux(io.mem_backup_en, mem_serdes.io.wide.req_data.ready, io.mem.req_data.ready)
  io.mem.req_data.valid := mem_dataq.io.deq.valid && !io.mem_backup_en
  io.mem.req_data.bits := mem_dataq.io.deq.bits
  mem_serdes.io.wide.req_data.valid := mem_dataq.io.deq.valid && io.mem_backup_en
  mem_serdes.io.wide.req_data.bits := mem_dataq.io.deq.bits

  llc.io.mem.resp.valid := Mux(io.mem_backup_en, mem_serdes.io.wide.resp.valid, io.mem.resp.valid)
  io.mem.resp.ready := Bool(true)
  llc.io.mem.resp.bits := Mux(io.mem_backup_en, mem_serdes.io.wide.resp.bits, io.mem.resp.bits)

  io.mem_backup <> mem_serdes.io.narrow
}


class Uncore extends Module
{
  require(params(BankIdLSB) + log2Up(params(NBanks)) < params(MIFAddrBits))
  val htif_width = params(HTIFWidth)
  val io = new Bundle {
    val host = new HostIO(htif_width)
    val mem = new MemIO
    val tiles = Vec.fill(params(NTiles)){new TileLinkIO}.flip
    val htif = Vec.fill(params(NTiles)){new HTIFIO}.flip
    val incoherent = Vec.fill(params(NTiles)){Bool()}.asInput
    val mem_backup = new MemSerializedIO(htif_width)
    val mem_backup_en = Bool(INPUT)
  }
  val htif = Module(new HTIF(CSRs.reset))
  val outmemsys = Module(new OuterMemorySystem)
  val incoherentWithHtif = (io.incoherent :+ Bool(true).asInput)
  outmemsys.io.incoherent := incoherentWithHtif
  htif.io.cpu <> io.htif
  outmemsys.io.mem <> io.mem
  outmemsys.io.mem_backup_en <> io.mem_backup_en

  // Add networking headers and endpoint queues
  def convertAddrToBank(addr: Bits): UInt = {
    addr(params(BankIdLSB) + log2Up(params(NBanks)) - 1, params(BankIdLSB))
  }

  (outmemsys.io.tiles :+ outmemsys.io.htif).zip(io.tiles :+ htif.io.mem).zipWithIndex.map { 
    case ((outer, client), i) => 
      outer.acquire <> Queue(TileLinkHeaderOverwriter(client.acquire, i, params(NBanks), convertAddrToBank _))
      outer.release <> Queue(TileLinkHeaderOverwriter(client.release, i, params(NBanks), convertAddrToBank _))
      outer.finish <> Queue(TileLinkHeaderOverwriter(client.finish, i, true))
      client.grant <> Queue(outer.grant, 1, pipe = true)
      client.probe <> Queue(outer.probe)
  }

  // pad out the HTIF using a divided clock
  val hio = Module((new SlowIO(512)) { Bits(width = params(HTIFWidth)+1) })
  hio.io.set_divisor.valid := htif.io.scr.wen && htif.io.scr.waddr === 63
  hio.io.set_divisor.bits := htif.io.scr.wdata
  htif.io.scr.rdata(63) := hio.io.divisor

  hio.io.out_fast.valid := htif.io.host.out.valid || outmemsys.io.mem_backup.req.valid
  hio.io.out_fast.bits := Cat(htif.io.host.out.valid, Mux(htif.io.host.out.valid, htif.io.host.out.bits, outmemsys.io.mem_backup.req.bits))
  htif.io.host.out.ready := hio.io.out_fast.ready
  outmemsys.io.mem_backup.req.ready := hio.io.out_fast.ready && !htif.io.host.out.valid
  io.host.out.valid := hio.io.out_slow.valid && hio.io.out_slow.bits(htif_width)
  io.host.out.bits := hio.io.out_slow.bits
  io.mem_backup.req.valid := hio.io.out_slow.valid && !hio.io.out_slow.bits(htif_width)
  hio.io.out_slow.ready := Mux(hio.io.out_slow.bits(htif_width), io.host.out.ready, io.mem_backup.req.ready)

  val mem_backup_resp_valid = io.mem_backup_en && io.mem_backup.resp.valid
  hio.io.in_slow.valid := mem_backup_resp_valid || io.host.in.valid
  hio.io.in_slow.bits := Cat(mem_backup_resp_valid, io.host.in.bits)
  io.host.in.ready := hio.io.in_slow.ready
  outmemsys.io.mem_backup.resp.valid := hio.io.in_fast.valid && hio.io.in_fast.bits(htif_width)
  outmemsys.io.mem_backup.resp.bits := hio.io.in_fast.bits
  htif.io.host.in.valid := hio.io.in_fast.valid && !hio.io.in_fast.bits(htif_width)
  htif.io.host.in.bits := hio.io.in_fast.bits
  hio.io.in_fast.ready := Mux(hio.io.in_fast.bits(htif_width), Bool(true), htif.io.host.in.ready)
  io.host.clk := hio.io.clk_slow
  io.host.clk_edge := Reg(next=io.host.clk && !Reg(next=io.host.clk))

  io.host.debug_stats_pcr := htif.io.host.debug_stats_pcr
}

class TopIO extends Bundle  {
  val host    = new HostIO(params(HTIFWidth))
  val mem     = new MemIO
}

class VLSITopIO extends TopIO {
  val mem_backup_en = Bool(INPUT)
  val in_mem_ready = Bool(OUTPUT)
  val in_mem_valid = Bool(INPUT)
  val out_mem_ready = Bool(INPUT)
  val out_mem_valid = Bool(OUTPUT)
}

class MemDessert extends Module {
  val io = new MemDesserIO(params(HTIFWidth))
  val x = Module(new MemDesser(params(HTIFWidth)))
  io.narrow <> x.io.narrow
  io.wide <> x.io.wide
}

class Top extends Module {

  //val vic = ICacheConfig(sets = 128, assoc = 1, tl = tl, as = as, btb = BTBConfig(as, 8))
  //val hc = hwacha.HwachaConfiguration(as, vic, dc, 8, 256, ndtlb = 8, nptlb = 2)

  val nTiles = params(NTiles)
  val io = new VLSITopIO

  val tl: PartialFunction[Any,Any] = params(TileLinkL1Params) //TODO PARAMS can't lookup in map() below?
  params.alter(tl)
  val resetSigs = Vec.fill(nTiles){Bool()}
  val tileList = (0 until nTiles).map(r => Module(new Tile(resetSignal = resetSigs(r)), tl))//TODO PARAMS above alter() is insufficient?
  val uncore = Module(new Uncore)

  for (i <- 0 until nTiles) {
    val hl = uncore.io.htif(i)
    val tl = uncore.io.tiles(i)
    val il = uncore.io.incoherent(i)

    resetSigs(i) := hl.reset
    val tile = tileList(i)

    tile.io.tilelink <> tl
    il := hl.reset
    tile.io.host.id := UInt(i)
    tile.io.host.reset := Reg(next=Reg(next=hl.reset))
    tile.io.host.pcr_req <> Queue(hl.pcr_req, 1)
    hl.pcr_rep <> Queue(tile.io.host.pcr_rep, 1)
    hl.ipi_req <> Queue(tile.io.host.ipi_req, 1)
    tile.io.host.ipi_rep <> Queue(hl.ipi_rep, 1)
    hl.debug_stats_pcr := tile.io.host.debug_stats_pcr
  }

  io.host <> uncore.io.host

  uncore.io.mem_backup.resp.valid := io.in_mem_valid

  io.out_mem_valid := uncore.io.mem_backup.req.valid
  uncore.io.mem_backup.req.ready := io.out_mem_ready

  io.mem_backup_en <> uncore.io.mem_backup_en
  io.mem <> uncore.io.mem
}
