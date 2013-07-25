package referencechip

import Chisel._
import uncore._
import rocket._
import rocket.Util._
import ReferenceChipBackend._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object DummyTopLevelConstants extends uncore.constants.CoherenceConfigConstants {
  val NTILES = 2
  val NBANKS = 1
  val HTIF_WIDTH = 16
  val ENABLE_SHARING = true
  val ENABLE_CLEAN_EXCLUSIVE = true
  val HAS_VEC = true
}

object ReferenceChipBackend {
  val initMap = new HashMap[Component, Bool]()
}

class ReferenceChipBackend extends VerilogBackend
{
  override def emitPortDef(m: MemAccess, idx: Int) = {
    val res = new StringBuilder()
    for (node <- m.mem.inputs) {
      if(node.name.contains("init"))
         res.append("    .init(" + node.name + "),\n")
    }
    (if (idx == 0) res.toString else "") + super.emitPortDef(m, idx)
  }

  def addMemPin(c: Component) = {
    for (node <- Component.nodes) {
      if (node.isInstanceOf[Mem[ _ ]] && node.component != null && node.asInstanceOf[Mem[_]].seqRead) {
        connectMemPin(c, node.component, node)
      }
    }
  }

  def connectMemPin(topC: Component, c: Component, p: Node): Unit = {
    var isNewPin = false
    val compInitPin = 
      if (initMap.contains(c)) {
        initMap(c)
      } else {
        isNewPin = true
        Bool(INPUT)
      }

    p.inputs += compInitPin

    if (isNewPin) {
      compInitPin.setName("init")
      c.io.asInstanceOf[Bundle] += compInitPin
      compInitPin.component = c
      initMap += (c -> compInitPin)
      connectMemPin(topC, c.parent, compInitPin)
    }
  }

  def addTopLevelPin(c: Component) = {
    val init = Bool(INPUT)
    init.setName("init")
    init.component = c
    c.io.asInstanceOf[Bundle] += init
    initMap += (c -> init)
  }

  transforms += ((c: Component) => addTopLevelPin(c))
  transforms += ((c: Component) => addMemPin(c))
}

class OuterMemorySystem(htif_width: Int, clientEndpoints: Seq[ClientCoherenceAgent])(implicit conf: UncoreConfiguration) extends Component
{
  implicit val lnconf = conf.ln
  val io = new Bundle {
    val tiles = Vec(conf.ln.nClients) { new TileLinkIO }.flip
    val htif = (new TileLinkIO).flip
    val incoherent = Vec(conf.ln.nClients) { Bool() }.asInput
    val mem_backup = new ioMemSerialized(htif_width)
    val mem_backup_en = Bool(INPUT)
    val mem = new ioMem
  }

  val lnWithHtifConf = conf.ln.copy(nEndpoints = conf.ln.nEndpoints+1, 
                                    idBits = log2Up(conf.ln.nEndpoints+1)+1,
                                    nClients = conf.ln.nClients+1)
  val ucWithHtifConf = conf.copy(ln = lnWithHtifConf)
  require(clientEndpoints.length == lnWithHtifConf.nClients)
  val masterEndpoints = (0 until lnWithHtifConf.nMasters).map(new L2CoherenceAgent(_)(ucWithHtifConf))
  val incoherentWithHtif = io.incoherent :+ Bool(true)

  val llc_tag_leaf = Mem(512, seqRead = true) { Bits(width = 152) }
  val llc_data_leaf = Mem(4096, seqRead = true) { Bits(width = 64) }
  val llc = new DRAMSideLLC(512, 8, 4, llc_tag_leaf, llc_data_leaf)
  //val llc = new DRAMSideLLCNull(NGLOBAL_XACTS, REFILL_CYCLES)
  val mem_serdes = new MemSerdes(htif_width)

  val net = new ReferenceChipCrossbarNetwork(masterEndpoints++clientEndpoints)(ucWithHtifConf)
  net.io zip (masterEndpoints.map(_.io.client) ++ io.tiles :+ io.htif) map { case (net, end) => net <> end }
  masterEndpoints.map{ _.io.incoherent zip incoherentWithHtif map { case (m, c) => m := c } }

  val conv = new MemIOUncachedTileLinkIOConverter(2)(ucWithHtifConf)
  if(lnWithHtifConf.nMasters > 1) {
    val arb = new UncachedTileLinkIOArbiterThatAppendsArbiterId(lnWithHtifConf.nMasters, conf.co)(lnWithHtifConf)
    arb.io.in zip masterEndpoints.map(_.io.master) map { case (arb, cache) => arb <> cache }
    conv.io.uncached <> arb.io.out
  } else {
    conv.io.uncached <> masterEndpoints.head.io.master
  }
  llc.io.cpu.req_cmd <> Queue(conv.io.mem.req_cmd)
  llc.io.cpu.req_data <> Queue(conv.io.mem.req_data, REFILL_CYCLES)
  conv.io.mem.resp <> llc.io.cpu.resp

  // mux between main and backup memory ports
  val mem_cmdq = (new Queue(2)) { new MemReqCmd }
  mem_cmdq.io.enq <> llc.io.mem.req_cmd
  mem_cmdq.io.deq.ready := Mux(io.mem_backup_en, mem_serdes.io.wide.req_cmd.ready, io.mem.req_cmd.ready)
  io.mem.req_cmd.valid := mem_cmdq.io.deq.valid && !io.mem_backup_en
  io.mem.req_cmd.bits := mem_cmdq.io.deq.bits
  mem_serdes.io.wide.req_cmd.valid := mem_cmdq.io.deq.valid && io.mem_backup_en
  mem_serdes.io.wide.req_cmd.bits := mem_cmdq.io.deq.bits

  val mem_dataq = (new Queue(REFILL_CYCLES)) { new MemData }
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

class Uncore(htif_width: Int, tileList: Seq[ClientCoherenceAgent])(implicit conf: UncoreConfiguration) extends Component
{
  implicit val lnconf = conf.ln
  val io = new Bundle {
    val debug = new DebugIO()
    val host = new HostIO(htif_width)
    val mem_backup = new ioMemSerialized(htif_width)
    val mem_backup_en = Bool(INPUT)
    val mem = new ioMem
    val tiles = Vec(conf.ln.nClients) { new TileLinkIO }.flip
    val htif = Vec(conf.ln.nClients) { new HTIFIO(conf.ln.nClients) }.flip
    val incoherent = Vec(conf.ln.nClients) { Bool() }.asInput
  }
  val nBanks = 1
  val bankIdLsb = 5

  val htif = new rocketHTIF(htif_width)
  val outmemsys = new OuterMemorySystem(htif_width, tileList :+ htif)
  htif.io.cpu <> io.htif
  outmemsys.io.incoherent <> io.incoherent
  io.mem <> outmemsys.io.mem
  outmemsys.io.mem_backup_en <> io.mem_backup_en

  // Add networking headers and endpoint queues
  def convertAddrToBank(addr: Bits): UFix = {
    require(bankIdLsb + log2Up(nBanks) < PADDR_BITS - OFFSET_BITS, {println("Invalid bits for bank multiplexing.")})
    addr(bankIdLsb + log2Up(nBanks) - 1, bankIdLsb)
  }

  (outmemsys.io.tiles :+ outmemsys.io.htif).zip(io.tiles :+ htif.io.mem).zipWithIndex.map { 
    case ((outer, client), i) => 
      outer.acquire <> TileLinkHeaderAppender(client.acquire, i, nBanks, convertAddrToBank _)
      outer.release <> TileLinkHeaderAppender(client.release, i, nBanks, convertAddrToBank _)

      val grant_ack_q = Queue(client.grant_ack)
      outer.grant_ack.valid := grant_ack_q.valid
      outer.grant_ack.bits := grant_ack_q.bits
      outer.grant_ack.bits.header.src := UFix(i)
      grant_ack_q.ready := outer.grant_ack.ready

      client.grant <> Queue(outer.grant, 1, pipe = true)
      client.probe <> Queue(outer.probe)
  }

  // pad out the HTIF using a divided clock
  val hio = (new SlowIO(512)) { Bits(width = htif_width+1) }
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
  io.host.clk_edge := Reg(io.host.clk && !Reg(io.host.clk))
}

class TopIO(htif_width: Int) extends Bundle  {
  val debug   = new rocket.DebugIO
  val host    = new rocket.HostIO(htif_width);
  val mem_backup_en = Bool(INPUT)
  val in_mem_ready = Bool(OUTPUT)
  val in_mem_valid = Bool(INPUT)
  val out_mem_ready = Bool(INPUT)
  val out_mem_valid = Bool(OUTPUT)
  val mem     = new ioMem
}

import DummyTopLevelConstants._

class MemDessert extends Component {
  val io = new MemDesserIO(HTIF_WIDTH)
  val x = new MemDesser(HTIF_WIDTH)
  io.narrow <> x.io.narrow
  io.wide <> x.io.wide
}

class Top extends Component {
  val co =  if(ENABLE_SHARING) {
              if(ENABLE_CLEAN_EXCLUSIVE) new MESICoherence
              else new MSICoherence
            } else {
              if(ENABLE_CLEAN_EXCLUSIVE) new MEICoherence
              else new MICoherence
            }

  implicit val lnConf = LogicalNetworkConfiguration(NTILES+NBANKS, log2Up(NTILES)+1, NBANKS, NTILES)
  implicit val uConf = UncoreConfiguration(co, lnConf)

  val io = new TopIO(HTIF_WIDTH)

  val resetSigs = Vec(NTILES){ Bool() }
  val ic = ICacheConfig(128, 2, co, ntlb = 8, nbtb = 16)
  val dc = DCacheConfig(128, 4, co, ntlb = 8,
                        nmshr = 2, nrpq = 16, nsdq = 17)
  val rc = RocketConfiguration(lnConf, co, ic, dc,
                               fpu = true, vec = HAS_VEC)
  val tileList = (0 until NTILES).map(r => new Tile(resetSignal = resetSigs(r))(rc))
  val uncore = new Uncore(HTIF_WIDTH, tileList)

  var error_mode = Bool(false)
  for (i <- 0 until NTILES) {
    val hl = uncore.io.htif(i)
    val tl = uncore.io.tiles(i)
    val il = uncore.io.incoherent(i)

    resetSigs(i) := hl.reset
    val tile = tileList(i)
    tile.io.tilelink <> tl
    il := hl.reset
    tile.io.host.reset := Reg(Reg(hl.reset))
    tile.io.host.pcr_req <> Queue(hl.pcr_req)
    hl.pcr_rep <> Queue(tile.io.host.pcr_rep)
    hl.ipi_req <> Queue(tile.io.host.ipi_req)
    tile.io.host.ipi_rep <> Queue(hl.ipi_rep)
    error_mode = error_mode || Reg(tile.io.host.debug.error_mode)
  }

  io.host <> uncore.io.host

  uncore.io.mem_backup.resp.valid := io.in_mem_valid

  io.out_mem_valid := uncore.io.mem_backup.req.valid
  uncore.io.mem_backup.req.ready := io.out_mem_ready

  io.mem_backup_en <> uncore.io.mem_backup_en
  io.mem <> uncore.io.mem
  io.debug.error_mode := error_mode
}
