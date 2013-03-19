package referencechip

import Chisel._
import Node._
import uncore._
import rocket._
import rocket.Util._
import ReferenceChipBackend._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap


object TileToCrossbarShim {
  def apply[T <: Data](logIO: ClientSourcedIO[LogicalNetworkIO[T]])(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) = {
    val shim = (new TileToCrossbarShim) { logIO.bits.payload.clone }
    shim.io.in <> logIO
    shim.io.out
  }
}
class TileToCrossbarShim[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new ClientSourcedIO){(new LogicalNetworkIO){ data }}.flip
    val out = (new FIFOIO){(new BasicCrossbarIO){ data }}
  }
  io.out.bits.header.src := io.in.bits.header.src + UFix(lconf.nHubs)
  io.out.bits.header.dst := io.in.bits.header.dst 
  io.out.bits.payload := io.in.bits.payload
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

object HubToCrossbarShim {
  def apply[T <: Data](logIO: MasterSourcedIO[LogicalNetworkIO[T]])(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) = {
    val shim = (new HubToCrossbarShim) { logIO.bits.payload.clone }
    shim.io.in <> logIO
    shim.io.out
  }
}
class HubToCrossbarShim[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new MasterSourcedIO){(new LogicalNetworkIO){ data }}
    val out = (new FIFOIO){(new BasicCrossbarIO){ data }}
  }
  io.out.bits.header.src := io.in.bits.header.src
  io.out.bits.header.dst := io.in.bits.header.dst + UFix(lconf.nHubs)
  io.out.bits.payload := io.in.bits.payload
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

object CrossbarToTileShim {
  def apply[T <: Data](physIO: FIFOIO[BasicCrossbarIO[T]])(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) = {
    val shim = (new CrossbarToTileShim) { physIO.bits.payload.clone }
    shim.io.in <> physIO
    shim.io.out
  }
}
class CrossbarToTileShim[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new FIFOIO){(new BasicCrossbarIO){ data }}.flip
    val out = (new ClientSourcedIO){(new LogicalNetworkIO){ data }}
  }
  io.out.bits.header.src := io.in.bits.header.src
  io.out.bits.header.dst := io.in.bits.header.dst - UFix(lconf.nHubs)
  io.out.bits.payload := io.in.bits.payload
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

object CrossbarToHubShim {
  def apply[T <: Data](physIO: FIFOIO[BasicCrossbarIO[T]])(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) = {
    val shim = (new CrossbarToHubShim) { physIO.bits.payload.clone }
    shim.io.in <> physIO
    shim.io.out
  }
}
class CrossbarToHubShim[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new FIFOIO){(new BasicCrossbarIO){ data }}.flip
    val out = (new MasterSourcedIO){(new LogicalNetworkIO){ data }}.flip
  }
  io.out.bits.header.src := io.in.bits.header.src - UFix(lconf.nHubs)
  io.out.bits.header.dst := io.in.bits.header.dst
  io.out.bits.payload := io.in.bits.payload
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

class ReferenceChipCrossbarNetwork(endpoints: Seq[CoherenceAgentRole])(implicit conf: LogicalNetworkConfiguration) extends LogicalNetwork[TileLinkIO](endpoints)(conf) {
  type TileLinkType = TileLinkIO
  val io = Vec(endpoints.map(_ match { case t:ClientCoherenceAgent => {(new TileLinkType).flip}; case h:MasterCoherenceAgent => {new TileLinkType}})){ new TileLinkType }

  //If we allow all physical networks to be identical, we can use
  //reflection to automatically create enough networks for any given 
  //bundle containing LogicalNetworkIOs
  val tl = new TileLinkType
  val payloadBitsForEachPhysicalNetwork = tl.getClass.getMethods.filter( x => 
      classOf[DirectionalFIFOIO[Data]].isAssignableFrom(x.getReturnType)).map(
      _.invoke(tl).asInstanceOf[DirectionalFIFOIO[LogicalNetworkIO[Data]]].bits.payload)
  implicit val pconf = new PhysicalNetworkConfiguration(conf.nEndpoints, conf.idBits)//same config for all networks
  val physicalNetworks: Seq[BasicCrossbar[Data]] = payloadBitsForEachPhysicalNetwork.map(d => (new BasicCrossbar){d.clone}) 

  //Use reflection to get the subset of each node's TileLink
  //corresponding to each direction of dataflow and connect each sub-bundle
  //to the appropriate port of the physical crossbar network, inserting
  //shims to convert headers and process flits in the process.
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => {
    val logNetIOSubBundles = io.getClass.getMethods.filter( x => 
      classOf[DirectionalFIFOIO[Data]].isAssignableFrom(x.getReturnType)).zipWithIndex
    val tileProducedSubBundles = logNetIOSubBundles.filter( x =>
      classOf[ClientSourcedIO[Data]].isAssignableFrom(x._1.getReturnType)).map{ case (m,i) =>
        (m.invoke(io).asInstanceOf[ClientSourcedIO[LogicalNetworkIO[Data]]],i) }
    val hubProducedSubBundles  = logNetIOSubBundles.filter( x =>
      classOf[MasterSourcedIO[Data]].isAssignableFrom(x._1.getReturnType)).map{ case (m,i) =>
        (m.invoke(io).asInstanceOf[MasterSourcedIO[LogicalNetworkIO[Data]]],i) }
    end match {
      case x:ClientCoherenceAgent => {
        tileProducedSubBundles.foreach{ case (sl,i) => { 
          physicalNetworks(i).io.in(id) <> TileToCrossbarShim(sl) 
          physicalNetworks(i).io.out(id).ready := Bool(false)
        }}
        hubProducedSubBundles.foreach{ case (sl,i) => {
          sl <> CrossbarToTileShim(physicalNetworks(i).io.out(id)) 
          physicalNetworks(i).io.in(id).valid := Bool(false)
        }}
      }
      case y:MasterCoherenceAgent => {
        hubProducedSubBundles.foreach{ case (sl,i) => {
          physicalNetworks(i).io.in(id) <> HubToCrossbarShim(sl) 
          physicalNetworks(i).io.out(id).ready := Bool(false)
        }}
        tileProducedSubBundles.foreach{ case (sl,i) => {
          sl <> CrossbarToHubShim(physicalNetworks(i).io.out(id)) 
          physicalNetworks(i).io.in(id).valid := Bool(false)
        }}
      }
    }
  }}
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
        val init = Bool(INPUT)
        init.setName("init")
        node.inputs += init
        connectMemPin(c, node.component, init)
      }
    }
  }

  def connectMemPin(topC: Component, c: Component, p: Bool): Unit = {
    p.component = c
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

class OuterMemorySystem(htif_width: Int, tileEndpoints: Seq[ClientCoherenceAgent])(implicit conf: CoherenceHubConfiguration) extends Component
{
  implicit val lnconf = conf.ln
  val io = new Bundle {
    val tiles = Vec(conf.ln.nTiles) { new TileLinkIO }.flip
    val htif = (new TileLinkIO).flip
    val incoherent = Vec(conf.ln.nTiles) { Bool() }.asInput
    val mem_backup = new ioMemSerialized(htif_width)
    val mem_backup_en = Bool(INPUT)
    val mem = new ioMemPipe
  }

  import rocket.Constants._

  val lnWithHtifConf = conf.ln.copy(nEndpoints = conf.ln.nEndpoints+1, 
                                    idBits = log2Up(conf.ln.nEndpoints+1)+1,
                                    nTiles = conf.ln.nTiles+1)
  val chWithHtifConf = conf.copy(ln = lnWithHtifConf)
  require(tileEndpoints.length == lnWithHtifConf.nTiles)
  //val hub = new CoherenceHubBroadcast()(chWithHtifConf)
  val llc_tag_leaf = Mem(1024, seqRead = true) { Bits(width = 72) }
  val llc_data_leaf = Mem(4096, seqRead = true) { Bits(width = 64) }
  val llc = new DRAMSideLLC(512, 8, 4, llc_tag_leaf, llc_data_leaf)
  //val llc = new DRAMSideLLCNull(NGLOBAL_XACTS, REFILL_CYCLES)
  val mem_serdes = new MemSerdes(htif_width)

  val hub = new CoherenceHubBroadcast()(chWithHtifConf)
  //val adapter = new CoherenceHubAdapter()(lnWithHtifConf)
  //val hub = new L2CoherenceAgent()(chWithHtifConf)
  //val net = new ReferenceChipCrossbarNetwork(List(hub)++tileEndpoints)(lnWithHtifConf)
  //net.io(0) <> adapter.io.net
  //hub.io.tiles <> adapter.io.hub
  //hub.io.network <> net.io(0)

  for (i <- 1 to conf.ln.nTiles) {
    //net.io(i) <> io.tiles(i-1)
    hub.io.tiles(i-1) <> io.tiles(i-1)
    hub.io.incoherent(i-1) := io.incoherent(i-1)
  }
  //net.io(conf.ln.nTiles+1) <> io.htif
  hub.io.tiles(conf.ln.nTiles) <> io.htif
  hub.io.incoherent(conf.ln.nTiles) := Bool(true)

  llc.io.cpu.req_cmd <> Queue(hub.io.mem.req_cmd)
  llc.io.cpu.req_data <> Queue(hub.io.mem.req_data, REFILL_CYCLES)
  hub.io.mem.resp <> llc.io.cpu.resp

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
  llc.io.mem.resp.bits := Mux(io.mem_backup_en, mem_serdes.io.wide.resp.bits, io.mem.resp.bits)

  io.mem_backup <> mem_serdes.io.narrow
}

class Uncore(htif_width: Int, tileEndpoints: Seq[ClientCoherenceAgent])(implicit conf: CoherenceHubConfiguration) extends Component
{
  implicit val lnconf = conf.ln
  val io = new Bundle {
    val debug = new DebugIO()
    val host = new HostIO(htif_width)
    val mem_backup = new ioMemSerialized(htif_width)
    val mem_backup_en = Bool(INPUT)
    val mem = new ioMemPipe
    val tiles = Vec(conf.ln.nTiles) { new TileLinkIO }.flip
    val htif = Vec(conf.ln.nTiles) { new HTIFIO(conf.ln.nTiles) }.flip
    val incoherent = Vec(conf.ln.nTiles) { Bool() }.asInput
  }

  val htif = new rocketHTIF(htif_width)
  htif.io.cpu <> io.htif

  val outmemsys = new OuterMemorySystem(htif_width, tileEndpoints++List(htif))
  outmemsys.io.tiles <> io.tiles
  outmemsys.io.htif <> htif.io.mem
  outmemsys.io.incoherent <> io.incoherent
  io.mem <> outmemsys.io.mem
  outmemsys.io.mem_backup_en <> io.mem_backup_en

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

object DummyTopLevelConstants extends _root_.uncore.constants.CoherenceConfigConstants {
  val NTILES = 1
  val HTIF_WIDTH = 16
  val ENABLE_SHARING = true
  val ENABLE_CLEAN_EXCLUSIVE = true
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

  implicit val lnConf = LogicalNetworkConfiguration(NTILES+1, log2Up(NTILES)+1, 1, NTILES)
  implicit val chConf = CoherenceHubConfiguration(co, lnConf)

  val io = new TopIO(HTIF_WIDTH)

  val resetSigs = Vec(NTILES){ Bool() }
  val ic = ICacheConfig(128, 2, co, ntlb = 8, nbtb = 16)
  val dc = DCacheConfig(128, 4, co, ntlb = 8,
                        nmshr = 2, nrpq = 16, nsdq = 17)
  val rc = RocketConfiguration(lnConf, co, ic, dc,
                               fpu = true, vec = true)
  val tileList = (0 until NTILES).map(r => new Tile(resetSignal = resetSigs(r))(rc))
  val uncore = new Uncore(HTIF_WIDTH, tileList)

  var error_mode = Bool(false)
  for (i <- 0 until NTILES) {
    val hl = uncore.io.htif(i)
    val tl = uncore.io.tiles(i)
    val il = uncore.io.incoherent(i)

    resetSigs(i) := hl.reset
    val tile = tileList(i)

    tile.io.host.reset := Reg(Reg(hl.reset))
    tile.io.host.pcr_req <> Queue(hl.pcr_req)
    hl.pcr_rep <> Queue(tile.io.host.pcr_rep)
    hl.ipi_req <> Queue(tile.io.host.ipi_req)
    tile.io.host.ipi_rep <> Queue(hl.ipi_rep)
    error_mode = error_mode || Reg(tile.io.host.debug.error_mode)

    val x_init_q = Queue(tile.io.tilelink.acquire)
    tl.acquire.valid := x_init_q.valid
    tl.acquire.bits.payload := x_init_q.bits.payload
    tl.acquire.bits.header.src := UFix(i)
    tl.acquire.bits.header.dst := UFix(0)
    x_init_q.ready := tl.acquire.ready
    val x_init_data_q = Queue(tile.io.tilelink.acquire_data)
    tl.acquire_data.valid := x_init_data_q.valid
    tl.acquire_data.bits.payload := x_init_data_q.bits.payload
    tl.acquire_data.bits.header.src := UFix(i)
    tl.acquire_data.bits.header.dst := UFix(0)
    x_init_data_q.ready := tl.acquire_data.ready
    val x_finish_q = Queue(tile.io.tilelink.grant_ack)
    tl.grant_ack.valid := x_finish_q.valid
    tl.grant_ack.bits.payload := x_finish_q.bits.payload
    tl.grant_ack.bits.header.src := UFix(i)
    tl.grant_ack.bits.header.dst := UFix(0)
    x_finish_q.ready := tl.grant_ack.ready
    val p_rep_q = Queue(tile.io.tilelink.release, 1)
    tl.release.valid := p_rep_q.valid
    tl.release.bits.payload := p_rep_q.bits.payload
    tl.release.bits.header.src := UFix(i)
    tl.release.bits.header.dst := UFix(0)
    p_rep_q.ready := tl.release.ready
    val p_rep_data_q = Queue(tile.io.tilelink.release_data)
    tl.release_data.valid := p_rep_data_q.valid
    tl.release_data.bits.payload := p_rep_data_q.bits.payload
    tl.release_data.bits.header.src := UFix(i)
    tl.release_data.bits.header.dst := UFix(0)
    p_rep_data_q.ready := tl.release_data.ready

    tile.io.tilelink.abort <> Queue(tl.abort)
    tile.io.tilelink.grant <> Queue(tl.grant, 1, pipe = true)
    tile.io.tilelink.probe <> Queue(tl.probe)
    il := hl.reset

  }

  io.host <> uncore.io.host

  uncore.io.mem_backup.resp.valid := io.in_mem_valid

  io.out_mem_valid := uncore.io.mem_backup.req.valid
  uncore.io.mem_backup.req.ready := io.out_mem_ready

  io.mem_backup_en <> uncore.io.mem_backup_en
  io.mem <> uncore.io.mem
  io.debug.error_mode := error_mode
}
