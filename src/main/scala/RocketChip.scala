package referencechip

import Chisel._
import Node._
import uncore._
import rocket._
import ReferenceChipBackend._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap


object TileToCrossbarShim {
  def apply[T <: Data](logIO: TileIO[T])(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) = {
    val shim = (new TileToCrossbarShim) { logIO.bits.clone }
    shim.io.in <> logIO
    shim.io.out
  }
}
class TileToCrossbarShim[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new TileIO){ data }.flip
    val out = (new BasicCrossbarIO){ data }
  }
  io.out.header.src := io.in.header.src + UFix(lconf.nHubs)
  io.out.header.dst := io.in.header.dst 
  io.out.bits := io.in.bits
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

object HubToCrossbarShim {
  def apply[T <: Data](logIO: HubIO[T])(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) = {
    val shim = (new HubToCrossbarShim) { logIO.bits.clone }
    shim.io.in <> logIO
    shim.io.out
  }
}
class HubToCrossbarShim[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new HubIO){ data }
    val out = (new BasicCrossbarIO){ data }
  }
  io.out.header.src := io.in.header.src
  io.out.header.dst := io.in.header.dst + UFix(lconf.nHubs)
  io.out.bits := io.in.bits
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

object CrossbarToTileShim {
  def apply[T <: Data](physIO: BasicCrossbarIO[T])(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) = {
    val shim = (new CrossbarToTileShim) { physIO.bits.clone }
    shim.io.in <> physIO
    shim.io.out
  }
}
class CrossbarToTileShim[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new BasicCrossbarIO){ data }.flip
    val out = (new TileIO){ data }
  }
  io.out.header.src := io.in.header.src
  io.out.header.dst := io.in.header.dst - UFix(lconf.nHubs)
  io.out.bits := io.in.bits
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

object CrossbarToHubShim {
  def apply[T <: Data](physIO: BasicCrossbarIO[T])(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) = {
    val shim = (new CrossbarToHubShim) { physIO.bits.clone }
    shim.io.in <> physIO
    shim.io.out
  }
}
class CrossbarToHubShim[T <: Data]()(data: => T)(implicit lconf: LogicalNetworkConfiguration, pconf: PhysicalNetworkConfiguration) extends Component {
  val io = new Bundle {
    val in  = (new BasicCrossbarIO){ data }.flip
    val out = (new HubIO){ data }.flip
  }
  io.out.header.src := io.in.header.src - UFix(lconf.nHubs)
  io.out.header.dst := io.in.header.dst
  io.out.bits := io.in.bits
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

class ReferenceChipCrossbarNetwork(endpoints: Seq[CoherenceAgent])(implicit conf: LogicalNetworkConfiguration) extends LogicalNetwork[TileLink](endpoints)(conf) {
  type TileLinkType = TileLink
  val io = Vec(endpoints.map(_ match { case t:ClientCoherenceAgent => {(new TileLinkType).flip}; case h:MasterCoherenceAgent => {new TileLinkType}})){ new TileLinkType }

  //If we allow all physical networks to be identical, we can use
  //reflection to automatically create enough networks for any given 
  //bundle containing LogicalNetworkIOs
  val tl = new TileLinkType
  val payloadBitsForEachPhysicalNetwork = tl.getClass.getMethods.filter( x => 
      classOf[LogicalNetworkIO[Data]].isAssignableFrom(x.getReturnType)).map(
      _.invoke(tl).asInstanceOf[LogicalNetworkIO[Data]].bits)
  implicit val pconf = new PhysicalNetworkConfiguration(conf.nEndpoints, conf.idBits)//same config for all networks
  val physicalNetworks: Seq[BasicCrossbar[Data]] = payloadBitsForEachPhysicalNetwork.map(d => (new BasicCrossbar){d.clone}) 

  //Use reflection to get the subset of each node's TileLink
  //corresponding to each direction of dataflow and connect each sub-bundle
  //to the appropriate port of the physical crossbar network, inserting
  //shims to convert headers and process flits in the process.
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => {
    val logNetIOSubBundles = io.getClass.getMethods.filter( x => 
      classOf[LogicalNetworkIO[Data]].isAssignableFrom(x.getReturnType)).zipWithIndex
    val tileProducedSubBundles = logNetIOSubBundles.filter( x =>
      classOf[TileIO[Data]].isAssignableFrom(x._1.getReturnType)).map{ case (m,i) =>
        (m.invoke(io).asInstanceOf[TileIO[Data]],i) } 
    val hubProducedSubBundles  = logNetIOSubBundles.filter( x =>
      classOf[HubIO[Data]].isAssignableFrom(x._1.getReturnType)).map{ case (m,i) =>
        (m.invoke(io).asInstanceOf[HubIO[Data]],i) }
    end match {
      case x:ClientCoherenceAgent => {
        tileProducedSubBundles.foreach{ case (sl,i) => 
          physicalNetworks(i).io.in(id) <> TileToCrossbarShim(sl) }
        hubProducedSubBundles.foreach{ case (sl,i) => 
          sl <> CrossbarToHubShim(physicalNetworks(i).io.out(id)) }
      }
      case y:MasterCoherenceAgent => {
        hubProducedSubBundles.foreach{ case (sl,i) => 
          physicalNetworks(i).io.in(id) <> HubToCrossbarShim(sl) }
        tileProducedSubBundles.foreach{ case (sl,i) => 
          sl <> CrossbarToTileShim(physicalNetworks(i).io.out(id)) }
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
    val tiles = Vec(conf.ln.nTiles) { new TileLink }.flip
    val htif = new TileLink().flip
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
  val hub = new CoherenceHubBroadcast()(chWithHtifConf)
  val llc_tag_leaf = Mem(1024, seqRead = true) { Bits(width = 72) }
  val llc_data_leaf = Mem(4096, seqRead = true) { Bits(width = 64) }
  val llc = new DRAMSideLLC(512, 8, 4, llc_tag_leaf, llc_data_leaf)
  val mem_serdes = new MemSerdes(htif_width)

  val testNet = new ReferenceChipCrossbarNetwork(List(hub)++tileEndpoints)(lnWithHtifConf)

  for (i <- 0 until conf.ln.nTiles) {
    hub.io.tiles(i) <> io.tiles(i)
  }
  hub.io.tiles(conf.ln.nTiles) <> io.htif

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
    val debug = new ioDebug()
    val host = new ioHost(htif_width)
    val mem_backup = new ioMemSerialized(htif_width)
    val mem_backup_en = Bool(INPUT)
    val mem = new ioMemPipe
    val tiles = Vec(conf.ln.nTiles) { new TileLink }.flip
    val htif = Vec(conf.ln.nTiles) { new ioHTIF(conf.ln.nTiles) }.flip
  }

  val htif = new rocketHTIF(htif_width)
  htif.io.cpu <> io.htif

  val outmemsys = new OuterMemorySystem(htif_width, tileEndpoints++List(htif))
  outmemsys.io.tiles <> io.tiles
  outmemsys.io.htif <> htif.io.mem
  io.mem <> outmemsys.io.mem
  outmemsys.io.mem_backup_en <> io.mem_backup_en

  // pad out the HTIF using a divided clock
  val hio = (new slowIO(8)) { Bits(width = htif_width+1) }
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

class ioTop(htif_width: Int) extends Bundle  {
  val debug   = new rocket.ioDebug();
  val host    = new rocket.ioHost(htif_width);
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

  val io = new ioTop(HTIF_WIDTH)

  val resetSigs = Vec(NTILES){ Bool() }
  val ic = ICacheConfig(128, 2, co, ntlb = 8, nbtb = 16)
  val dc = DCacheConfig(128, 4, co, ntlb = 8,
                        nmshr = 2, nrpq = 16, nsdq = 17)
  val rc = RocketConfiguration(NTILES, co, ic, dc,
                               fpu = true, vec = true)
  val tileList = (0 until NTILES).map(r => new Tile(resetSignal = resetSigs(r))(rc))
  val uncore = new Uncore(HTIF_WIDTH, tileList)

  var error_mode = Bool(false)
  for (i <- 0 until NTILES) {
    val hl = uncore.io.htif(i)
    val tl = uncore.io.tiles(i)

    resetSigs(i) := hl.reset
    val tile = tileList(i)

    tile.io.host.reset := Reg(Reg(hl.reset))
    tile.io.host.pcr_req <> Queue(hl.pcr_req)
    hl.pcr_rep <> Queue(tile.io.host.pcr_rep)
    hl.ipi_req <> Queue(tile.io.host.ipi_req)
    tile.io.host.ipi_rep <> Queue(hl.ipi_rep)
    error_mode = error_mode || Reg(tile.io.host.debug.error_mode)

    tl.xact_init <> Queue(tile.io.tilelink.xact_init)
    tl.xact_init_data <> Queue(tile.io.tilelink.xact_init_data)
    tile.io.tilelink.xact_abort <> Queue(tl.xact_abort)
    tile.io.tilelink.xact_rep <> Queue(tl.xact_rep, 1, pipe = true)
    tl.xact_finish <> Queue(tile.io.tilelink.xact_finish)
    tile.io.tilelink.probe_req <> Queue(tl.probe_req)
    tl.probe_rep <> Queue(tile.io.tilelink.probe_rep, 1)
    tl.probe_rep_data <> Queue(tile.io.tilelink.probe_rep_data)
    tl.incoherent := hl.reset
  }

  io.host <> uncore.io.host

  uncore.io.mem_backup.resp.valid := io.in_mem_valid

  io.out_mem_valid := uncore.io.mem_backup.req.valid
  uncore.io.mem_backup.req.ready := io.out_mem_ready

  io.mem_backup_en <> uncore.io.mem_backup_en
  io.mem <> uncore.io.mem
  io.debug.error_mode := error_mode
}
