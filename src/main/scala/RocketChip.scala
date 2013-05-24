package referencechip

import Chisel._
import Node._
import uncore.Constants._
import uncore._
import rocket._
import rocket.Util._
import ReferenceChipBackend._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object TileLinkHeaderAppender {
  def apply[T <: SourcedMessage with HasPhysicalAddress, U <: SourcedMessage with HasMemData](in: ClientSourcedDataIO[LogicalNetworkIO[T],LogicalNetworkIO[U]], clientId: Int, nBanks: Int, addrConvert: Bits => UFix)(implicit conf: UncoreConfiguration) = {
    val shim = new TileLinkHeaderAppender(clientId, nBanks, addrConvert)(in.meta.bits.payload.clone, in.data.bits.payload.clone)
    shim.io.in <> in
    shim.io.out
  }
  def apply[T <: SourcedMessage with HasPhysicalAddress](in: ClientSourcedFIFOIO[LogicalNetworkIO[T]], clientId: Int, nBanks: Int, addrConvert: Bits => UFix)(implicit conf: UncoreConfiguration) = {
    val shim = new TileLinkHeaderAppender(clientId, nBanks, addrConvert)(in.bits.payload.clone, new AcquireData)
    shim.io.in.meta <> in
    shim.io.out.meta
  }
}

class TileLinkHeaderAppender[T <: SourcedMessage with HasPhysicalAddress, U <: SourcedMessage with HasMemData](clientId: Int, nBanks: Int, addrConvert: Bits => UFix)(metadata: => T, data: => U)(implicit conf: UncoreConfiguration) extends Component {
  implicit val ln = conf.ln
  val io = new Bundle {
    val in = new ClientSourcedDataIO()((new LogicalNetworkIO){ metadata }, (new LogicalNetworkIO){ data }).flip
    val out = new ClientSourcedDataIO()((new LogicalNetworkIO){ metadata }, (new LogicalNetworkIO){ data })
  }

  val meta_q = Queue(io.in.meta)
  val data_q = Queue(io.in.data)
  if(nBanks == 1) {
    io.out.meta.bits.payload := meta_q.bits.payload
    io.out.meta.bits.header.src := UFix(clientId)
    io.out.meta.bits.header.dst := UFix(0)
    io.out.meta.valid := meta_q.valid
    meta_q.ready := io.out.meta.ready
    io.out.data.bits.payload := data_q.bits.payload
    io.out.data.bits.header.src := UFix(clientId)
    io.out.data.bits.header.dst := UFix(0)
    io.out.data.valid := data_q.valid
    data_q.ready := io.out.data.ready
  } else {
    val meta_has_data = conf.co.messageHasData(meta_q.bits.payload)
    val addr_q = (new Queue(2, pipe = true, flow = true)){io.in.meta.bits.payload.addr.clone}
    val data_cnt = Reg(resetVal = UFix(0, width = log2Up(REFILL_CYCLES)))
    val data_cnt_up = data_cnt + UFix(1)

    io.out.meta.bits.payload := meta_q.bits.payload
    io.out.meta.bits.header.src := UFix(clientId)
    io.out.meta.bits.header.dst := addrConvert(meta_q.bits.payload.addr)
    io.out.data.bits.payload := meta_q.bits.payload
    io.out.data.bits.header.src := UFix(clientId)
    io.out.data.bits.header.dst := addrConvert(addr_q.io.deq.bits)
    addr_q.io.enq.bits := meta_q.bits.payload.addr

    io.out.meta.valid := meta_q.valid && addr_q.io.enq.ready
    meta_q.ready := io.out.meta.ready && addr_q.io.enq.ready
    io.out.data.valid := data_q.valid && addr_q.io.deq.valid
    data_q.ready := io.out.data.ready && addr_q.io.deq.valid
    addr_q.io.enq.valid := meta_q.valid && io.out.meta.ready && meta_has_data
    addr_q.io.deq.ready := Bool(false)

    when(data_q.valid && data_q.ready) {
      data_cnt := data_cnt_up
      when(data_cnt_up === UFix(0)) {
        addr_q.io.deq.ready := Bool(true)
      }
    }
  }
}

class MemIOUncachedTileLinkIOConverter(qDepth: Int)(implicit conf: UncoreConfiguration) extends Component {
  implicit val ln = conf.ln
  val io = new Bundle {
    val uncached = new UncachedTileLinkIO().flip
    val mem = new ioMem
  }
  val mem_cmd_q = (new Queue(qDepth)){new MemReqCmd}
  val mem_data_q = (new Queue(qDepth)){new MemData}
  mem_cmd_q.io.enq.valid := io.uncached.acquire.meta.valid
  io.uncached.acquire.meta.ready := mem_cmd_q.io.enq.ready 
  mem_cmd_q.io.enq.bits.rw := conf.co.needsOuterWrite(io.uncached.acquire.meta.bits.payload.a_type, UFix(0))
  mem_cmd_q.io.enq.bits.tag := io.uncached.acquire.meta.bits.payload.client_xact_id
  mem_cmd_q.io.enq.bits.addr := io.uncached.acquire.meta.bits.payload.addr
  mem_data_q.io.enq.valid := io.uncached.acquire.data.valid
  io.uncached.acquire.data.ready := mem_data_q.io.enq.ready
  mem_data_q.io.enq.bits.data := io.uncached.acquire.data.bits.payload.data 
  io.uncached.grant.valid := io.mem.resp.valid
  io.mem.resp.ready := io.uncached.grant.ready
  io.uncached.grant.bits.payload.data := io.mem.resp.bits.data
  io.uncached.grant.bits.payload.client_xact_id := io.mem.resp.bits.tag
  io.uncached.grant.bits.payload.master_xact_id := UFix(0) // DNC
  io.uncached.grant.bits.payload.g_type := UFix(0) // DNC
  io.mem.req_cmd <> mem_cmd_q.io.deq
  io.mem.req_data <> mem_data_q.io.deq
}

class ReferenceChipCrossbarNetwork(endpoints: Seq[CoherenceAgentRole])(implicit conf: UncoreConfiguration) extends LogicalNetwork[TileLinkIO](endpoints)(conf.ln) {
  implicit val lnConf = conf.ln
  type TileLinkType = TileLinkIO
  val io = Vec(endpoints.map(_ match { case t:ClientCoherenceAgent => {(new TileLinkType).flip}; case h:MasterCoherenceAgent => {new TileLinkType}})){ new TileLinkType }
  implicit val pconf = new PhysicalNetworkConfiguration(conf.ln.nEndpoints, conf.ln.idBits) // Same config for all networks

  type FBCIO[T <: Data] = FIFOIO[BasicCrossbarIO[T]]
  type FLNIO[T <: Data] = FIFOIO[LogicalNetworkIO[T]]
  type PBCIO[M <: Data, D <: Data] = PairedDataIO[BasicCrossbarIO[M], BasicCrossbarIO[D]]
  type PLNIO[M <: Data, D <: Data] = PairedDataIO[LogicalNetworkIO[M], LogicalNetworkIO[D]]
  type FromCrossbar[T <: Data] = FBCIO[T] => FLNIO[T]
  type ToCrossbar[T <: Data] = FLNIO[T] => FBCIO[T]

  def DefaultFromCrossbarShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = new FIFOIO()(new LogicalNetworkIO()(in.bits.payload.clone)).asDirectionless
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
  def CrossbarToMasterShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = DefaultFromCrossbarShim(in)
    out.bits.header.src := in.bits.header.src - UFix(conf.ln.nMasters)
    out
  }
  def CrossbarToClientShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = DefaultFromCrossbarShim(in)
    out.bits.header.dst := in.bits.header.dst - UFix(conf.ln.nMasters)
    out
  }
  def DefaultToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = new FIFOIO()(new BasicCrossbarIO()(in.bits.payload.clone)).asDirectionless
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
  def MasterToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = DefaultToCrossbarShim(in)
    out.bits.header.dst := in.bits.header.dst + UFix(conf.ln.nMasters)
    out
  }
  def ClientToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = DefaultToCrossbarShim(in)
    out.bits.header.src := in.bits.header.src + UFix(conf.ln.nMasters)
    out
  }

  def doFIFOInputHookup[T <: Data](phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T], shim: ToCrossbar[T]) = {
    val s = shim(log_io)
    phys_in.valid := s.valid
    phys_in.bits := s.bits
    s.ready := phys_in.ready
    phys_out.ready := Bool(false)
  }
  def doFIFOOutputHookup[T <: Data](phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T], shim: FromCrossbar[T]) = {
    val s = shim(phys_out)
    log_io.valid := s.valid
    log_io.bits := s.bits
    s.ready := log_io.ready
    phys_in.valid := Bool(false)
  }
  //TODO: Change all the manifest stuff to use TypeTags in Scala 2.11
  def doFIFOHookup[S <: CoherenceAgentRole: ClassManifest, T <: Data](end: CoherenceAgentRole, phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T], inShim: ToCrossbar[T], outShim: FromCrossbar[T]) = {
    if(scala.reflect.ClassManifest.fromClass(end.getClass) <:< classManifest[S]) // end.getClass is a subtype of S
      doFIFOInputHookup(phys_in, phys_out, log_io, inShim)
    else doFIFOOutputHookup(phys_in, phys_out, log_io, outShim)
  }
  def doClientSourcedFIFOHookup[T <: Data](end: CoherenceAgentRole, phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T]) =
    doFIFOHookup[ClientCoherenceAgent, T](end, phys_in, phys_out, log_io, ClientToCrossbarShim, CrossbarToMasterShim)
  def doMasterSourcedFIFOHookup[T <: Data](end: CoherenceAgentRole, phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T]) =
    doFIFOHookup[MasterCoherenceAgent, T](end, phys_in, phys_out, log_io, MasterToCrossbarShim, CrossbarToClientShim)
    
  def doPairedDataHookup[S <: CoherenceAgentRole : ClassManifest, T <: Data, R <: Data](end: CoherenceAgentRole, phys_in: PBCIO[T,R], phys_out: PBCIO[T,R], log_io: PLNIO[T,R], inShim: ToCrossbar[T], outShim: FromCrossbar[T], inShimD: ToCrossbar[R], outShimD: FromCrossbar[R]) = {
    if(scala.reflect.ClassManifest.fromClass(end.getClass) <:< classManifest[S]) {
      doFIFOInputHookup[T](phys_in.meta, phys_out.meta, log_io.meta, inShim)
      doFIFOInputHookup[R](phys_in.data, phys_out.data, log_io.data, inShimD)
    } else {
      doFIFOOutputHookup[T](phys_in.meta, phys_out.meta, log_io.meta, outShim)
      doFIFOOutputHookup[R](phys_in.data, phys_out.data, log_io.data, outShimD)
    }
  }
  def doClientSourcedPairedHookup[T <: Data, R <: Data](end: CoherenceAgentRole, phys_in: PBCIO[T,R], phys_out: PBCIO[T,R], log_io: PLNIO[T,R]) =
    doPairedDataHookup[ClientCoherenceAgent, T, R](end, phys_in, phys_out, log_io, ClientToCrossbarShim, CrossbarToMasterShim, ClientToCrossbarShim, CrossbarToMasterShim)
  def doMasterSourcedPairedHookup[T <: Data, R <: Data](end: CoherenceAgentRole, phys_in: PBCIO[T,R], phys_out: PBCIO[T,R], log_io: PLNIO[T,R]) =
    doPairedDataHookup[MasterCoherenceAgent, T, R](end, phys_in, phys_out, log_io, MasterToCrossbarShim, CrossbarToClientShim, MasterToCrossbarShim, CrossbarToClientShim)


  def acqHasData(acq: BasicCrossbarIO[Acquire]) = conf.co.messageHasData(acq.payload)
  val acq_net = new PairedCrossbar(REFILL_CYCLES, acqHasData _)(new Acquire, new AcquireData)
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doClientSourcedPairedHookup(end, acq_net.io.in(id), acq_net.io.out(id), io.acquire) }

  def relHasData(rel: BasicCrossbarIO[Release]) = conf.co.messageHasData(rel.payload)
  val rel_net = new PairedCrossbar(REFILL_CYCLES, relHasData _)(new Release, new ReleaseData)
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doClientSourcedPairedHookup(end, rel_net.io.in(id), rel_net.io.out(id), io.release) }

  val probe_net = new BasicCrossbar()(new Probe)
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doMasterSourcedFIFOHookup(end, probe_net.io.in(id), probe_net.io.out(id), io.probe) }

  val grant_net = new BasicCrossbar()(new Grant)
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doMasterSourcedFIFOHookup(end, grant_net.io.in(id), grant_net.io.out(id), io.grant) }

  val ack_net = new BasicCrossbar()(new GrantAck)
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doClientSourcedFIFOHookup(end, ack_net.io.in(id), ack_net.io.out(id), io.grant_ack) }

  val physicalNetworks = List(acq_net, rel_net, probe_net, grant_net, ack_net)
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

  import rocket.Constants._

  val lnWithHtifConf = conf.ln.copy(nEndpoints = conf.ln.nEndpoints+1, 
                                    idBits = log2Up(conf.ln.nEndpoints+1)+1,
                                    nClients = conf.ln.nClients+1)
  val ucWithHtifConf = conf.copy(ln = lnWithHtifConf)
  require(clientEndpoints.length == lnWithHtifConf.nClients)
  val masterEndpoints = (0 until lnWithHtifConf.nMasters).map(new L2CoherenceAgent(_)(ucWithHtifConf))

  val llc_tag_leaf = Mem(512, seqRead = true) { Bits(width = 152) }
  val llc_data_leaf = Mem(4096, seqRead = true) { Bits(width = 64) }
  val llc = new DRAMSideLLC(512, 8, 4, llc_tag_leaf, llc_data_leaf)
  //val llc = new DRAMSideLLCNull(NGLOBAL_XACTS, REFILL_CYCLES)
  val mem_serdes = new MemSerdes(htif_width)

  val net = new ReferenceChipCrossbarNetwork(masterEndpoints++clientEndpoints)(ucWithHtifConf)
  net.io zip (masterEndpoints.map(_.io.client) ++ io.tiles :+ io.htif) map { case (net, end) => net <> end }
  masterEndpoints.map{ _.io.incoherent zip (io.incoherent ++ List(Bool(true))) map { case (m, c) => m := c } }

  val conv = new MemIOUncachedTileLinkIOConverter(2)(ucWithHtifConf)
  if(lnWithHtifConf.nMasters > 1) {
    val arb = new UncachedTileLinkIOArbiter(lnWithHtifConf.nMasters, conf.co)(lnWithHtifConf)
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

object DummyTopLevelConstants extends _root_.uncore.constants.CoherenceConfigConstants {
  val NTILES = 1
  val NBANKS = 1
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

  implicit val lnConf = LogicalNetworkConfiguration(NTILES+NBANKS, log2Up(NTILES)+1, NBANKS, NTILES)
  implicit val uConf = UncoreConfiguration(co, lnConf)

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
