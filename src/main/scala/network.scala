package referencechip

import Chisel._
import uncore._
import scala.reflect._

object TileLinkHeaderAppender {
  def apply[T <: SourcedMessage with HasPhysicalAddress, U <: SourcedMessage with HasTileLinkData](in: ClientSourcedDataIO[LogicalNetworkIO[T],LogicalNetworkIO[U]], clientId: Int, nBanks: Int, addrConvert: Bits => UInt)(implicit conf: TileLinkConfiguration) = {
    val shim = Module(new TileLinkHeaderAppender(clientId, nBanks, addrConvert)(in.meta.bits.payload.clone, in.data.bits.payload.clone))
    shim.io.in <> in
    shim.io.out
  }
  def apply[T <: SourcedMessage with HasPhysicalAddress](in: ClientSourcedFIFOIO[LogicalNetworkIO[T]], clientId: Int, nBanks: Int, addrConvert: Bits => UInt)(implicit conf: TileLinkConfiguration) = {
    val shim = Module(new TileLinkHeaderAppender(clientId, nBanks, addrConvert)(in.bits.payload.clone, new AcquireData))
    shim.io.in.meta <> in
    shim.io.out.meta
  }
}

class TileLinkHeaderAppender[T <: SourcedMessage with HasPhysicalAddress, U <: SourcedMessage with HasTileLinkData](clientId: Int, nBanks: Int, addrConvert: Bits => UInt)(metadata: => T, data: => U)(implicit conf: TileLinkConfiguration) extends Module {
  implicit val ln = conf.ln
  val io = new Bundle {
    val in = new ClientSourcedDataIO()((new LogicalNetworkIO){ metadata }, (new LogicalNetworkIO){ data }).flip
    val out = new ClientSourcedDataIO()((new LogicalNetworkIO){ metadata }, (new LogicalNetworkIO){ data })
  }

  val meta_q = Queue(io.in.meta)
  val data_q = Queue(io.in.data)
  if(nBanks == 1) {
    io.out.meta.bits.payload := meta_q.bits.payload
    io.out.meta.bits.header.src := UInt(clientId)
    io.out.meta.bits.header.dst := UInt(0)
    io.out.meta.valid := meta_q.valid
    meta_q.ready := io.out.meta.ready
    io.out.data.bits.payload := data_q.bits.payload
    io.out.data.bits.header.src := UInt(clientId)
    io.out.data.bits.header.dst := UInt(0)
    io.out.data.valid := data_q.valid
    data_q.ready := io.out.data.ready
  } else {
    val meta_has_data = conf.co.messageHasData(meta_q.bits.payload)
    val addr_q = Module(new Queue(io.in.meta.bits.payload.addr.clone, 2, pipe = true, flow = true))
    val data_cnt = Reg(init=UInt(0, width = log2Up(REFILL_CYCLES)))
    val data_cnt_up = data_cnt + UInt(1)

    io.out.meta.bits.payload := meta_q.bits.payload
    io.out.meta.bits.header.src := UInt(clientId)
    io.out.meta.bits.header.dst := addrConvert(meta_q.bits.payload.addr)
    io.out.data.bits.payload := meta_q.bits.payload
    io.out.data.bits.header.src := UInt(clientId)
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
      when(data_cnt_up === UInt(0)) {
        addr_q.io.deq.ready := Bool(true)
      }
    }
  }
}

//Adapter betweewn an UncachedTileLinkIO and a mem controller MemIO
class MemIOUncachedTileLinkIOConverter(qDepth: Int)(implicit conf: TileLinkConfiguration) extends Module {
  val io = new Bundle {
    val uncached = new UncachedTileLinkIO().flip
    val mem = new ioMem
  }
  val mem_cmd_q = Module(new Queue(new MemReqCmd, qDepth))
  val mem_data_q = Module(new Queue(new MemData, qDepth))
  mem_cmd_q.io.enq.valid := io.uncached.acquire.meta.valid
  io.uncached.acquire.meta.ready := mem_cmd_q.io.enq.ready 
  mem_cmd_q.io.enq.bits.rw := conf.co.needsOuterWrite(io.uncached.acquire.meta.bits.payload.a_type, UInt(0))
  mem_cmd_q.io.enq.bits.tag := io.uncached.acquire.meta.bits.payload.client_xact_id
  mem_cmd_q.io.enq.bits.addr := io.uncached.acquire.meta.bits.payload.addr
  mem_data_q.io.enq.valid := io.uncached.acquire.data.valid
  io.uncached.acquire.data.ready := mem_data_q.io.enq.ready
  mem_data_q.io.enq.bits.data := io.uncached.acquire.data.bits.payload.data 
  io.uncached.grant.valid := io.mem.resp.valid
  io.mem.resp.ready := io.uncached.grant.ready
  io.uncached.grant.bits.payload.data := io.mem.resp.bits.data
  io.uncached.grant.bits.payload.client_xact_id := io.mem.resp.bits.tag
  io.uncached.grant.bits.payload.master_xact_id := UInt(0) // DNC
  io.uncached.grant.bits.payload.g_type := UInt(0) // DNC
  io.mem.req_cmd <> mem_cmd_q.io.deq
  io.mem.req_data <> mem_data_q.io.deq
}

class ReferenceChipCrossbarNetwork(endpoints: Seq[CoherenceAgentRole])(implicit conf: UncoreConfiguration) extends LogicalNetwork[TileLinkIO](endpoints)(conf.tl.ln) {
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)
  val io = Vec(endpoints.map(_ match { case t:ClientCoherenceAgent => {(new TileLinkIO).flip}; case h:MasterCoherenceAgent => {new TileLinkIO}}))
  implicit val pconf = new PhysicalNetworkConfiguration(ln.nEndpoints, ln.idBits) // Same config for all networks

  // Aliases for the various network IO bundle types
  type FBCIO[T <: Data] = DecoupledIO[PhysicalNetworkIO[T]]
  type FLNIO[T <: Data] = DecoupledIO[LogicalNetworkIO[T]]
  type PBCIO[M <: Data, D <: Data] = PairedDataIO[PhysicalNetworkIO[M], PhysicalNetworkIO[D]]
  type PLNIO[M <: Data, D <: Data] = PairedDataIO[LogicalNetworkIO[M], LogicalNetworkIO[D]]
  type FromCrossbar[T <: Data] = FBCIO[T] => FLNIO[T]
  type ToCrossbar[T <: Data] = FLNIO[T] => FBCIO[T]

  // Shims for converting between logical network IOs and physical network IOs
  //TODO: Could be less verbose if you could override subbundles after a <>
  def DefaultFromCrossbarShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = Decoupled(new LogicalNetworkIO()(in.bits.payload.clone))
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
  def CrossbarToMasterShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = DefaultFromCrossbarShim(in)
    out.bits.header.src := in.bits.header.src - UInt(ln.nMasters)
    out
  }
  def CrossbarToClientShim[T <: Data](in: FBCIO[T]): FLNIO[T] = {
    val out = DefaultFromCrossbarShim(in)
    out.bits.header.dst := in.bits.header.dst - UInt(ln.nMasters)
    out
  }
  def DefaultToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = Decoupled(new PhysicalNetworkIO()(in.bits.payload.clone))
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
  def MasterToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = DefaultToCrossbarShim(in)
    out.bits.header.dst := in.bits.header.dst + UInt(ln.nMasters)
    out
  }
  def ClientToCrossbarShim[T <: Data](in: FLNIO[T]): FBCIO[T] = {
    val out = DefaultToCrossbarShim(in)
    out.bits.header.src := in.bits.header.src + UInt(ln.nMasters)
    out
  }

  // Make an individual connection between virtual and physical ports using
  // a particular shim. Also seal the unused FIFO control signal.
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

  // Use reflection to determine whether a particular endpoint should be
  // hooked up as an [input/output] for a FIFO nework that is transmiitting
  // [client/master]-sourced messages.
  def doFIFOHookup[S <: CoherenceAgentRole: ClassTag, T <: Data](end: CoherenceAgentRole, phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T], inShim: ToCrossbar[T], outShim: FromCrossbar[T]) = {
    // Is end's type a subtype of S, the agent type associated with inputs?
    if(classTag[S].runtimeClass.isInstance(end)) 
      doFIFOInputHookup(phys_in, phys_out, log_io, inShim)
    else 
      doFIFOOutputHookup(phys_in, phys_out, log_io, outShim)
  }

  def doClientSourcedFIFOHookup[T <: Data](end: CoherenceAgentRole, phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T]) =
    doFIFOHookup[ClientCoherenceAgent, T](end, phys_in, phys_out, log_io, ClientToCrossbarShim, CrossbarToMasterShim)

  def doMasterSourcedFIFOHookup[T <: Data](end: CoherenceAgentRole, phys_in: FBCIO[T], phys_out: FBCIO[T], log_io: FLNIO[T]) =
    doFIFOHookup[MasterCoherenceAgent, T](end, phys_in, phys_out, log_io, MasterToCrossbarShim, CrossbarToClientShim)
    
  // Use reflection to determine whether a particular endpoint should be
  // hooked up as an [input/output] for a Paired nework that is transmiitting
  // [client/master]-sourced messages.
  def doPairedDataHookup[S <: CoherenceAgentRole : ClassTag, T <: Data, R <: Data](end: CoherenceAgentRole, phys_in: PBCIO[T,R], phys_out: PBCIO[T,R], log_io: PLNIO[T,R], inShim: ToCrossbar[T], outShim: FromCrossbar[T], inShimD: ToCrossbar[R], outShimD: FromCrossbar[R]) = {
    // Is end's type a subtype of S, the agent type associated with inputs?
    if(classTag[S].runtimeClass.isInstance(end)) {
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


  // Actually instantiate the particular networks required for TileLink
  def acqHasData(acq: PhysicalNetworkIO[Acquire]) = co.messageHasData(acq.payload)
  val acq_net = Module(new PairedCrossbar(REFILL_CYCLES, acqHasData _)(new Acquire, new AcquireData))
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doClientSourcedPairedHookup(end, acq_net.io.in(id), acq_net.io.out(id), io.acquire) }

  def relHasData(rel: PhysicalNetworkIO[Release]) = co.messageHasData(rel.payload)
  val rel_net = Module(new PairedCrossbar(REFILL_CYCLES, relHasData _)(new Release, new ReleaseData))
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doClientSourcedPairedHookup(end, rel_net.io.in(id), rel_net.io.out(id), io.release) }

  val probe_net = Module(new BasicCrossbar()(new Probe))
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doMasterSourcedFIFOHookup(end, probe_net.io.in(id), probe_net.io.out(id), io.probe) }

  val grant_net = Module(new BasicCrossbar()(new Grant))
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doMasterSourcedFIFOHookup(end, grant_net.io.in(id), grant_net.io.out(id), io.grant) }

  val ack_net = Module(new BasicCrossbar()(new GrantAck))
  endpoints.zip(io).zipWithIndex.map{ case ((end, io), id) => doClientSourcedFIFOHookup(end, ack_net.io.in(id), ack_net.io.out(id), io.grant_ack) }

  val physicalNetworks = List(acq_net, rel_net, probe_net, grant_net, ack_net)
}
