package referencechip

import Chisel._
import uncore._
import scala.reflect._
import scala.reflect.runtime.universe._

object TileLinkHeaderAppender {
  def apply[T <: ClientSourcedMessage with HasPhysicalAddress, U <: ClientSourcedMessage with HasTileLinkData](in: PairedDataIO[LogicalNetworkIO[T],LogicalNetworkIO[U]], clientId: Int, nBanks: Int, addrConvert: Bits => UInt)(implicit conf: TileLinkConfiguration) = {
    val shim = Module(new TileLinkHeaderAppender(in.meta.bits.payload, in.data.bits.payload, clientId, nBanks, addrConvert))
    shim.io.in <> in
    shim.io.out
  }
  def apply[T <: ClientSourcedMessage with HasPhysicalAddress](in: DecoupledIO[LogicalNetworkIO[T]], clientId: Int, nBanks: Int, addrConvert: Bits => UInt)(implicit conf: TileLinkConfiguration) = {
    val shim = Module(new TileLinkHeaderAppender(in.bits.payload.clone, new AcquireData, clientId, nBanks, addrConvert))
    shim.io.in.meta <> in
    shim.io.out.meta
  }
}

class TileLinkHeaderAppender[T <: ClientSourcedMessage with HasPhysicalAddress, U <: ClientSourcedMessage with HasTileLinkData](mType: T, dType: U, clientId: Int, nBanks: Int, addrConvert: Bits => UInt)(implicit conf: TileLinkConfiguration) extends Module {
  implicit val ln = conf.ln
  val io = new Bundle {
    val in = new PairedDataIO(new LogicalNetworkIO(mType), new LogicalNetworkIO(dType)).flip
    val out = new PairedDataIO(new LogicalNetworkIO(mType), new LogicalNetworkIO(dType))
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

class ReferenceChipCrossbarNetwork(implicit conf: UncoreConfiguration) extends LogicalNetwork[TileLinkIO]()(conf.tl.ln) {
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)
  val io = new Bundle {
    val clients = Vec.fill(ln.nClients){(new TileLinkIO).flip}
    val masters = Vec.fill(ln.nMasters){new TileLinkIO}
  }
  implicit val pconf = new PhysicalNetworkConfiguration(ln.nEndpoints, ln.idBits) // Same config for all networks

  // Actually instantiate the particular networks required for TileLink
  val acqNet = Module(new PairedCrossbar(new Acquire, new AcquireData, REFILL_CYCLES, (acq: PhysicalNetworkIO[Acquire]) => co.messageHasData(acq.payload)))
  val relNet = Module(new PairedCrossbar(new Release, new ReleaseData, REFILL_CYCLES, (rel: PhysicalNetworkIO[Release]) => co.messageHasData(rel.payload)))
  val probeNet = Module(new BasicCrossbar(new Probe))
  val grantNet = Module(new BasicCrossbar(new Grant))
  val ackNet = Module(new BasicCrossbar(new GrantAck))

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
    val out = Decoupled(new LogicalNetworkIO(in.bits.payload)).asDirectionless
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
    val out = Decoupled(new PhysicalNetworkIO(in.bits.payload)).asDirectionless
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

  def doFIFOHookup[T <: Data](isEndpointSourceOfMessage: Boolean, physIn: FBCIO[T], physOut: FBCIO[T], logIO: FLNIO[T], inShim: ToCrossbar[T], outShim: FromCrossbar[T]) = {
    if(isEndpointSourceOfMessage) doFIFOInputHookup(physIn, physOut, logIO, inShim)
    else                 doFIFOOutputHookup(physIn, physOut, logIO, outShim)
  }
    
  //Hookup all instances of a particular subbundle of 
  def doFIFOHookups[T <: Data: TypeTag](physIO: BasicCrossbarIO[T], getLogIO: TileLinkIO => FLNIO[T]) = {
    typeTag[T].tpe match{ 
      case t if t <:< typeTag[ClientSourcedMessage].tpe => {
        io.masters.zipWithIndex.map{ case (i, id) => doFIFOHookup[T](false, physIO.in(id), physIO.out(id), getLogIO(i), ClientToCrossbarShim, CrossbarToMasterShim) }
        io.clients.zipWithIndex.map{ case (i, id) => doFIFOHookup[T](true, physIO.in(id+ln.nMasters), physIO.out(id+ln.nMasters), getLogIO(i), ClientToCrossbarShim, CrossbarToMasterShim) }
      }
      case t if t <:< typeTag[MasterSourcedMessage].tpe => {
        io.masters.zipWithIndex.map{ case (i, id) => doFIFOHookup[T](true, physIO.in(id), physIO.out(id), getLogIO(i), MasterToCrossbarShim, CrossbarToClientShim) }
        io.clients.zipWithIndex.map{ case (i, id) => doFIFOHookup[T](false, physIO.in(id+ln.nMasters), physIO.out(id+ln.nMasters), getLogIO(i), MasterToCrossbarShim, CrossbarToClientShim) }
      }
      case _ => require(false, "Unknown message sourcing.")
    }
  }

  def doPairedDataHookup[T <: Data, R <: Data](isEndpointSourceOfMessage: Boolean, physIn: PBCIO[T,R], physOut: PBCIO[T,R], logIO: PLNIO[T,R], inShim: ToCrossbar[T], outShim: FromCrossbar[T], inShimD: ToCrossbar[R], outShimD: FromCrossbar[R]) = {
    if(isEndpointSourceOfMessage) {
      doFIFOInputHookup[T](physIn.meta, physOut.meta, logIO.meta, inShim)
      doFIFOInputHookup[R](physIn.data, physOut.data, logIO.data, inShimD)
    } else {
      doFIFOOutputHookup[T](physIn.meta, physOut.meta, logIO.meta, outShim)
      doFIFOOutputHookup[R](physIn.data, physOut.data, logIO.data, outShimD)
    }
  }

  def doPairedDataHookups[T <: Data: TypeTag, R <: Data](physIO: PairedCrossbarIO[T,R], getLogIO: TileLinkIO => PLNIO[T,R]) = {
    typeTag[T].tpe match{ 
      case t if t <:< typeTag[ClientSourcedMessage].tpe => {
        io.masters.zipWithIndex.map{ case (i, id) => doPairedDataHookup[T,R](false, physIO.in(id), physIO.out(id), getLogIO(i), ClientToCrossbarShim, CrossbarToMasterShim, ClientToCrossbarShim, CrossbarToMasterShim) }
        io.clients.zipWithIndex.map{ case (i, id) => doPairedDataHookup[T,R](true, physIO.in(id+ln.nMasters), physIO.out(id+ln.nMasters), getLogIO(i), ClientToCrossbarShim, CrossbarToMasterShim, ClientToCrossbarShim, CrossbarToMasterShim) }
      }
      case t if t <:< typeTag[MasterSourcedMessage].tpe => {
        io.masters.zipWithIndex.map{ case (i, id) => doPairedDataHookup[T,R](true, physIO.in(id), physIO.out(id), getLogIO(i), MasterToCrossbarShim, CrossbarToClientShim, MasterToCrossbarShim, CrossbarToClientShim) }
        io.clients.zipWithIndex.map{ case (i, id) => doPairedDataHookup[T,R](false, physIO.in(id+ln.nMasters), physIO.out(id+ln.nMasters), getLogIO(i), MasterToCrossbarShim, CrossbarToClientShim, MasterToCrossbarShim, CrossbarToClientShim) }
      }
      case _ => require(false, "Unknown message sourcing.")
    }
  }

  doPairedDataHookups(acqNet.io, (tl: TileLinkIO) => tl.acquire)
  doPairedDataHookups(relNet.io, (tl: TileLinkIO) => tl.release)
  doFIFOHookups(probeNet.io, (tl: TileLinkIO) => tl.probe)
  doFIFOHookups(grantNet.io, (tl: TileLinkIO) => tl.grant)
  doFIFOHookups(ackNet.io, (tl: TileLinkIO) => tl.grant_ack)
}
