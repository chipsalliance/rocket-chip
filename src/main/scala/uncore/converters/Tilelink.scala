package uncore.converters

import Chisel._
import util.{ReorderQueue, DecoupledHelper}
import junctions.PAddrBits
import uncore.tilelink._
import uncore.util._
import uncore.constants._
import cde.Parameters

/** Utilities for safely wrapping a *UncachedTileLink by pinning probe.ready and release.valid low */
object TileLinkIOWrapper {
  def apply(tl: ClientUncachedTileLinkIO)(implicit p: Parameters): ClientTileLinkIO = {
    val conv = Module(new ClientTileLinkIOWrapper)
    conv.io.in <> tl
    conv.io.out
  }
  def apply(tl: UncachedTileLinkIO)(implicit p: Parameters): TileLinkIO = {
    val conv = Module(new TileLinkIOWrapper)
    conv.io.in <> tl
    conv.io.out
  }
  def apply(tl: ClientTileLinkIO): ClientTileLinkIO = tl
  def apply(tl: TileLinkIO): TileLinkIO = tl
}

class TileLinkIOWrapper(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in = new UncachedTileLinkIO().flip
    val out = new TileLinkIO
  }
  io.out.acquire <> io.in.acquire
  io.in.grant <> io.out.grant
  io.out.finish <> io.in.finish
  io.out.probe.ready := Bool(true)
  io.out.release.valid := Bool(false)
}

class ClientTileLinkIOWrapper(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in = new ClientUncachedTileLinkIO().flip
    val out = new ClientTileLinkIO
  }
  io.out.acquire <> io.in.acquire
  io.in.grant <> io.out.grant
  io.out.probe.ready := Bool(true)
  io.out.release.valid := Bool(false)
  io.out.finish.valid := Bool(false)
}

class ClientTileLinkIOUnwrapper(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in = new ClientTileLinkIO().flip
    val out = new ClientUncachedTileLinkIO
  }

  val acqArb = Module(new LockingRRArbiter(new Acquire, 2, tlDataBeats,
    Some((acq: Acquire) => acq.hasMultibeatData())))

  val acqRoq = Module(new ReorderQueue(Bool(), tlClientXactIdBits))
  val relRoq = Module(new ReorderQueue(Bool(), tlClientXactIdBits))

  val iacq = io.in.acquire.bits
  val irel = io.in.release.bits
  val ognt = io.out.grant.bits

  val acq_roq_enq = iacq.first()
  val rel_roq_enq = irel.first()

  val acq_roq_ready = !acq_roq_enq || acqRoq.io.enq.ready
  val rel_roq_ready = !rel_roq_enq || relRoq.io.enq.ready

  val acq_helper = DecoupledHelper(
    io.in.acquire.valid,
    acq_roq_ready,
    acqArb.io.in(0).ready)

  val rel_helper = DecoupledHelper(
    io.in.release.valid,
    rel_roq_ready,
    acqArb.io.in(1).ready)

  acqRoq.io.enq.valid := acq_helper.fire(acq_roq_ready, acq_roq_enq)
  acqRoq.io.enq.bits.data := iacq.isBuiltInType()
  acqRoq.io.enq.bits.tag := iacq.client_xact_id

  acqArb.io.in(0).valid := acq_helper.fire(acqArb.io.in(0).ready)
  acqArb.io.in(0).bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = Mux(iacq.isBuiltInType(),
      iacq.a_type, Acquire.getBlockType),
    client_xact_id = iacq.client_xact_id,
    addr_block = iacq.addr_block,
    addr_beat = iacq.addr_beat,
    data = iacq.data,
    union = iacq.union)
  io.in.acquire.ready := acq_helper.fire(io.in.acquire.valid)

  relRoq.io.enq.valid := rel_helper.fire(rel_roq_ready, rel_roq_enq)
  relRoq.io.enq.bits.data := irel.isVoluntary()
  relRoq.io.enq.bits.tag := irel.client_xact_id

  acqArb.io.in(1).valid := rel_helper.fire(acqArb.io.in(1).ready)
  acqArb.io.in(1).bits := PutBlock(
    client_xact_id = irel.client_xact_id,
    addr_block = irel.addr_block,
    addr_beat = irel.addr_beat,
    data = irel.data)
  io.in.release.ready := rel_helper.fire(io.in.release.valid)

  io.out.acquire <> acqArb.io.out

  val grant_deq_roq = io.out.grant.fire() && ognt.last()

  acqRoq.io.deq.valid := acqRoq.io.deq.matches && grant_deq_roq
  acqRoq.io.deq.tag := ognt.client_xact_id

  relRoq.io.deq.valid := !acqRoq.io.deq.matches && grant_deq_roq
  relRoq.io.deq.tag := ognt.client_xact_id

  assert(!grant_deq_roq || acqRoq.io.deq.matches || relRoq.io.deq.matches,
    "TileLink Unwrapper: client_xact_id mismatch")

  val gnt_builtin = acqRoq.io.deq.data
  val gnt_voluntary = relRoq.io.deq.data

  val acq_grant = Grant(
    is_builtin_type = gnt_builtin,
    g_type = Mux(gnt_builtin, ognt.g_type, tlCoh.getExclusiveGrantType),
    client_xact_id = ognt.client_xact_id,
    manager_xact_id = ognt.manager_xact_id,
    addr_beat = ognt.addr_beat,
    data = ognt.data)

  assert(!io.in.release.valid || io.in.release.bits.isVoluntary(), "Unwrapper can only process voluntary releases.")
  val rel_grant = Grant(
    is_builtin_type = Bool(true),
    g_type = Grant.voluntaryAckType, // We should only every be working with voluntary releases
    client_xact_id = ognt.client_xact_id,
    manager_xact_id = ognt.manager_xact_id,
    addr_beat = ognt.addr_beat,
    data = ognt.data)

  io.in.grant.valid := io.out.grant.valid
  io.in.grant.bits := Mux(acqRoq.io.deq.matches, acq_grant, rel_grant)
  io.out.grant.ready := io.in.grant.ready

  io.in.probe.valid := Bool(false)
  io.in.finish.ready := Bool(false)
}

object TileLinkIOUnwrapper {
  def apply(in: ClientTileLinkIO)(implicit p: Parameters): ClientUncachedTileLinkIO = {
    val unwrapper = Module(new ClientTileLinkIOUnwrapper)
    unwrapper.io.in <> in
    unwrapper.io.out
  }
}

object TileLinkWidthAdapter {
  def apply(in: ClientUncachedTileLinkIO, outerId: String)(implicit p: Parameters) = {
    val outerDataBits = p(TLKey(outerId)).dataBitsPerBeat
    if (outerDataBits > in.tlDataBits) {
      val widener = Module(new TileLinkIOWidener(in.p(TLId), outerId))
      widener.io.in <> in
      widener.io.out
    } else if (outerDataBits < in.tlDataBits) {
      val narrower = Module(new TileLinkIONarrower(in.p(TLId), outerId))
      narrower.io.in <> in
      narrower.io.out
    } else { in }
  }
  def apply(out: ClientUncachedTileLinkIO, in: ClientUncachedTileLinkIO)(implicit p: Parameters): Unit = {
    require(out.tlDataBits * out.tlDataBeats == in.tlDataBits * in.tlDataBeats)
    out <> apply(in, out.p(TLId))
  }
}

class TileLinkIOWidener(innerTLId: String, outerTLId: String)
    (implicit p: Parameters) extends TLModule()(p) {

  val paddrBits = p(PAddrBits)
  val innerParams = p(TLKey(innerTLId))
  val outerParams = p(TLKey(outerTLId)) 
  val innerDataBeats = innerParams.dataBeats
  val innerDataBits = innerParams.dataBitsPerBeat
  val innerWriteMaskBits = innerParams.writeMaskBits
  val innerByteAddrBits = log2Up(innerWriteMaskBits)
  val innerMaxXacts = innerParams.maxClientXacts * innerParams.maxClientsPerPort
  val innerXactIdBits = log2Up(innerMaxXacts)
  val outerDataBeats = outerParams.dataBeats
  val outerDataBits = outerParams.dataBitsPerBeat
  val outerWriteMaskBits = outerParams.writeMaskBits
  val outerByteAddrBits = log2Up(outerWriteMaskBits)
  val outerBeatAddrBits = log2Up(outerDataBeats)
  val outerBlockOffset = outerBeatAddrBits + outerByteAddrBits
  val outerMaxClients = outerParams.maxClientsPerPort
  val outerClientIdBits = log2Up(outerParams.maxClientXacts * outerMaxClients)
  val outerManagerIdBits = log2Up(outerParams.maxManagerXacts)
  val outerBlockAddrBits = paddrBits - outerBlockOffset

  require(outerDataBeats <= innerDataBeats)
  require(outerDataBits >= innerDataBits)
  require(outerDataBits % innerDataBits == 0)
  require(outerDataBits * outerDataBeats == innerDataBits * innerDataBeats)

  val factor = innerDataBeats / outerDataBeats

  val io = new Bundle {
    val in = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => innerTLId})).flip
    val out = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => outerTLId}))
  }

  val iacq = io.in.acquire.bits
  val oacq = io.out.acquire.bits
  val ognt = io.out.grant.bits
  val ignt = io.in.grant.bits

  val shrink = iacq.a_type === Acquire.putBlockType
  val stretch = ognt.g_type === Grant.getDataBlockType
  val smallget = iacq.a_type === Acquire.getType
  val smallput = iacq.a_type === Acquire.putType
  val smallgnt = ognt.g_type === Grant.getDataBeatType

  val sending_put = Reg(init = Bool(false))
  val collecting = Reg(init = Bool(false))
  val put_block = Reg(UInt(width = outerBlockAddrBits))
  val put_id = Reg(UInt(width = outerClientIdBits))
  val put_data = Reg(Vec(factor, UInt(width = innerDataBits)))
  val put_wmask = Reg(Vec(factor, UInt(width = innerWriteMaskBits)))
  val put_allocate = Reg(Bool())
  val (put_beat, put_done) = Counter(io.out.acquire.fire() && oacq.hasMultibeatData(), outerDataBeats)
  val (recv_idx, recv_done) = Counter(io.in.acquire.fire() && iacq.hasMultibeatData(), factor)

  val in_addr = iacq.full_addr()
  val out_addr_block = in_addr(paddrBits - 1, outerBlockOffset)
  val out_addr_beat  = in_addr(outerBlockOffset - 1, outerByteAddrBits)
  val out_addr_byte  = in_addr(outerByteAddrBits - 1, 0)

  val switch_addr = in_addr(outerByteAddrBits - 1, innerByteAddrBits)
  val smallget_switch = Reg(Vec(innerMaxXacts, switch_addr))

  def align_data(addr: UInt, data: UInt): UInt =
    data << Cat(addr, UInt(0, log2Up(innerDataBits)))

  def align_wmask(addr: UInt, wmask: UInt): UInt =
    wmask << Cat(addr, UInt(0, log2Up(innerWriteMaskBits)))

  val outerConfig = p.alterPartial({ case TLId => outerTLId })

  val get_acquire = Get(
    client_xact_id = iacq.client_xact_id,
    addr_block = out_addr_block,
    addr_beat = out_addr_beat,
    addr_byte = out_addr_byte,
    operand_size = iacq.op_size(),
    alloc = iacq.allocate())(outerConfig)

  val get_block_acquire = GetBlock(
    client_xact_id = iacq.client_xact_id,
    addr_block = out_addr_block,
    alloc = iacq.allocate())(outerConfig)

  val put_acquire = Put(
    client_xact_id = iacq.client_xact_id,
    addr_block = out_addr_block,
    addr_beat = out_addr_beat,
    data = align_data(switch_addr, iacq.data),
    wmask = Some(align_wmask(switch_addr, iacq.wmask())),
    alloc = iacq.allocate())(outerConfig)

  val put_block_acquire = PutBlock(
    client_xact_id = put_id,
    addr_block = put_block,
    addr_beat = put_beat,
    data = put_data.asUInt,
    wmask = Some(put_wmask.asUInt))(outerConfig)

  io.out.acquire.valid := sending_put || (!shrink && io.in.acquire.valid)
  io.out.acquire.bits := MuxCase(get_block_acquire, Seq(
    sending_put -> put_block_acquire,
    smallget -> get_acquire,
    smallput -> put_acquire))
  io.in.acquire.ready := !sending_put && (shrink || io.out.acquire.ready)

  when (io.in.acquire.fire() && shrink) {
    when (!collecting) {
      put_block := out_addr_block
      put_id := iacq.client_xact_id
      put_allocate := iacq.allocate()
      collecting := Bool(true)
    }
    put_data(recv_idx) := iacq.data
    put_wmask(recv_idx) := iacq.wmask()
  }

  when (io.in.acquire.fire() && smallget) {
    smallget_switch(iacq.client_xact_id) := switch_addr
  }

  when (recv_done) { sending_put := Bool(true) }
  when (sending_put && io.out.acquire.ready) { sending_put := Bool(false) }
  when (put_done) { collecting := Bool(false) }

  val returning_data = Reg(init = Bool(false))
  val (send_idx, send_done) = Counter(
    io.in.grant.ready && returning_data, factor)

  val gnt_beat = Reg(UInt(width = outerBeatAddrBits))
  val gnt_client_id = Reg(UInt(width = outerClientIdBits))
  val gnt_manager_id = Reg(UInt(width = outerManagerIdBits))
  val gnt_data = Reg(UInt(width = outerDataBits))

  when (io.out.grant.fire() && stretch) {
    gnt_data := ognt.data
    gnt_client_id := ognt.client_xact_id
    gnt_manager_id := ognt.manager_xact_id
    gnt_beat := ognt.addr_beat
    returning_data := Bool(true)
  }

  when (send_done) { returning_data := Bool(false) }

  def select_data(data: UInt, sel: UInt): UInt =
    data >> (sel << log2Up(innerDataBits))

  val gnt_switch = smallget_switch(ognt.client_xact_id)

  val innerConfig = p.alterPartial({ case TLId => innerTLId })

  val get_block_grant = Grant(
    is_builtin_type = Bool(true),
    g_type = Grant.getDataBlockType,
    client_xact_id = gnt_client_id,
    manager_xact_id = gnt_manager_id,
    addr_beat = Cat(gnt_beat, send_idx),
    data = select_data(gnt_data, send_idx))(innerConfig)

  val get_grant = Grant(
    is_builtin_type = Bool(true),
    g_type = Grant.getDataBeatType,
    client_xact_id = ognt.client_xact_id,
    manager_xact_id = ognt.manager_xact_id,
    addr_beat = Cat(ognt.addr_beat, gnt_switch),
    data = select_data(ognt.data, gnt_switch))(innerConfig)

  val default_grant = Grant(
    is_builtin_type = Bool(true),
    g_type = ognt.g_type,
    client_xact_id = ognt.client_xact_id,
    manager_xact_id = ognt.manager_xact_id,
    addr_beat = ognt.addr_beat,
    data = ognt.data)(innerConfig)

  io.in.grant.valid := returning_data || (!stretch && io.out.grant.valid)
  io.in.grant.bits := MuxCase(default_grant, Seq(
    returning_data -> get_block_grant,
    smallgnt -> get_grant))
  io.out.grant.ready := !returning_data && (stretch || io.in.grant.ready)
}

class TileLinkIONarrower(innerTLId: String, outerTLId: String)
    (implicit p: Parameters) extends TLModule()(p) {

  val innerParams = p(TLKey(innerTLId))
  val outerParams = p(TLKey(outerTLId)) 
  val innerDataBeats = innerParams.dataBeats
  val innerDataBits = innerParams.dataBitsPerBeat
  val innerWriteMaskBits = innerParams.writeMaskBits
  val innerByteAddrBits = log2Up(innerWriteMaskBits)
  val outerDataBeats = outerParams.dataBeats
  val outerDataBits = outerParams.dataBitsPerBeat
  val outerWriteMaskBits = outerParams.writeMaskBits
  val outerByteAddrBits = log2Up(outerWriteMaskBits)
  val outerBeatAddrBits = log2Up(outerDataBeats)
  val outerBlockOffset = outerBeatAddrBits + outerByteAddrBits
  val outerMaxClients = outerParams.maxClientsPerPort
  val outerIdBits = log2Up(outerParams.maxClientXacts * outerMaxClients)

  require(outerDataBeats > innerDataBeats)
  require(outerDataBeats % innerDataBeats == 0)
  require(outerDataBits < innerDataBits)
  require(outerDataBits * outerDataBeats == innerDataBits * innerDataBeats)

  val factor = outerDataBeats / innerDataBeats

  val io = new Bundle {
    val in = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => innerTLId})).flip
    val out = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => outerTLId}))
  }

  val iacq = io.in.acquire.bits
  val ognt = io.out.grant.bits

  val stretch = iacq.a_type === Acquire.putBlockType
  val shrink = iacq.a_type === Acquire.getBlockType
  val smallput = iacq.a_type === Acquire.putType
  val smallget = iacq.a_type === Acquire.getType

  val acq_data_buffer = Reg(UInt(width = innerDataBits))
  val acq_wmask_buffer = Reg(UInt(width = innerWriteMaskBits))
  val acq_client_id = Reg(iacq.client_xact_id)
  val acq_addr_block = Reg(iacq.addr_block)
  val acq_addr_beat = Reg(iacq.addr_beat)
  val oacq_ctr = Counter(factor)

  val outer_beat_addr = iacq.full_addr()(outerBlockOffset - 1, outerByteAddrBits)
  val outer_byte_addr = iacq.full_addr()(outerByteAddrBits - 1, 0)

  val mask_chunks = Vec.tabulate(factor) { i =>
    val lsb = i * outerWriteMaskBits
    val msb = (i + 1) * outerWriteMaskBits - 1
    iacq.wmask()(msb, lsb)
  }

  val data_chunks = Vec.tabulate(factor) { i =>
    val lsb = i * outerDataBits
    val msb = (i + 1) * outerDataBits - 1
    iacq.data(msb, lsb)
  }

  val beat_sel = Cat(mask_chunks.map(mask => mask.orR).reverse)

  val smallput_data = Mux1H(beat_sel, data_chunks)
  val smallput_wmask = Mux1H(beat_sel, mask_chunks)
  val smallput_beat = Cat(iacq.addr_beat, PriorityEncoder(beat_sel))

  assert(!io.in.acquire.valid || !smallput || PopCount(beat_sel) <= UInt(1),
    "Can't perform Put wider than outer width")

  val read_size_ok = iacq.op_size() <= UInt(log2Ceil(outerDataBits / 8))
  assert(!io.in.acquire.valid || !smallget || read_size_ok,
    "Can't perform Get wider than outer width")

  val outerConfig = p.alterPartial({ case TLId => outerTLId })
  val innerConfig = p.alterPartial({ case TLId => innerTLId })

  val get_block_acquire = GetBlock(
    client_xact_id = iacq.client_xact_id,
    addr_block = iacq.addr_block,
    alloc = iacq.allocate())(outerConfig)

  val put_block_acquire = PutBlock(
    client_xact_id = acq_client_id,
    addr_block = acq_addr_block,
    addr_beat = if (factor > 1)
                  Cat(acq_addr_beat, oacq_ctr.value)
                else acq_addr_beat,
    data = acq_data_buffer(outerDataBits - 1, 0),
    wmask = Some(acq_wmask_buffer(outerWriteMaskBits - 1, 0)))(outerConfig)

  val get_acquire = Get(
    client_xact_id = iacq.client_xact_id,
    addr_block = iacq.addr_block,
    addr_beat = outer_beat_addr,
    addr_byte = outer_byte_addr,
    operand_size = iacq.op_size(),
    alloc = iacq.allocate())(outerConfig)

  val put_acquire = Put(
    client_xact_id = iacq.client_xact_id,
    addr_block = iacq.addr_block,
    addr_beat = smallput_beat,
    data = smallput_data,
    wmask = Some(smallput_wmask))(outerConfig)

  val sending_put = Reg(init = Bool(false))

  val pass_valid = io.in.acquire.valid && !stretch

  io.out.acquire.bits := MuxCase(Wire(io.out.acquire.bits, init=iacq), Seq(
    (sending_put, put_block_acquire),
    (shrink, get_block_acquire),
    (smallput, put_acquire),
    (smallget, get_acquire)))
  io.out.acquire.valid := sending_put || pass_valid
  io.in.acquire.ready := !sending_put && (stretch || io.out.acquire.ready)

  when (io.in.acquire.fire() && stretch) {
    acq_data_buffer := iacq.data
    acq_wmask_buffer := iacq.wmask()
    acq_client_id := iacq.client_xact_id
    acq_addr_block := iacq.addr_block
    acq_addr_beat := iacq.addr_beat
    sending_put := Bool(true)
  }

  when (sending_put && io.out.acquire.ready) {
    acq_data_buffer := acq_data_buffer >> outerDataBits
    acq_wmask_buffer := acq_wmask_buffer >> outerWriteMaskBits
    when (oacq_ctr.inc()) { sending_put := Bool(false) }
  }

  val ognt_block = ognt.hasMultibeatData()
  val gnt_data_buffer = Reg(Vec(factor, UInt(width = outerDataBits)))
  val gnt_client_id = Reg(ognt.client_xact_id)
  val gnt_manager_id = Reg(ognt.manager_xact_id)

  val ignt_ctr = Counter(innerDataBeats)
  val ognt_ctr = Counter(factor)
  val sending_get = Reg(init = Bool(false))

  val get_block_grant = Grant(
    is_builtin_type = Bool(true),
    g_type = Grant.getDataBlockType,
    client_xact_id = gnt_client_id,
    manager_xact_id = gnt_manager_id,
    addr_beat = ignt_ctr.value,
    data = gnt_data_buffer.asUInt)(innerConfig)

  val smallget_grant = ognt.g_type === Grant.getDataBeatType

  val get_grant = Grant(
    is_builtin_type = Bool(true),
    g_type = Grant.getDataBeatType,
    client_xact_id = ognt.client_xact_id,
    manager_xact_id = ognt.manager_xact_id,
    addr_beat = ognt.addr_beat >> UInt(log2Up(factor)),
    data = Fill(factor, ognt.data))(innerConfig)

  io.in.grant.valid := sending_get || (io.out.grant.valid && !ognt_block)
  io.out.grant.ready := !sending_get && (ognt_block || io.in.grant.ready)

  io.in.grant.bits := MuxCase(Wire(io.in.grant.bits, init=ognt), Seq(
    sending_get -> get_block_grant,
    smallget_grant -> get_grant))

  when (io.out.grant.valid && ognt_block && !sending_get) {
    gnt_data_buffer(ognt_ctr.value) := ognt.data
    when (ognt_ctr.inc()) {
      gnt_client_id := ognt.client_xact_id
      gnt_manager_id := ognt.manager_xact_id
      sending_get := Bool(true)
    }
  }

  when (io.in.grant.ready && sending_get) {
    ignt_ctr.inc()
    sending_get := Bool(false)
  }
}

class TileLinkFragmenterSource(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in  = Decoupled(new Acquire).flip
    val out = Decoupled(new Acquire)
    val que = Decoupled(UInt(width = tlBeatAddrBits))
  }

  // Pipeline stage with acquire data; needed to ensure in.bits stay fixed when !in.ready
  val acq_valid = RegInit(Bool(false))
  val acq_bits  = Reg(new Acquire)
  // The last beat of generate acquire to send
  val acq_last_beat = Reg(UInt(width = tlBeatAddrBits))
  val acq_last = acq_bits.addr_beat === acq_last_beat

  // 'in' has the first beat?
  val in_multi_put = io.in.bits.isBuiltInType(Acquire.putBlockType)
  val in_multi_get = io.in.bits.isBuiltInType(Acquire.getBlockType)
  val in_first_beat = !in_multi_put || io.in.bits.addr_beat === UInt(0)

  // Move stuff from acq to out whenever out is ready
  io.out.valid := acq_valid
  // When can acq accept a request?
  val acq_ready = !acq_valid || (acq_last && io.out.ready)
  // Move the first beat from in to acq only when both acq and que are ready
  io.in.ready := (!in_first_beat || io.que.ready) && acq_ready
  io.que.valid := (in_first_beat && io.in.valid) && acq_ready

  // in.fire moves data from in to acq and (optionally) que
  // out.fire moves data from acq to out

  // Desired flow control results:
  assert (!io.que.fire() || io.in.fire())                               // 1. que.fire => in.fire
  assert (!(io.in.fire() && in_first_beat) || io.que.fire())            // 2. in.fire && in_first_beat => que.fire
  assert (!io.out.fire() || acq_valid)                                  // 3. out.fire => acq_valid
  assert (!io.in.fire() || (!acq_valid || (io.out.fire() && acq_last))) // 4. in.fire => !acq_valid || (out.fire && acq_last)
  // Proofs:
  // 1. que.fire => que.ready && in.valid && acq_ready => in.ready && in.valid
  // 2. in.fire && in_first_beat => in.valid && acq_ready && [(!in_first_beat || que.ready) && in_first_beat] =>
  //   in.valid && acq_ready && que.ready && in_first_beat => que.valid && que.ready
  // 3. out.fire => out.valid => acq_valid
  // 4. in.fire => acq_ready => !acq_valid || (acq_last && out.ready) =>
  //   !acq_valid || (acq_valid && acq_last && out.ready) => !acq_valid || (acq_last && out.fire)

  val multi_size = SInt(-1, width = tlBeatAddrBits).asUInt // TL2: use in.bits.size()/beatBits-1
  val in_sizeMinus1 = Mux(in_multi_get || in_multi_put, multi_size, UInt(0))
  val in_insertSizeMinus1 = Mux(in_multi_get, multi_size, UInt(0))

  when (io.in.fire()) {
    // Theorem 4 makes this safe; we overwrite garbage, or replace the final acq
    acq_valid := Bool(true)
    acq_bits := io.in.bits
    acq_last_beat := io.in.bits.addr_beat + in_insertSizeMinus1
    // Replace this with size truncation in TL2:
    acq_bits.a_type := Mux(in_multi_put, Acquire.putType, Mux(in_multi_get, Acquire.getType, io.in.bits.a_type))
  } .elsewhen (io.out.fire()) {
    acq_valid := !acq_last // false => !in.valid || (!que.ready && in_first_beat)
    acq_bits.addr_beat := acq_bits.addr_beat + UInt(1)
    // acq_last && out.fire => acq_last && out.ready && acq_valid => acq_ready
    // Suppose in.valid, then !in.fire => !in.ready => !(!in_first_beat || que.ready) => !que.ready && in_first_beat
  }

  // Safe by theorem 3
  io.out.bits := acq_bits
  // Safe by theorem 1
  io.que.bits := in_sizeMinus1
}

class TileLinkFragmenterSink(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in  = Decoupled(new Grant).flip
    val out = Decoupled(new Grant)
    val que = Decoupled(UInt(width = tlBeatAddrBits)).flip
  }

  val count_valid = RegInit(Bool(false))
  val multi_op = Reg(Bool())
  val count_bits = Reg(UInt(width = tlBeatAddrBits))
  val last = count_bits === UInt(0)

  val in_put = io.in.bits.isBuiltInType(Grant.putAckType)
  val in_get = io.in.bits.isBuiltInType(Grant.getDataBeatType)
  val deliver = last || in_get

  // Accept the input, discarding the non-final put grant
  io.in.ready := count_valid && (io.out.ready || !deliver)
  // Output the grant whenever we want delivery
  io.out.valid := count_valid && io.in.valid && deliver
  // Take a new number whenever we deliver the last beat
  io.que.ready := !count_valid || (io.in.valid && io.out.ready && last)

  // Desired flow control results:
  assert (!io.out.fire() || (count_valid && io.in.fire()))   // 1. out.fire => in.fire && count_valid
  assert (!(io.in.fire() && deliver) || io.out.fire())       // 2. in.fire && deliver => out.fire
  assert (!(io.out.fire() && last) || io.que.ready)          // 3. out.fire && last => que.ready
  assert (!io.que.fire() || (!count_valid || io.out.fire())) // 4. que.fire => !count_valid || (out.fire && last)
  // Proofs:
  // 1. out.fire => out.ready && (count_valid && in.valid && deliver) => (count_valid && out.ready) && in.valid => in.fire
  // 2. in.fire && deliver => in.valid && count_valid && [(out.ready || !deliver) && deliver] =>
  //      in.valid && count_valid && deliver && out.ready => out.fire
  // 3. out.fire && last => out.valid && out.ready && last => in.valid && out.ready && last => que.ready
  // 4. que.fire => que.valid && (!count_valid || (in.valid && out.ready && last))
  //             => !count_valid || (count_valid && in.valid && out.ready && [last => deliver])
  //             => !count_valid || (out.valid && out.ready && last)

  when (io.que.fire()) {
    // Theorem 4 makes this safe; we overwrite garbage or last output
    count_valid := Bool(true)
    count_bits := io.que.bits
    multi_op := io.que.bits =/= UInt(0)
  } .elsewhen (io.in.fire()) {
    count_valid := !last // false => !que.valid
    count_bits := count_bits - UInt(1)
    // Proof: in.fire && [last => deliver] =2=> out.fire && last =3=> que.ready
    //  !que.fire && que.ready => !que.valid
  }

  // Safe by Theorem 1
  io.out.bits := io.in.bits
  io.out.bits.g_type := Mux(multi_op, Mux(in_get, Grant.getDataBlockType, Grant.putAckType), io.in.bits.g_type)
}

class TileLinkFragmenter(depth: Int = 1)(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in = new ClientUncachedTileLinkIO().flip
    val out = new ClientUncachedTileLinkIO
  }

  // TL2:
  // supportsAcquire = false
  // modify all outward managers to supportsMultibeat = true
  // assert: all managers must behaveFIFO (not inspect duplicated id field)

  val source = Module(new TileLinkFragmenterSource)
  val sink = Module(new TileLinkFragmenterSink)
  sink.io.que <> Queue(source.io.que, depth)

  source.io.in <> io.in.acquire
  io.out.acquire <> source.io.out
  sink.io.in <> io.out.grant
  io.in.grant <> sink.io.out
}

object TileLinkFragmenter {
  // Pass the source/client to fragment
  def apply(source: ClientUncachedTileLinkIO, depth: Int = 1)(implicit p: Parameters): ClientUncachedTileLinkIO = {
    val fragmenter = Module(new TileLinkFragmenter(depth))
    fragmenter.io.in <> source
    fragmenter.io.out
  }
}
