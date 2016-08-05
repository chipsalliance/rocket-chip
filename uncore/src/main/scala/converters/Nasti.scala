package uncore.converters

import Chisel._
import junctions._
import uncore.tilelink._
import uncore.constants._
import cde.Parameters
import scala.math.min

class IdMapper(val inIdBits: Int, val outIdBits: Int,
               val forceMapping: Boolean = false)
              (implicit val p: Parameters) extends Module {

  val io = new Bundle {
    val req = new Bundle {
      val valid  = Bool(INPUT)
      val ready  = Bool(OUTPUT)
      val in_id  = UInt(INPUT, inIdBits)
      val out_id = UInt(OUTPUT, outIdBits)
    }
    val resp = new Bundle {
      val valid   = Bool(INPUT)
      val matches = Bool(OUTPUT)
      val out_id  = UInt(INPUT, outIdBits)
      val in_id   = UInt(OUTPUT, inIdBits)
    }
  }
  val maxInXacts = 1 << inIdBits

  if (inIdBits <= outIdBits && !forceMapping) {
    io.req.ready := Bool(true)
    io.req.out_id := io.req.in_id
    io.resp.matches := Bool(true)
    io.resp.in_id := io.resp.out_id
  } else {
    val nInXacts = 1 << inIdBits
    // No point in allowing more out xacts than in xacts
    val nOutXacts = min(1 << outIdBits, nInXacts)

    val out_id_free = Reg(init = Vec.fill(nOutXacts){Bool(true)})
    val in_id_free = Reg(init = Vec.fill(nInXacts){Bool(true)})
    val next_out_id = PriorityEncoder(out_id_free)
    val id_mapping = Reg(Vec(nOutXacts, UInt(0, inIdBits)))

    val req_fire = io.req.valid && io.req.ready
    when (req_fire) {
      out_id_free(io.req.out_id) := Bool(false)
      in_id_free(io.req.in_id) := Bool(false)
      id_mapping(io.req.out_id) := io.req.in_id
    }
    when (io.resp.valid) {
      out_id_free(io.resp.out_id) := Bool(true)
      in_id_free(io.resp.in_id) := Bool(true)
    }

    io.req.ready := out_id_free.reduce(_ || _) && in_id_free(io.req.in_id)
    io.req.out_id := next_out_id

    io.resp.in_id := id_mapping(io.resp.out_id)
    io.resp.matches := !out_id_free(io.resp.out_id)
  }
}

class NastiIOTileLinkIOConverterInfo(implicit p: Parameters) extends TLBundle()(p) {
  val addr_beat = UInt(width = tlBeatAddrBits)
  val subblock = Bool()
}

class NastiIOTileLinkIOConverter(implicit p: Parameters) extends TLModule()(p)
    with HasNastiParameters {
  val io = new Bundle {
    val tl = new ClientUncachedTileLinkIO().flip
    val nasti = new NastiIO
  }

  private def opSizeToXSize(ops: UInt) = MuxLookup(ops, UInt("b111"), Seq(
    MT_B  -> UInt(0),
    MT_BU -> UInt(0),
    MT_H  -> UInt(1),
    MT_HU -> UInt(1),
    MT_W  -> UInt(2),
    MT_WU -> UInt(2),
    MT_D  -> UInt(3),
    MT_Q  -> UInt(log2Up(tlDataBytes))))

  val dataBits = tlDataBits*tlDataBeats 
  require(tlDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree") // TODO: remove this restriction
  require(tlDataBeats < (1 << nastiXLenBits), "Can't have that many beats")

  val has_data = io.tl.acquire.bits.hasData()

  val is_subblock = io.tl.acquire.bits.isSubBlockType()
  val is_multibeat = io.tl.acquire.bits.hasMultibeatData()
  val (tl_cnt_out, tl_wrap_out) = Counter(
    io.tl.acquire.fire() && is_multibeat, tlDataBeats)

  val get_valid = io.tl.acquire.valid && !has_data
  val put_valid = io.tl.acquire.valid && has_data

  // Reorder queue saves extra information needed to send correct
  // grant back to TL client
  val roqIdBits = min(tlClientXactIdBits, nastiXIdBits)
  val roq = Module(new ReorderQueue(
    new NastiIOTileLinkIOConverterInfo, roqIdBits))

  val get_id_mapper = Module(new IdMapper(tlClientXactIdBits, nastiXIdBits))
  val put_id_mapper = Module(new IdMapper(tlClientXactIdBits, nastiXIdBits))

  val get_id_ready = get_id_mapper.io.req.ready
  val put_id_mask = is_subblock || io.tl.acquire.bits.addr_beat === UInt(0)
  val put_id_ready = put_id_mapper.io.req.ready || !put_id_mask

  // For Get/GetBlock, make sure Reorder queue can accept new entry
  val get_helper = DecoupledHelper(
    get_valid,
    roq.io.enq.ready,
    io.nasti.ar.ready,
    get_id_ready)

  val w_inflight = Reg(init = Bool(false))
  val w_id = Reg(init = UInt(0, nastiXIdBits))

  // For Put/PutBlock, make sure aw and w channel are both ready before
  // we send the first beat
  val aw_ready = w_inflight || io.nasti.aw.ready
  val put_helper = DecoupledHelper(
    put_valid,
    aw_ready,
    io.nasti.w.ready,
    put_id_ready)

  val (nasti_cnt_out, nasti_wrap_out) = Counter(
    io.nasti.r.fire() && !roq.io.deq.data.subblock, tlDataBeats)

  roq.io.enq.valid := get_helper.fire(roq.io.enq.ready)
  roq.io.enq.bits.tag := io.nasti.ar.bits.id
  roq.io.enq.bits.data.addr_beat := io.tl.acquire.bits.addr_beat
  roq.io.enq.bits.data.subblock := is_subblock
  roq.io.deq.valid := io.nasti.r.fire() && (nasti_wrap_out || roq.io.deq.data.subblock)
  roq.io.deq.tag := io.nasti.r.bits.id

  get_id_mapper.io.req.valid := get_helper.fire(get_id_ready)
  get_id_mapper.io.req.in_id := io.tl.acquire.bits.client_xact_id
  get_id_mapper.io.resp.valid := io.nasti.r.fire() && io.nasti.r.bits.last
  get_id_mapper.io.resp.out_id := io.nasti.r.bits.id

  put_id_mapper.io.req.valid := put_helper.fire(put_id_ready, put_id_mask)
  put_id_mapper.io.req.in_id := io.tl.acquire.bits.client_xact_id
  put_id_mapper.io.resp.valid := io.nasti.b.fire()
  put_id_mapper.io.resp.out_id := io.nasti.b.bits.id

  // Decompose outgoing TL Acquires into Nasti address and data channels
  io.nasti.ar.valid := get_helper.fire(io.nasti.ar.ready)
  io.nasti.ar.bits := NastiReadAddressChannel(
    id = get_id_mapper.io.req.out_id,
    addr = io.tl.acquire.bits.full_addr(),
    size = Mux(is_subblock,
      opSizeToXSize(io.tl.acquire.bits.op_size()),
      UInt(log2Ceil(tlDataBytes))),
    len = Mux(is_subblock, UInt(0), UInt(tlDataBeats - 1)))

  def mask_helper(all_inside_0: Seq[Bool], defsize: Int): (Seq[Bool], UInt, UInt) = {
    val len = all_inside_0.size
    if (len == 1) {
      (Seq(Bool(true)), UInt(0), UInt(defsize))
    } else {
      val sub_inside_0 = Seq.tabulate (len/2) { i => all_inside_0(2*i) && all_inside_0(2*i+1) }
      val (sub_outside_0, sub_offset, sub_size) = mask_helper(sub_inside_0, defsize+1)
      val all_outside_0 = Seq.tabulate (len) { i => sub_outside_0(i/2) && all_inside_0(i^1) }
      val odd_outside_0 = Seq.tabulate (len/2) { i => all_outside_0(2*i+1) }
      val odd_outside = odd_outside_0.reduce (_ || _)
      val all_outside = all_outside_0.reduce (_ || _)
      val offset = Cat(sub_offset, odd_outside)
      val size = Mux(all_outside, UInt(defsize), sub_size)
      (all_outside_0, offset, size)
    }
  }

  val all_inside_0 = (~io.tl.acquire.bits.wmask()).toBools
  val (_, put_offset, put_size) = mask_helper(all_inside_0, 0)

  io.nasti.aw.valid := put_helper.fire(aw_ready, !w_inflight)
  io.nasti.aw.bits := NastiWriteAddressChannel(
    id = put_id_mapper.io.req.out_id,
    addr = io.tl.acquire.bits.full_addr() |
           Mux(is_multibeat, UInt(0), put_offset),
    size = Mux(is_multibeat, UInt(log2Ceil(tlDataBytes)), put_size),
    len = Mux(is_multibeat, UInt(tlDataBeats - 1), UInt(0)))

  io.nasti.w.valid := put_helper.fire(io.nasti.w.ready)
  io.nasti.w.bits := NastiWriteDataChannel(
    id = w_id,
    data = io.tl.acquire.bits.data,
    strb = Some(io.tl.acquire.bits.wmask()),
    last = Mux(w_inflight,
      tl_cnt_out === UInt(tlDataBeats - 1), !is_multibeat))

  io.tl.acquire.ready := Mux(has_data,
    put_helper.fire(put_valid),
    get_helper.fire(get_valid))

  when (!w_inflight && io.tl.acquire.fire() && is_multibeat) {
    w_inflight := Bool(true)
    w_id := put_id_mapper.io.req.out_id
  }

  when (w_inflight) {
    when (tl_wrap_out) { w_inflight := Bool(false) }
  }

  // Aggregate incoming NASTI responses into TL Grants
  val (tl_cnt_in, tl_wrap_in) = Counter(
    io.tl.grant.fire() && io.tl.grant.bits.hasMultibeatData(), tlDataBeats)
  val gnt_arb = Module(new LockingArbiter(new GrantToDst, 2,
    tlDataBeats, Some((gnt: GrantToDst) => gnt.hasMultibeatData())))
  io.tl.grant <> gnt_arb.io.out

  gnt_arb.io.in(0).valid := io.nasti.r.valid
  io.nasti.r.ready := gnt_arb.io.in(0).ready
  gnt_arb.io.in(0).bits := Grant(
    is_builtin_type = Bool(true),
    g_type = Mux(roq.io.deq.data.subblock,
      Grant.getDataBeatType, Grant.getDataBlockType),
    client_xact_id = get_id_mapper.io.resp.in_id,
    manager_xact_id = UInt(0),
    addr_beat = Mux(roq.io.deq.data.subblock, roq.io.deq.data.addr_beat, tl_cnt_in),
    data = io.nasti.r.bits.data)

  assert(!roq.io.deq.valid || roq.io.deq.matches,
    "TL -> NASTI converter ReorderQueue: NASTI tag error")
  assert(!gnt_arb.io.in(0).valid || get_id_mapper.io.resp.matches,
    "TL -> NASTI ID Mapper: NASTI tag error")

  gnt_arb.io.in(1).valid := io.nasti.b.valid
  io.nasti.b.ready := gnt_arb.io.in(1).ready
  gnt_arb.io.in(1).bits := Grant(
    is_builtin_type = Bool(true),
    g_type = Grant.putAckType,
    client_xact_id = put_id_mapper.io.resp.in_id,
    manager_xact_id = UInt(0),
    addr_beat = UInt(0),
    data = Bits(0))
  assert(!gnt_arb.io.in(1).valid || put_id_mapper.io.resp.matches, "NASTI tag error")

  assert(!io.nasti.r.valid || io.nasti.r.bits.resp === UInt(0), "NASTI read error")
  assert(!io.nasti.b.valid || io.nasti.b.bits.resp === UInt(0), "NASTI write error")
}

class TileLinkIONastiIOConverter(implicit p: Parameters) extends TLModule()(p)
    with HasNastiParameters {
  val io = new Bundle {
    val nasti = (new NastiIO).flip
    val tl = new ClientUncachedTileLinkIO
  }

  val (s_idle :: s_put :: Nil) = Enum(Bits(), 2)
  val state = Reg(init = s_idle)

  private val blockOffset = tlByteAddrBits + tlBeatAddrBits

  val aw_req = Reg(new NastiWriteAddressChannel)
  val w_tl_id = Reg(io.tl.acquire.bits.client_xact_id)

  def is_singlebeat(chan: NastiAddressChannel): Bool =
    chan.len === UInt(0)

  def is_multibeat(chan: NastiAddressChannel): Bool =
    chan.len === UInt(tlDataBeats - 1) && chan.size === UInt(log2Up(tlDataBytes))

  def nasti_addr_block(chan: NastiAddressChannel): UInt =
    chan.addr(nastiXAddrBits - 1, blockOffset)

  def nasti_addr_beat(chan: NastiAddressChannel): UInt =
    chan.addr(blockOffset - 1, tlByteAddrBits)

  def nasti_addr_byte(chan: NastiAddressChannel): UInt =
    chan.addr(tlByteAddrBits - 1, 0)

  def nasti_operand_size(chan: NastiAddressChannel): UInt =
    MuxLookup(chan.size, MT_Q, Seq(
      UInt(0) -> MT_BU,
      UInt(1) -> MT_HU,
      UInt(2) -> MT_WU,
      UInt(3) -> MT_D))

  def size_mask(size: UInt): UInt =
    (UInt(1) << (UInt(1) << size)) - UInt(1)

  def nasti_wmask(aw: NastiWriteAddressChannel, w: NastiWriteDataChannel): UInt = {
    val base = w.strb & size_mask(aw.size)
    val addr_byte = nasti_addr_byte(aw)
    w.strb & (size_mask(aw.size) << addr_byte)
  }

  def tl_last(gnt: GrantMetadata): Bool =
    !gnt.hasMultibeatData() || gnt.addr_beat === UInt(tlDataBeats - 1)

  def tl_b_grant(gnt: GrantMetadata): Bool =
    gnt.g_type === Grant.putAckType

  assert(!io.nasti.ar.valid ||
    is_singlebeat(io.nasti.ar.bits) || is_multibeat(io.nasti.ar.bits),
    "NASTI read transaction cannot convert to TileLInk")

  assert(!io.nasti.aw.valid ||
    is_singlebeat(io.nasti.aw.bits) || is_multibeat(io.nasti.aw.bits),
    "NASTI write transaction cannot convert to TileLInk")

  val put_count = Reg(init = UInt(0, tlBeatAddrBits))
  val get_id_mapper = Module(new IdMapper(nastiXIdBits, tlClientXactIdBits, true))
  val put_id_mapper = Module(new IdMapper(nastiXIdBits, tlClientXactIdBits, true))

  when (io.nasti.aw.fire()) {
    aw_req := io.nasti.aw.bits
    w_tl_id := put_id_mapper.io.req.out_id
    state := s_put
  }

  when (io.nasti.w.fire()) {
    put_count := put_count + UInt(1)
    when (io.nasti.w.bits.last) {
      put_count := UInt(0)
      state := s_idle
    }
  }

  val get_acquire = Mux(is_multibeat(io.nasti.ar.bits),
    GetBlock(
      client_xact_id = get_id_mapper.io.req.out_id,
      addr_block = nasti_addr_block(io.nasti.ar.bits)),
    Get(
      client_xact_id = get_id_mapper.io.req.out_id,
      addr_block = nasti_addr_block(io.nasti.ar.bits),
      addr_beat = nasti_addr_beat(io.nasti.ar.bits),
      addr_byte = nasti_addr_byte(io.nasti.ar.bits),
      operand_size = nasti_operand_size(io.nasti.ar.bits),
      alloc = Bool(false)))

  val put_acquire = Mux(is_multibeat(aw_req),
    PutBlock(
      client_xact_id = w_tl_id,
      addr_block = nasti_addr_block(aw_req),
      addr_beat = put_count,
      data = io.nasti.w.bits.data,
      wmask = Some(io.nasti.w.bits.strb)),
    Put(
      client_xact_id = w_tl_id,
      addr_block = nasti_addr_block(aw_req),
      addr_beat = nasti_addr_beat(aw_req),
      data = io.nasti.w.bits.data,
      wmask = Some(nasti_wmask(aw_req, io.nasti.w.bits))))

  val get_helper = DecoupledHelper(
    io.nasti.ar.valid,
    get_id_mapper.io.req.ready,
    io.tl.acquire.ready)

  get_id_mapper.io.req.valid := get_helper.fire(
    get_id_mapper.io.req.ready, state === s_idle)
  get_id_mapper.io.req.in_id := io.nasti.ar.bits.id
  get_id_mapper.io.resp.out_id := io.tl.grant.bits.client_xact_id
  get_id_mapper.io.resp.valid := io.nasti.r.fire() && io.nasti.r.bits.last

  val aw_ok = (state === s_idle && !io.nasti.ar.valid)

  put_id_mapper.io.req.valid := aw_ok && io.nasti.aw.valid
  put_id_mapper.io.req.in_id := io.nasti.aw.bits.id
  put_id_mapper.io.resp.out_id := io.tl.grant.bits.client_xact_id
  put_id_mapper.io.resp.valid := io.nasti.b.fire()

  io.tl.acquire.bits := Mux(state === s_put, put_acquire, get_acquire)
  io.tl.acquire.valid := get_helper.fire(io.tl.acquire.ready, state === s_idle) ||
                         (state === s_put && io.nasti.w.valid)

  io.nasti.ar.ready := get_helper.fire(io.nasti.ar.valid, state === s_idle)
  io.nasti.aw.ready := aw_ok && put_id_mapper.io.req.ready
  io.nasti.w.ready  := (state === s_put && io.tl.acquire.ready)

  val nXacts = tlMaxClientXacts * tlMaxClientsPerPort

  io.nasti.b.valid := io.tl.grant.valid && tl_b_grant(io.tl.grant.bits)
  io.nasti.b.bits := NastiWriteResponseChannel(
    id = put_id_mapper.io.resp.in_id)

  assert(!io.nasti.b.valid || put_id_mapper.io.resp.matches,
    "Put ID does not match")

  io.nasti.r.valid := io.tl.grant.valid && !tl_b_grant(io.tl.grant.bits)
  io.nasti.r.bits := NastiReadDataChannel(
    id = get_id_mapper.io.resp.in_id,
    data = io.tl.grant.bits.data,
    last = tl_last(io.tl.grant.bits))

  assert(!io.nasti.r.valid || get_id_mapper.io.resp.matches,
    "Get ID does not match")

  io.tl.grant.ready := Mux(tl_b_grant(io.tl.grant.bits),
    io.nasti.b.ready, io.nasti.r.ready)
}
