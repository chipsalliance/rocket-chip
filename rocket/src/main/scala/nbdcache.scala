package rocket

import Chisel._
import uncore._
import Util._

case class DCacheConfig(sets: Int, ways: Int,
                        nmshr: Int, nrpq: Int, nsdq: Int, ntlb: Int,
                        tl: TileLinkConfiguration,
                        as: AddressSpaceConfiguration,
                        reqtagbits: Int, databits: Int,
                        rowwords: Int = 2,
                        code: Code = new IdentityCode,
                        narrowRead: Boolean = true)
{
  def states = tl.co.nClientStates
  def lines = sets*ways
  def dm = ways == 1
  def offbits = log2Up(tl.dataBits/8)
  def ppnbits = as.ppnBits
  def vpnbits = as.vpnBits
  def pgidxbits = as.pgIdxBits
  def maxaddrbits = ppnbits.max(vpnbits+1) + pgidxbits
  def paddrbits = as.paddrBits
  def lineaddrbits = paddrbits - offbits
  def idxbits = log2Up(sets)
  def waybits = log2Up(ways)
  def untagbits = offbits + idxbits
  def tagbits = lineaddrbits - idxbits
  def databytes = databits/8
  def wordoffbits = log2Up(databytes)
  def rowbits = rowwords*databits
  def rowbytes = rowwords*databytes
  def rowoffbits = log2Up(rowbytes)
  def refillcycles = tl.dataBits/(rowbits)
  def isNarrowRead = narrowRead && databits*ways % rowbits == 0
  val statebits = log2Up(states)
  val metabits = statebits + tagbits
  val encdatabits = code.width(databits)
  val encmetabits = code.width(metabits)
  val encrowbits = rowwords*encdatabits
  val lrsc_cycles = 32 // ISA requires 16-insn LRSC sequences to succeed

  require(states > 0)
  require(isPow2(sets))
  require(isPow2(ways)) // TODO: relax this
  require(rowbits <= tl.dataBits)
}

abstract trait DCacheBundle extends Bundle {
  implicit val conf: DCacheConfig
  override def clone = this.getClass.getConstructors.head.newInstance(conf).asInstanceOf[this.type]
}

abstract class ReplacementPolicy
{
  def way: UInt
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(implicit conf: DCacheConfig) extends ReplacementPolicy
{
  private val replace = Bool()
  replace := Bool(false)
  val lfsr = LFSR16(replace)

  def way = if (conf.dm) UInt(0) else lfsr(conf.waybits-1,0)
  def miss = replace := Bool(true)
  def hit = {}
}

class StoreGen(typ: Bits, addr: Bits, dat: Bits)
{
  val byte = typ === MT_B || typ === MT_BU
  val half = typ === MT_H || typ === MT_HU
  val word = typ === MT_W || typ === MT_WU
  def mask =
    Mux(byte, Bits(  1) <<     addr(2,0),
    Mux(half, Bits(  3) << Cat(addr(2,1), Bits(0,1)),
    Mux(word, Bits( 15) << Cat(addr(2),   Bits(0,2)),
              Bits(255))))
  def data =
    Mux(byte, Fill(8, dat( 7,0)),
    Mux(half, Fill(4, dat(15,0)),
                      wordData))
  lazy val wordData =
    Mux(word, Fill(2, dat(31,0)),
                      dat)
}

class LoadGen(typ: Bits, addr: Bits, dat: Bits, zero: Bool)
{
  val t = new StoreGen(typ, addr, dat)
  val sign = typ === MT_B || typ === MT_H || typ === MT_W || typ === MT_D

  val wordShift = Mux(addr(2), dat(63,32), dat(31,0))
  val word = Cat(Mux(t.word, Fill(32, sign && wordShift(31)), dat(63,32)), wordShift)
  val halfShift = Mux(addr(1), word(31,16), word(15,0))
  val half = Cat(Mux(t.half, Fill(48, sign && halfShift(15)), word(63,16)), halfShift)
  val byteShift = Mux(zero, UInt(0), Mux(addr(0), half(15,8), half(7,0)))
  val byte = Cat(Mux(zero || t.byte, Fill(56, sign && byteShift(7)), half(63,8)), byteShift)
}

class MSHRReq(implicit conf: DCacheConfig) extends HellaCacheReq {
  val tag_match = Bool()
  val old_meta = new MetaData
  val way_en = Bits(width = conf.ways)
}

class Replay(implicit conf: DCacheConfig) extends HellaCacheReq {
  val sdq_id = UInt(width = log2Up(conf.nsdq))
}

class DataReadReq(implicit val conf: DCacheConfig) extends DCacheBundle {
  val way_en = Bits(width = conf.ways)
  val addr   = Bits(width = conf.untagbits)
}

class DataWriteReq(implicit val conf: DCacheConfig) extends DCacheBundle {
  val way_en = Bits(width = conf.ways)
  val addr   = Bits(width = conf.untagbits)
  val wmask  = Bits(width = conf.rowwords)
  val data   = Bits(width = conf.encrowbits)
}

class InternalProbe(implicit conf: DCacheConfig) extends Probe()(conf.tl) {
  val client_xact_id = Bits(width = conf.tl.clientXactIdBits)

  override def clone = new InternalProbe().asInstanceOf[this.type]
}

class WritebackReq(implicit conf: DCacheConfig) extends Bundle {
  val tag = Bits(width = conf.tagbits)
  val idx = Bits(width = conf.idxbits)
  val way_en = Bits(width = conf.ways)
  val client_xact_id = Bits(width = conf.tl.clientXactIdBits)
  val master_xact_id = Bits(width = conf.tl.masterXactIdBits)
  val r_type = UInt(width = conf.tl.co.releaseTypeWidth) 

  override def clone = new WritebackReq().asInstanceOf[this.type]
}

object MetaData {
  def apply(tag: Bits, state: UInt)(implicit conf: DCacheConfig) = {
    val meta = new MetaData
    meta.state := state
    meta.tag := tag
    meta
  }
}
class MetaData(implicit val conf: DCacheConfig) extends DCacheBundle {
  val state = UInt(width = conf.statebits)
  val tag = Bits(width = conf.tagbits)
}

class MetaReadReq(implicit val conf: DCacheConfig) extends DCacheBundle {
  val addr  = UInt(width = conf.paddrbits)
}

class MetaWriteReq(implicit val conf: DCacheConfig) extends DCacheBundle {
  val way_en = Bits(width = conf.ways)
  val idx  = Bits(width = conf.idxbits)
  val data = new MetaData()
}

class MSHR(id: Int)(implicit conf: DCacheConfig) extends Module {
  implicit val (tl, ln) = (conf.tl, conf.tl.ln)
  val io = new Bundle {
    val req_pri_val    = Bool(INPUT)
    val req_pri_rdy    = Bool(OUTPUT)
    val req_sec_val    = Bool(INPUT)
    val req_sec_rdy    = Bool(OUTPUT)
    val req_bits       = new MSHRReq().asInput
    val req_sdq_id     = UInt(INPUT, log2Up(conf.nsdq))

    val idx_match       = Bool(OUTPUT)
    val tag             = Bits(OUTPUT, conf.tagbits)

    val mem_req  = Decoupled(new Acquire)
    val mem_resp = new DataWriteReq().asOutput
    val meta_read = Decoupled(new MetaReadReq)
    val meta_write = Decoupled(new MetaWriteReq)
    val replay = Decoupled(new Replay)
    val mem_grant = Valid(new LogicalNetworkIO(new Grant)).flip
    val mem_finish = Decoupled(new LogicalNetworkIO(new GrantAck))
    val wb_req = Decoupled(new WritebackReq)
    val probe_rdy = Bool(OUTPUT)
  }

  val s_invalid :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_refill_req :: s_refill_resp :: s_meta_write_req :: s_meta_write_resp :: s_drain_rpq :: Nil = Enum(UInt(), 9)
  val state = Reg(init=s_invalid)

  val acquire_type = Reg(UInt())
  val release_type = Reg(UInt())
  val line_state = Reg(UInt())
  val refill_count = Reg(UInt(width = log2Up(conf.refillcycles))) // TODO: zero-width wire
  val req = Reg(new MSHRReq())

  val req_cmd = io.req_bits.cmd
  val req_idx = req.addr(conf.untagbits-1,conf.offbits)
  val idx_match = req_idx === io.req_bits.addr(conf.untagbits-1,conf.offbits)
  val sec_rdy = idx_match && (state === s_wb_req || state === s_wb_resp || state === s_meta_clear || (state === s_refill_req || state === s_refill_resp) && !tl.co.needsTransactionOnSecondaryMiss(req_cmd, io.mem_req.bits))

  require(isPow2(conf.refillcycles))
  val reply = io.mem_grant.valid && io.mem_grant.bits.payload.client_xact_id === UInt(id)
  val refill_done = reply && (if(conf.refillcycles > 1) refill_count.andR else Bool(true))
  val wb_done = reply && (state === s_wb_resp)

  val rpq = Module(new Queue(new Replay, conf.nrpq))
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && !isPrefetch(req_cmd)
  rpq.io.enq.bits := io.req_bits
  rpq.io.enq.bits.sdq_id := io.req_sdq_id
  rpq.io.deq.ready := io.replay.ready && state === s_drain_rpq || state === s_invalid

  when (state === s_drain_rpq && !rpq.io.deq.valid) {
    state := s_invalid
  }
  when (state === s_meta_write_resp) {
    // this wait state allows us to catch RAW hazards on the tags via nack_victim
    state := s_drain_rpq
  }
  when (state === s_meta_write_req && io.meta_write.ready) {
    state := s_meta_write_resp
  }
  when (state === s_refill_resp) {
    when (refill_done) { state := s_meta_write_req }
    when (reply) {
      if(conf.refillcycles > 1) refill_count := refill_count + UInt(1)
      line_state := tl.co.newStateOnGrant(io.mem_grant.bits.payload, io.mem_req.bits)
    }
  }
  when (io.mem_req.fire()) { // s_refill_req
    state := s_refill_resp
  }
  when (state === s_meta_clear && io.meta_write.ready) {
    state := s_refill_req
  }
  when (state === s_wb_resp && reply) {
    state := s_meta_clear
  }
  when (io.wb_req.fire()) { // s_wb_req
    state := s_wb_resp 
  }

  when (io.req_sec_val && io.req_sec_rdy) { // s_wb_req, s_wb_resp, s_refill_req
    acquire_type := tl.co.getAcquireTypeOnSecondaryMiss(req_cmd, tl.co.newStateOnFlush(), io.mem_req.bits)
  }
  when (io.req_pri_val && io.req_pri_rdy) {
    line_state := tl.co.newStateOnFlush()
    refill_count := UInt(0)
    acquire_type := tl.co.getAcquireTypeOnPrimaryMiss(req_cmd, tl.co.newStateOnFlush())
    release_type := tl.co.getReleaseTypeOnVoluntaryWriteback() //TODO downgrades etc 
    req := io.req_bits

    when (io.req_bits.tag_match) {
      when (tl.co.isHit(req_cmd, io.req_bits.old_meta.state)) { // set dirty bit
        state := s_meta_write_req
        line_state := tl.co.newStateOnHit(req_cmd, io.req_bits.old_meta.state)
      }.otherwise { // upgrade permissions
        state := s_refill_req
      }
    }.otherwise { // writback if necessary and refill
      state := Mux(tl.co.needsWriteback(io.req_bits.old_meta.state), s_wb_req, s_meta_clear)
    }
  }

  val ackq = Module(new Queue(new LogicalNetworkIO(new GrantAck), 1))
  ackq.io.enq.valid := (wb_done || refill_done) && tl.co.requiresAckForGrant(io.mem_grant.bits.payload.g_type)
  ackq.io.enq.bits.payload.master_xact_id := io.mem_grant.bits.payload.master_xact_id
  ackq.io.enq.bits.header.dst := io.mem_grant.bits.header.src
  val can_finish = state === s_invalid || state === s_refill_req || state === s_refill_resp
  io.mem_finish.valid := ackq.io.deq.valid && can_finish
  ackq.io.deq.ready := io.mem_finish.ready && can_finish
  io.mem_finish.bits := ackq.io.deq.bits

  io.idx_match := (state != s_invalid) && idx_match
  io.mem_resp := req
  io.mem_resp.addr := (if(conf.refillcycles > 1) Cat(req_idx, refill_count) else req_idx) << conf.rowoffbits
  io.tag := req.addr >> conf.untagbits
  io.req_pri_rdy := state === s_invalid
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  val meta_hazard = Reg(init=UInt(0,2))
  when (meta_hazard != UInt(0)) { meta_hazard := meta_hazard + 1 }
  when (io.meta_write.fire()) { meta_hazard := 1 }
  io.probe_rdy := !idx_match || (state != s_wb_req && state != s_wb_resp && state != s_meta_clear && meta_hazard === 0)

  io.meta_write.valid := state === s_meta_write_req || state === s_meta_clear
  io.meta_write.bits.idx := req_idx
  io.meta_write.bits.data.state := Mux(state === s_meta_clear, tl.co.newStateOnFlush(), line_state)
  io.meta_write.bits.data.tag := io.tag
  io.meta_write.bits.way_en := req.way_en

  io.wb_req.valid := state === s_wb_req && ackq.io.enq.ready
  io.wb_req.bits.tag := req.old_meta.tag
  io.wb_req.bits.idx := req_idx
  io.wb_req.bits.way_en := req.way_en
  io.wb_req.bits.client_xact_id := Bits(id)
  io.wb_req.bits.master_xact_id := Bits(0) // DNC
  io.wb_req.bits.r_type := tl.co.getReleaseTypeOnVoluntaryWriteback()

  io.mem_req.valid := state === s_refill_req && ackq.io.enq.ready
  io.mem_req.bits.a_type := acquire_type
  io.mem_req.bits.addr := Cat(io.tag, req_idx).toUInt
  io.mem_req.bits.client_xact_id := Bits(id)
  io.mem_finish <> ackq.io.deq

  io.meta_read.valid := state === s_drain_rpq
  io.meta_read.bits.addr := io.mem_req.bits.addr << conf.offbits

  io.replay.valid := state === s_drain_rpq && rpq.io.deq.valid
  io.replay.bits := rpq.io.deq.bits
  io.replay.bits.phys := Bool(true)
  io.replay.bits.addr := Cat(io.tag, req_idx, rpq.io.deq.bits.addr(conf.offbits-1,0)).toUInt

  when (!io.meta_read.ready) {
    rpq.io.deq.ready := Bool(false)
    io.replay.bits.cmd := M_NOP
  }
}

class MSHRFile(implicit conf: DCacheConfig) extends Module {
  implicit val (tl, ln) = (conf.tl, conf.tl.ln)
  val io = new Bundle {
    val req = Decoupled(new MSHRReq).flip
    val secondary_miss = Bool(OUTPUT)

    val mem_req  = Decoupled(new Acquire)
    val mem_resp = new DataWriteReq().asOutput
    val meta_read = Decoupled(new MetaReadReq)
    val meta_write = Decoupled(new MetaWriteReq)
    val replay = Decoupled(new Replay)
    val mem_grant = Valid(new LogicalNetworkIO(new Grant)).flip
    val mem_finish = Decoupled(new LogicalNetworkIO(new GrantAck))
    val wb_req = Decoupled(new WritebackReq)

    val probe_rdy = Bool(OUTPUT)
    val fence_rdy = Bool(OUTPUT)
  }

  val sdq_val = Reg(init=Bits(0, conf.nsdq))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(conf.nsdq-1,0))
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = io.req.valid && io.req.ready && isWrite(io.req.bits.cmd)
  val sdq = Mem(io.req.bits.data, conf.nsdq)
  when (sdq_enq) { sdq(sdq_alloc_id) := io.req.bits.data }

  val idxMatch = Vec.fill(conf.nmshr){Bool()}
  val tagList = Vec.fill(conf.nmshr){Bits()}
  val tag_match = Mux1H(idxMatch, tagList) === io.req.bits.addr >> conf.untagbits

  val wbTagList = Vec.fill(conf.nmshr){Bits()}
  val memRespMux = Vec.fill(conf.nmshr){new DataWriteReq}
  val meta_read_arb = Module(new Arbiter(new MetaReadReq, conf.nmshr))
  val meta_write_arb = Module(new Arbiter(new MetaWriteReq, conf.nmshr))
  val mem_req_arb = Module(new Arbiter(new Acquire, conf.nmshr))
  val mem_finish_arb = Module(new Arbiter(new LogicalNetworkIO(new GrantAck), conf.nmshr))
  val wb_req_arb = Module(new Arbiter(new WritebackReq, conf.nmshr))
  val replay_arb = Module(new Arbiter(new Replay, conf.nmshr))
  val alloc_arb = Module(new Arbiter(Bool(), conf.nmshr))

  var idx_match = Bool(false)
  var pri_rdy = Bool(false)
  var sec_rdy = Bool(false)

  io.fence_rdy := true
  io.probe_rdy := true

  for (i <- 0 to conf.nmshr-1) {
    val mshr = Module(new MSHR(i))

    idxMatch(i) := mshr.io.idx_match
    tagList(i) := mshr.io.tag
    wbTagList(i) := mshr.io.wb_req.bits.tag

    alloc_arb.io.in(i).valid := mshr.io.req_pri_rdy
    mshr.io.req_pri_val := alloc_arb.io.in(i).ready

    mshr.io.req_sec_val := io.req.valid && sdq_rdy && tag_match
    mshr.io.req_bits := io.req.bits
    mshr.io.req_sdq_id := sdq_alloc_id

    mshr.io.meta_read <> meta_read_arb.io.in(i)
    mshr.io.meta_write <> meta_write_arb.io.in(i)
    mshr.io.mem_req <> mem_req_arb.io.in(i)
    mshr.io.mem_finish <> mem_finish_arb.io.in(i)
    mshr.io.wb_req <> wb_req_arb.io.in(i)
    mshr.io.replay <> replay_arb.io.in(i)

    mshr.io.mem_grant <> io.mem_grant
    memRespMux(i) := mshr.io.mem_resp

    pri_rdy = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy = sec_rdy || mshr.io.req_sec_rdy
    idx_match = idx_match || mshr.io.idx_match

    when (!mshr.io.req_pri_rdy) { io.fence_rdy := false }
    when (!mshr.io.probe_rdy) { io.probe_rdy := false }
  }

  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && !idx_match

  meta_read_arb.io.out <> io.meta_read
  meta_write_arb.io.out <> io.meta_write
  mem_req_arb.io.out <> io.mem_req
  mem_finish_arb.io.out <> io.mem_finish
  wb_req_arb.io.out <> io.wb_req

  io.req.ready := Mux(idx_match, tag_match && sec_rdy, pri_rdy) && sdq_rdy
  io.secondary_miss := idx_match
  io.mem_resp := memRespMux(io.mem_grant.bits.payload.client_xact_id)

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.cmd)
  io.replay.bits.data := sdq(RegEnable(replay_arb.io.out.bits.sdq_id, free_sdq))
  io.replay <> replay_arb.io.out

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(io.replay.bits.sdq_id) & Fill(conf.nsdq, free_sdq)) | 
               PriorityEncoderOH(~sdq_val(conf.nsdq-1,0)) & Fill(conf.nsdq, sdq_enq)
  }
}


class WritebackUnit(implicit conf: DCacheConfig) extends Module {
  implicit val tl = conf.tl
  val io = new Bundle {
    val req = Decoupled(new WritebackReq()).flip
    val meta_read = Decoupled(new MetaReadReq)
    val data_req = Decoupled(new DataReadReq())
    val data_resp = Bits(INPUT, conf.encrowbits)
    val release = Decoupled(new Release)
  }

  val active = Reg(init=Bool(false))
  val r1_data_req_fired = Reg(init=Bool(false))
  val r2_data_req_fired = Reg(init=Bool(false))
  val cnt = Reg(init = UInt(0, width = log2Up(conf.refillcycles+1)))
  val req = Reg(new WritebackReq)

  io.release.valid := false
  when (active) {
    r1_data_req_fired := false
    r2_data_req_fired := r1_data_req_fired
    when (io.data_req.fire() && io.meta_read.fire()) {
      r1_data_req_fired := true
      cnt := cnt + 1
    }
    if(conf.refillcycles > 1) { // Coalescing buffer inserted
      when (!r1_data_req_fired && !r2_data_req_fired && cnt === conf.refillcycles) {
        io.release.valid := true
        active := !io.release.ready
      }
    } else {                    // No buffer, data released a cycle earlier
      when (r2_data_req_fired) {
        io.release.valid := true
        when(!io.release.ready) {
          r1_data_req_fired := false
          r2_data_req_fired := false
          cnt := UInt(0)
        } .otherwise {
          active := false
        }
      }
    }
  }
  when (io.req.fire()) {
    active := true
    cnt := 0
    req := io.req.bits
  }

  val fire = active && cnt < UInt(conf.refillcycles)
  io.req.ready := !active

  // We reissue the meta read as it sets up the muxing for s2_data_muxed
  io.meta_read.valid := fire
  io.meta_read.bits.addr := io.release.bits.addr << conf.offbits

  io.data_req.valid := fire
  io.data_req.bits.way_en := req.way_en
  if(conf.refillcycles > 1) {
    io.data_req.bits.addr := Cat(req.idx, cnt(log2Up(conf.refillcycles)-1,0)) << conf.rowoffbits
  } else {
    io.data_req.bits.addr := req.idx << conf.rowoffbits
  }

  io.release.bits.r_type := req.r_type
  io.release.bits.addr := Cat(req.tag, req.idx).toUInt
  io.release.bits.client_xact_id := req.client_xact_id
  io.release.bits.master_xact_id := req.master_xact_id
  if(conf.refillcycles > 1) {
    val data_buf = Reg(Bits())
    when(active && r2_data_req_fired) {
      data_buf := Cat(io.data_resp, data_buf(conf.refillcycles*conf.encrowbits-1, conf.encrowbits))
    }
    io.release.bits.data := data_buf
  } else {
    io.release.bits.data := io.data_resp
  }

}

class ProbeUnit(implicit conf: DCacheConfig) extends Module {
  implicit val tl = conf.tl
  val io = new Bundle {
    val req = Decoupled(new InternalProbe).flip
    val rep = Decoupled(new Release)
    val meta_read = Decoupled(new MetaReadReq)
    val meta_write = Decoupled(new MetaWriteReq)
    val wb_req = Decoupled(new WritebackReq)
    val way_en = Bits(INPUT, conf.ways)
    val mshr_rdy = Bool(INPUT)
    val line_state = UInt(INPUT, 2)
  }

  val s_reset :: s_invalid :: s_meta_read :: s_meta_resp :: s_mshr_req :: s_release :: s_writeback_req :: s_writeback_resp :: s_meta_write :: Nil = Enum(UInt(), 9)
  val state = Reg(init=s_invalid)
  val line_state = Reg(UInt())
  val way_en = Reg(Bits())
  val req = Reg(new InternalProbe)
  val hit = way_en.orR

  when (state === s_meta_write && io.meta_write.ready) {
    state := s_invalid
  }
  when (state === s_writeback_resp && io.wb_req.ready) {
    state := s_meta_write
  }
  when (state === s_writeback_req && io.wb_req.ready) {
    state := s_writeback_resp
  }
  when (state === s_release && io.rep.ready) {
    state := s_invalid
    when (hit) {
      state := Mux(tl.co.needsWriteback(line_state), s_writeback_req, s_meta_write)
    }
  }
  when (state === s_mshr_req) {
    state := s_release
    line_state := io.line_state
    way_en := io.way_en
    when (!io.mshr_rdy) { state := s_meta_read }
  }
  when (state === s_meta_resp) {
    state := s_mshr_req
  }
  when (state === s_meta_read && io.meta_read.ready) {
    state := s_meta_resp
  }
  when (state === s_invalid && io.req.valid) {
    state := s_meta_read
    req := io.req.bits
  }
  when (state === s_reset) {
    state := s_invalid
  }

  io.req.ready := state === s_invalid
  io.rep.valid := state === s_release && !(hit && tl.co.needsWriteback(line_state))
  io.rep.bits := Release(tl.co.getReleaseTypeOnProbe(req, Mux(hit, line_state, tl.co.newStateOnFlush)), req.addr, req.client_xact_id, req.master_xact_id)

  io.meta_read.valid := state === s_meta_read
  io.meta_read.bits.addr := req.addr << UInt(conf.offbits)

  io.meta_write.valid := state === s_meta_write
  io.meta_write.bits.way_en := way_en
  io.meta_write.bits.idx := req.addr
  io.meta_write.bits.data.state := tl.co.newStateOnProbe(req, line_state)
  io.meta_write.bits.data.tag := req.addr >> UInt(conf.idxbits)

  io.wb_req.valid := state === s_writeback_req
  io.wb_req.bits.way_en := way_en
  io.wb_req.bits.idx := req.addr
  io.wb_req.bits.tag := req.addr >> UInt(conf.idxbits)
  io.wb_req.bits.r_type := tl.co.getReleaseTypeOnProbe(req, Mux(hit, line_state, tl.co.newStateOnFlush))
  io.wb_req.bits.client_xact_id := req.client_xact_id
  io.wb_req.bits.master_xact_id := req.master_xact_id
}

class MetaDataArray(implicit conf: DCacheConfig) extends Module {
  implicit val tl = conf.tl
  val io = new Bundle {
    val read = Decoupled(new MetaReadReq).flip
    val write = Decoupled(new MetaWriteReq).flip
    val resp = Vec.fill(conf.ways){(new MetaData).asOutput}
  }

  val rst_cnt = Reg(init=UInt(0, log2Up(conf.sets+1)))
  val rst = rst_cnt < conf.sets
  when (rst) { rst_cnt := rst_cnt+1 }

  val metabits = io.write.bits.data.state.getWidth + conf.tagbits
  val tags = Mem(UInt(width = metabits*conf.ways), conf.sets, seqRead = true)

  when (rst || io.write.valid) {
    val addr = Mux(rst, rst_cnt, io.write.bits.idx)
    val data = Cat(Mux(rst, tl.co.newStateOnFlush, io.write.bits.data.state), io.write.bits.data.tag)
    val mask = Mux(rst, SInt(-1), io.write.bits.way_en)
    tags.write(addr, Fill(conf.ways, data), FillInterleaved(metabits, mask))
  }
  val tag = tags(RegEnable(io.read.bits.addr >> conf.offbits, io.read.valid))

  for (w <- 0 until conf.ways) {
    val m = tag(metabits*(w+1)-1, metabits*w)
    io.resp(w).state := m >> conf.tagbits
    io.resp(w).tag := m
  }

  io.read.ready := !rst && !io.write.valid // so really this could be a 6T RAM
  io.write.ready := !rst
}

class DataArray(implicit conf: DCacheConfig) extends Module {
  val io = new Bundle {
    val read = Decoupled(new DataReadReq).flip
    val write = Decoupled(new DataWriteReq).flip
    val resp = Vec.fill(conf.ways){Bits(OUTPUT, conf.encrowbits)}
  }

  val waddr = io.write.bits.addr >> conf.rowoffbits
  val raddr = io.read.bits.addr >> conf.rowoffbits

  if (conf.isNarrowRead) {
    for (w <- 0 until conf.ways by conf.rowwords) {
      val wway_en = io.write.bits.way_en(w+conf.rowwords-1,w)
      val rway_en = io.read.bits.way_en(w+conf.rowwords-1,w)
      val resp = Vec.fill(conf.rowwords){Bits(width = conf.encrowbits)}
      val r_raddr = RegEnable(io.read.bits.addr, io.read.valid)
      for (p <- 0 until resp.size) {
        val array = Mem(Bits(width=conf.encrowbits), conf.sets*conf.refillcycles, seqRead = true)
        when (wway_en.orR && io.write.valid && io.write.bits.wmask(p)) {
          val data = Fill(conf.rowwords, io.write.bits.data(conf.encdatabits*(p+1)-1,conf.encdatabits*p))
          val mask = FillInterleaved(conf.encdatabits, wway_en)
          array.write(waddr, data, mask)
        }
        resp(p) := array(RegEnable(raddr, rway_en.orR && io.read.valid))
      }
      for (dw <- 0 until conf.rowwords) {
        val r = AVec(resp.map(_(conf.encdatabits*(dw+1)-1,conf.encdatabits*dw)))
        val resp_mux =
          if (r.size == 1) r
          else AVec(r(r_raddr(conf.rowoffbits-1,conf.wordoffbits)), r.tail:_*)
        io.resp(w+dw) := resp_mux.toBits
      }
    }
  } else {
    val wmask = FillInterleaved(conf.encdatabits, io.write.bits.wmask)
    for (w <- 0 until conf.ways) {
      val array = Mem(Bits(width=conf.encrowbits), conf.sets*conf.refillcycles, seqRead = true)
      when (io.write.bits.way_en(w) && io.write.valid) {
        array.write(waddr, io.write.bits.data, wmask)
      }
      io.resp(w) := array(RegEnable(raddr, io.read.bits.way_en(w) && io.read.valid))
    }
  }

  io.read.ready := Bool(true)
  io.write.ready := Bool(true)
}

class AMOALU(implicit conf: DCacheConfig) extends Module {
  val io = new Bundle {
    val addr = Bits(INPUT, conf.offbits)
    val cmd = Bits(INPUT, 4)
    val typ = Bits(INPUT, 3)
    val lhs = Bits(INPUT, conf.databits)
    val rhs = Bits(INPUT, conf.databits)
    val out = Bits(OUTPUT, conf.databits)
  }

  require(conf.databits == 64)
  val storegen = new StoreGen(io.typ, io.addr, io.rhs)
  val rhs = storegen.wordData
  
  val sgned = io.cmd === M_XA_MIN || io.cmd === M_XA_MAX
  val max = io.cmd === M_XA_MAX || io.cmd === M_XA_MAXU
  val min = io.cmd === M_XA_MIN || io.cmd === M_XA_MINU
  val word = io.typ === MT_W || io.typ === MT_WU || io.typ === MT_B || io.typ === MT_BU

  val mask = SInt(-1,64) ^ (io.addr(2) << 31)
  val adder_out = (io.lhs & mask).toUInt + (rhs & mask)

  val cmp_lhs  = Mux(word && !io.addr(2), io.lhs(31), io.lhs(63))
  val cmp_rhs  = Mux(word && !io.addr(2), rhs(31), rhs(63))
  val lt_lo = io.lhs(31,0) < rhs(31,0)
  val lt_hi = io.lhs(63,32) < rhs(63,32)
  val eq_hi = io.lhs(63,32) === rhs(63,32)
  val lt = Mux(word, Mux(io.addr(2), lt_hi, lt_lo), lt_hi || eq_hi && lt_lo)
  val less = Mux(cmp_lhs === cmp_rhs, lt, Mux(sgned, cmp_lhs, cmp_rhs))

  val out = Mux(io.cmd === M_XA_ADD, adder_out,
            Mux(io.cmd === M_XA_AND, io.lhs & rhs,
            Mux(io.cmd === M_XA_OR,  io.lhs | rhs,
            Mux(io.cmd === M_XA_XOR, io.lhs ^ rhs,
            Mux(Mux(less, min, max), io.lhs,
            storegen.data)))))

  val wmask = FillInterleaved(8, storegen.mask)
  io.out := wmask & out | ~wmask & io.lhs
}

class HellaCacheReq(implicit val conf: DCacheConfig) extends DCacheBundle {
  val kill = Bool()
  val typ  = Bits(width = MT_SZ)
  val phys = Bool()
  val addr = UInt(width = conf.maxaddrbits)
  val data = Bits(width = conf.databits)
  val tag  = Bits(width = conf.reqtagbits)
  val cmd  = Bits(width = M_SZ)
}

class HellaCacheResp(implicit val conf: DCacheConfig) extends DCacheBundle {
  val nack = Bool() // comes 2 cycles after req.fire
  val replay = Bool()
  val typ = Bits(width = 3)
  val has_data = Bool()
  val data = Bits(width = conf.databits)
  val data_subword = Bits(width = conf.databits)
  val tag = Bits(width = conf.reqtagbits)
  val cmd  = Bits(width = 4)
  val addr = UInt(width = conf.maxaddrbits)
  val store_data = Bits(width = conf.databits)
}

class AlignmentExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class HellaCacheExceptions extends Bundle {
  val ma = new AlignmentExceptions
  val pf = new AlignmentExceptions
}

// interface between D$ and processor/DTLB
class HellaCacheIO(implicit conf: DCacheConfig) extends Bundle {
  val req = Decoupled(new HellaCacheReq)
  val resp = Valid(new HellaCacheResp).flip
  val replay_next = Valid(Bits(width = conf.reqtagbits)).flip
  val xcpt = (new HellaCacheExceptions).asInput
  val ptw = new TLBPTWIO()(conf.as).flip
  val ordered = Bool(INPUT)
}

class HellaCache(implicit conf: DCacheConfig) extends Module {
  implicit val (tl, ln) = (conf.tl, conf.tl.ln)
  val io = new Bundle {
    val cpu = (new HellaCacheIO).flip
    val mem = new TileLinkIO
  }
 
  val indexmsb    = conf.untagbits-1
  val indexlsb    = conf.offbits
  val offsetmsb   = indexlsb-1
  val offsetlsb   = log2Up(conf.databytes)

  val wb = Module(new WritebackUnit)
  val prober = Module(new ProbeUnit)
  val mshrs = Module(new MSHRFile)

  io.cpu.req.ready := Bool(true)
  val s1_valid = Reg(next=io.cpu.req.fire(), init=Bool(false))
  val s1_req = Reg(io.cpu.req.bits.clone)
  val s1_valid_masked = s1_valid && !io.cpu.req.bits.kill
  val s1_replay = Reg(init=Bool(false))
  val s1_clk_en = Reg(Bool())

  val s2_valid = Reg(next=s1_valid_masked, init=Bool(false))
  val s2_req = Reg(io.cpu.req.bits.clone)
  val s2_replay = Reg(next=s1_replay, init=Bool(false)) && s2_req.cmd != M_NOP
  val s2_recycle = Bool()
  val s2_valid_masked = Bool()

  val s3_valid = Reg(init=Bool(false))
  val s3_req = Reg(io.cpu.req.bits.clone)
  val s3_way = Reg(Bits())

  val s1_recycled = RegEnable(s2_recycle, s1_clk_en)
  val s1_read  = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_sc = s1_req.cmd === M_XSC
  val s1_readwrite = s1_read || s1_write || isPrefetch(s1_req.cmd)

  val dtlb = Module(new TLB(8)(conf.as))
  dtlb.io.ptw <> io.cpu.ptw
  dtlb.io.req.valid := s1_valid_masked && s1_readwrite && !s1_req.phys
  dtlb.io.req.bits.passthrough := s1_req.phys
  dtlb.io.req.bits.asid := UInt(0)
  dtlb.io.req.bits.vpn := s1_req.addr >> conf.pgidxbits
  dtlb.io.req.bits.instruction := Bool(false)
  when (!dtlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := Bool(false) }
  
  when (io.cpu.req.valid) {
    s1_req := io.cpu.req.bits
  }
  when (wb.io.meta_read.valid) {
    s1_req := wb.io.meta_read.bits
    s1_req.phys := Bool(true)
  }
  when (prober.io.meta_read.valid) {
    s1_req := prober.io.meta_read.bits
    s1_req.phys := Bool(true)
  }
  when (mshrs.io.replay.valid) {
    s1_req := mshrs.io.replay.bits
  }
  when (s2_recycle) {
    s1_req := s2_req
  }
  val s1_addr = Cat(dtlb.io.resp.ppn, s1_req.addr(conf.pgidxbits-1,0))

  when (s1_clk_en) {
    s2_req.kill := s1_req.kill
    s2_req.typ := s1_req.typ
    s2_req.phys := s1_req.phys
    s2_req.addr := s1_addr
    when (s1_write) {
      s2_req.data := Mux(s1_replay, mshrs.io.replay.bits.data, io.cpu.req.bits.data)
    }
    when (s1_recycled) { s2_req.data := s1_req.data }
    s2_req.tag := s1_req.tag
    s2_req.cmd := s1_req.cmd
  }

  val misaligned =
    (((s1_req.typ === MT_H) || (s1_req.typ === MT_HU)) && (s1_req.addr(0) != Bits(0))) ||
    (((s1_req.typ === MT_W) || (s1_req.typ === MT_WU)) && (s1_req.addr(1,0) != Bits(0))) ||
    ((s1_req.typ === MT_D) && (s1_req.addr(2,0) != Bits(0)))
    
  io.cpu.xcpt.ma.ld := s1_read && misaligned
  io.cpu.xcpt.ma.st := s1_write && misaligned
  io.cpu.xcpt.pf.ld := s1_read && dtlb.io.resp.xcpt_ld
  io.cpu.xcpt.pf.st := s1_write && dtlb.io.resp.xcpt_st

  // tags
  val meta = Module(new MetaDataArray)
  val metaReadArb = Module(new Arbiter(new MetaReadReq, 5))
  val metaWriteArb = Module(new Arbiter(new MetaWriteReq, 2))
  metaReadArb.io.out <> meta.io.read
  metaWriteArb.io.out <> meta.io.write

  // data
  val data = Module(new DataArray)
  val readArb = Module(new Arbiter(new DataReadReq, 4))

  val writeArb = Module(new Arbiter(new DataWriteReq, 2))
  data.io.write.valid := writeArb.io.out.valid
  writeArb.io.out.ready := data.io.write.ready
  data.io.write.bits := writeArb.io.out.bits
  val wdata_encoded = (0 until conf.rowwords).map(i => conf.code.encode(writeArb.io.out.bits.data(conf.databits*(i+1)-1,conf.databits*i)))
  data.io.write.bits.data := AVec(wdata_encoded).toBits

  // tag read for new requests
  metaReadArb.io.in(4).valid := io.cpu.req.valid
  metaReadArb.io.in(4).bits.addr := io.cpu.req.bits.addr
  when (!metaReadArb.io.in(4).ready) { io.cpu.req.ready := Bool(false) }

  // data read for new requests
  readArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  readArb.io.in(3).valid := io.cpu.req.valid
  readArb.io.in(3).bits.way_en := SInt(-1)
  when (!readArb.io.in(3).ready) { io.cpu.req.ready := Bool(false) }

  // recycled requests
  metaReadArb.io.in(0).valid := s2_recycle
  metaReadArb.io.in(0).bits.addr := s2_req.addr
  readArb.io.in(0).valid := s2_recycle
  readArb.io.in(0).bits.addr := s2_req.addr
  readArb.io.in(0).bits.way_en := SInt(-1)

  // tag check and way muxing
  def wayMap[T <: Data](f: Int => T) = Vec((0 until conf.ways).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> conf.untagbits)).toBits
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && tl.co.isValid(meta.io.resp(w).state)).toBits
  s1_clk_en := metaReadArb.io.out.valid
  val s1_writeback = s1_clk_en && !s1_valid && !s1_replay
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).state, s1_clk_en)))
  val s2_hit = s2_tag_match && tl.co.isHit(s2_req.cmd, s2_hit_state) && s2_hit_state === tl.co.newStateOnHit(s2_req.cmd, s2_hit_state)

  // load-reserved/store-conditional
  val lrsc_count = Reg(init=UInt(0))
  val lrsc_valid = lrsc_count.orR
  val lrsc_addr = Reg(UInt())
  val (s2_lr, s2_sc) = (s2_req.cmd === M_XLR, s2_req.cmd === M_XSC)
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req.addr >> conf.offbits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_valid) { lrsc_count := lrsc_count - 1 }
  when (s2_valid_masked && s2_hit || s2_replay) {
    when (s2_lr) {
      when (!lrsc_valid) { lrsc_count := conf.lrsc_cycles-1 }
      lrsc_addr := s2_req.addr >> conf.offbits
    }
    when (s2_sc) {
      lrsc_count := 0
    }
  }
  when (io.cpu.ptw.sret) { lrsc_count := 0 }

  val s2_data = Vec.fill(conf.ways){Bits(width = conf.encrowbits)}
  for (w <- 0 until conf.ways) {
    val regs = Vec.fill(conf.rowwords){Reg(Bits(width = conf.encdatabits))}
    val en1 = s1_clk_en && s1_tag_eq_way(w)
    for (i <- 0 until regs.size) {
      val en = en1 && (Bool(i == 0 || !conf.isNarrowRead) || s1_writeback)
      when (en) { regs(i) := data.io.resp(w) >> conf.encdatabits*i }
    }
    s2_data(w) := regs.toBits
  }
  val s2_data_muxed = Mux1H(s2_tag_match_way, s2_data)
  val s2_data_decoded = (0 until conf.rowwords).map(i => conf.code.decode(s2_data_muxed(conf.encdatabits*(i+1)-1,conf.encdatabits*i)))
  val s2_data_corrected = AVec(s2_data_decoded.map(_.corrected)).toBits
  val s2_data_uncorrected = AVec(s2_data_decoded.map(_.uncorrected)).toBits
  val s2_word_idx = if (conf.isNarrowRead) UInt(0) else s2_req.addr(log2Up(conf.rowwords*conf.databytes)-1,3)
  val s2_data_correctable = AVec(s2_data_decoded.map(_.correctable)).toBits()(s2_word_idx)
  
  // store/amo hits
  s3_valid := (s2_valid_masked && s2_hit || s2_replay) && !s2_sc_fail && isWrite(s2_req.cmd)
  val amoalu = Module(new AMOALU)
  when ((s2_valid || s2_replay) && (isWrite(s2_req.cmd) || s2_data_correctable)) {
    s3_req := s2_req
    s3_req.data := Mux(s2_data_correctable, s2_data_corrected, amoalu.io.out)
    s3_way := s2_tag_match_way
  }

  writeArb.io.in(0).bits.addr := s3_req.addr
  writeArb.io.in(0).bits.wmask := UInt(1) << (if(conf.rowoffbits > offsetlsb) 
                                                s3_req.addr(conf.rowoffbits-1,offsetlsb).toUInt
                                              else UInt(0))
  writeArb.io.in(0).bits.data := Fill(conf.rowwords, s3_req.data)
  writeArb.io.in(0).valid := s3_valid
  writeArb.io.in(0).bits.way_en :=  s3_way

  // replacement policy
  val replacer = new RandomReplacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegEnable(replacer.way, s1_clk_en))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegEnable(meta.io.resp(w), s1_clk_en && s1_replaced_way_en(w))).toSeq)

  // miss handling
  mshrs.io.req.valid := s2_valid_masked && !s2_hit && (isPrefetch(s2_req.cmd) || isRead(s2_req.cmd) || isWrite(s2_req.cmd))
  mshrs.io.req.bits := s2_req
  mshrs.io.req.bits.tag_match := s2_tag_match
  mshrs.io.req.bits.old_meta := Mux(s2_tag_match, MetaData(s2_repl_meta.tag, s2_hit_state), s2_repl_meta)
  mshrs.io.req.bits.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshrs.io.req.bits.data := s2_req.data
  when (mshrs.io.req.fire()) { replacer.miss }

  io.mem.acquire <> DecoupledLogicalNetworkIOWrapper(mshrs.io.mem_req)

  // replays
  readArb.io.in(1).valid := mshrs.io.replay.valid
  readArb.io.in(1).bits := mshrs.io.replay.bits
  readArb.io.in(1).bits.way_en := SInt(-1)
  mshrs.io.replay.ready := readArb.io.in(1).ready
  s1_replay := mshrs.io.replay.valid && readArb.io.in(1).ready
  metaReadArb.io.in(1) <> mshrs.io.meta_read
  metaWriteArb.io.in(0) <> mshrs.io.meta_write

  // probes
  val releaseArb = Module(new Arbiter(new Release, 2))
  DecoupledLogicalNetworkIOWrapper(releaseArb.io.out) <> io.mem.release

  val probe = DecoupledLogicalNetworkIOUnwrapper(io.mem.probe)
  prober.io.req.valid := probe.valid && !lrsc_valid
  probe.ready := prober.io.req.ready && !lrsc_valid
  prober.io.req.bits := probe.bits
  prober.io.rep <> releaseArb.io.in(1)
  prober.io.way_en := s2_tag_match_way
  prober.io.line_state := s2_hit_state
  prober.io.meta_read <> metaReadArb.io.in(2)
  prober.io.meta_write <> metaWriteArb.io.in(1)
  prober.io.mshr_rdy := mshrs.io.probe_rdy

  // refills
  def doRefill(g: Grant): Bool = tl.co.messageUpdatesDataArray(g)
  val refill = if(conf.refillcycles > 1) {
    val ser = Module(new FlowThroughSerializer(io.mem.grant.bits, conf.refillcycles, doRefill))
    ser.io.in <> io.mem.grant
    ser.io.out
  } else io.mem.grant
  mshrs.io.mem_grant.valid := refill.fire()
  mshrs.io.mem_grant.bits := refill.bits
  refill.ready := writeArb.io.in(1).ready || !doRefill(refill.bits.payload)
  writeArb.io.in(1).valid := refill.valid && doRefill(refill.bits.payload)
  writeArb.io.in(1).bits := mshrs.io.mem_resp
  writeArb.io.in(1).bits.wmask := SInt(-1)
  writeArb.io.in(1).bits.data := refill.bits.payload.data(conf.encrowbits-1,0)
  readArb.io.out.ready := !refill.valid || refill.ready // insert bubble if refill gets blocked
  readArb.io.out <> data.io.read

  // writebacks
  val wbArb = Module(new Arbiter(new WritebackReq, 2))
  prober.io.wb_req <> wbArb.io.in(0)
  mshrs.io.wb_req <> wbArb.io.in(1)
  wbArb.io.out <> wb.io.req
  wb.io.meta_read <> metaReadArb.io.in(3)
  wb.io.data_req <> readArb.io.in(2)
  wb.io.data_resp := s2_data_corrected
  releaseArb.io.in(0) <> wb.io.release

  // store->load bypassing
  val s4_valid = Reg(next=s3_valid, init=Bool(false))
  val s4_req = RegEnable(s3_req, s3_valid && metaReadArb.io.out.valid)
  val bypasses = List(
    ((s2_valid_masked || s2_replay) && !s2_sc_fail, s2_req, amoalu.io.out),
    (s3_valid, s3_req, s3_req.data),
    (s4_valid, s4_req, s4_req.data)
  ).map(r => (r._1 && (s1_addr >> conf.wordoffbits === r._2.addr >> conf.wordoffbits) && isWrite(r._2.cmd), r._3))
  val s2_store_bypass_data = Reg(Bits(width = conf.databits))
  val s2_store_bypass = Reg(Bool())
  when (s1_clk_en) {
    s2_store_bypass := false
    when (bypasses.map(_._1).reduce(_||_)) {
      s2_store_bypass_data := PriorityMux(bypasses)
      s2_store_bypass := true
    }
  }

  // load data subword mux/sign extension
  val s2_data_word_prebypass = s2_data_uncorrected >> Cat(s2_word_idx, Bits(0,log2Up(conf.databits)))
  val s2_data_word = Mux(s2_store_bypass, s2_store_bypass_data, s2_data_word_prebypass)
  val loadgen = new LoadGen(s2_req.typ, s2_req.addr, s2_data_word, s2_sc)

  amoalu.io := s2_req
  amoalu.io.lhs := s2_data_word
  amoalu.io.rhs := s2_req.data

  // nack it like it's hot
  val s1_nack = dtlb.io.req.valid && dtlb.io.resp.miss ||
                s1_req.addr(indexmsb,indexlsb) === prober.io.meta_write.bits.idx && !prober.io.req.ready
  val s2_nack_hit = RegEnable(s1_nack, s1_valid || s1_replay)
  when (s2_nack_hit) { mshrs.io.req.valid := Bool(false) }
  val s2_nack_victim = s2_hit && mshrs.io.secondary_miss
  val s2_nack_miss = !s2_hit && !mshrs.io.req.ready
  val s2_nack = s2_nack_hit || s2_nack_victim || s2_nack_miss
  s2_valid_masked := s2_valid && !s2_nack

  val s2_recycle_ecc = (s2_valid || s2_replay) && s2_hit && s2_data_correctable
  val s2_recycle_next = Reg(init=Bool(false))
  when (s1_valid || s1_replay) { s2_recycle_next := (s1_valid || s1_replay) && s2_recycle_ecc }
  s2_recycle := s2_recycle_ecc || s2_recycle_next

  // after a nack, block until nack condition resolves to save energy
  val block_miss = Reg(init=Bool(false))
  block_miss := (s2_valid || block_miss) && s2_nack_miss
  when (block_miss) {
    io.cpu.req.ready := Bool(false)
  }

  io.cpu.resp.valid  := (s2_replay || s2_valid_masked && s2_hit) && !s2_data_correctable
  io.cpu.resp.bits.nack := s2_valid && s2_nack
  io.cpu.resp.bits := s2_req
  io.cpu.resp.bits.has_data := isRead(s2_req.cmd) || s2_sc
  io.cpu.resp.bits.replay := s2_replay
  io.cpu.resp.bits.data := loadgen.word
  io.cpu.resp.bits.data_subword := loadgen.byte | s2_sc_fail
  io.cpu.resp.bits.store_data := s2_req.data
  io.cpu.ordered := mshrs.io.fence_rdy && !s1_valid && !s2_valid

  io.cpu.replay_next.valid := s1_replay && (s1_read || s1_sc)
  io.cpu.replay_next.bits := s1_req.tag

  io.mem.grant_ack <> mshrs.io.mem_finish
}

// exposes a sane decoupled request interface
class SimpleHellaCacheIF(implicit conf: DCacheConfig) extends Module
{
  val io = new Bundle {
    val requestor = new HellaCacheIO().flip
    val cache = new HellaCacheIO
  }

  val replaying_cmb = Bool()
  val replaying = Reg(next = replaying_cmb, init = Bool(false))
  replaying_cmb := replaying

  val replayq1 = Module(new Queue(new HellaCacheReq, 1, flow = true))
  val replayq2 = Module(new Queue(new HellaCacheReq, 1))
  val req_arb = Module(new Arbiter(new HellaCacheReq, 2))

  req_arb.io.in(0) <> replayq1.io.deq
  req_arb.io.in(1).valid := !replaying_cmb && io.requestor.req.valid
  req_arb.io.in(1).bits := io.requestor.req.bits
  io.requestor.req.ready := !replaying_cmb && req_arb.io.in(1).ready

  val s2_nack = io.cache.resp.bits.nack
  val s3_nack = Reg(next=s2_nack)

  val s0_req_fire = io.cache.req.fire()
  val s1_req_fire = Reg(next=s0_req_fire)
  val s2_req_fire = Reg(next=s1_req_fire)

  io.cache.req.bits.kill := s2_nack
  io.cache.req.bits.phys := Bool(true)
  io.cache.req.bits.data := RegEnable(req_arb.io.out.bits.data, s0_req_fire)
  io.cache.req <> req_arb.io.out

  // replay queues
  // replayq1 holds the older request
  // replayq2 holds the newer request (for the first nack)
  // we need to split the queues like this for the case where the older request
  // goes through but gets nacked, while the newer request stalls
  // if this happens, the newer request will go through before the older
  // request
  // we don't need to check replayq1.io.enq.ready and replayq2.io.enq.ready as
  // there will only be two requests going through at most

  // stash d$ request in stage 2 if nacked (older request)
  replayq1.io.enq.valid := Bool(false)
  replayq1.io.enq.bits.cmd := io.cache.resp.bits.cmd
  replayq1.io.enq.bits.typ := io.cache.resp.bits.typ
  replayq1.io.enq.bits.addr := io.cache.resp.bits.addr
  replayq1.io.enq.bits.data := io.cache.resp.bits.store_data
  replayq1.io.enq.bits.tag := io.cache.resp.bits.tag

  // stash d$ request in stage 1 if nacked (newer request)
  replayq2.io.enq.valid := s2_req_fire && s3_nack
  replayq2.io.enq.bits.data := io.cache.resp.bits.store_data
  replayq2.io.enq.bits <> io.cache.resp.bits
  replayq2.io.deq.ready := Bool(false)

  when (s2_nack) {
    replayq1.io.enq.valid := Bool(true)
    replaying_cmb := Bool(true)
  }

  // when replaying request got sunk into the d$
  when (s2_req_fire && Reg(next=Reg(next=replaying_cmb)) && !s2_nack) {
    // see if there's a stashed request in replayq2
    when (replayq2.io.deq.valid) {
      replayq1.io.enq.valid := Bool(true)
      replayq1.io.enq.bits.cmd := replayq2.io.deq.bits.cmd
      replayq1.io.enq.bits.typ := replayq2.io.deq.bits.typ
      replayq1.io.enq.bits.addr := replayq2.io.deq.bits.addr
      replayq1.io.enq.bits.data := replayq2.io.deq.bits.data
      replayq1.io.enq.bits.tag := replayq2.io.deq.bits.tag
      replayq2.io.deq.ready := Bool(true)
    } .otherwise {
      replaying_cmb := Bool(false)
    }
  }

  io.requestor.resp := io.cache.resp
}
