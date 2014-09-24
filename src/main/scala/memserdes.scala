// See LICENSE for license details.

package uncore
import Chisel._
import scala.math._

case object PAddrBits extends Field[Int]
case object VAddrBits extends Field[Int]
case object PgIdxBits extends Field[Int]
case object ASIdBits extends Field[Int]
case object PermBits extends Field[Int]
case object PPNBits extends Field[Int]
case object VPNBits extends Field[Int]

case object MIFAddrBits extends Field[Int]
case object MIFDataBits extends Field[Int]
case object MIFTagBits extends Field[Int]
case object MIFDataBeats extends Field[Int]

trait HasMemData extends Bundle {
  val data = Bits(width = params(MIFDataBits))
}

trait HasMemAddr extends Bundle {
  val addr = UInt(width = params(MIFAddrBits))
}

trait HasMemTag extends Bundle {
  val tag = UInt(width = params(MIFTagBits))
}

class MemReqCmd extends HasMemAddr with HasMemTag {
  val rw = Bool()
}

class MemResp extends HasMemData with HasMemTag

class MemData extends HasMemData

class MemIO extends Bundle {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemData)
  val resp     = Decoupled(new MemResp).flip
}

class MemPipeIO extends Bundle {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemData)
  val resp     = Valid(new MemResp).flip
}

class MemSerializedIO(w: Int) extends Bundle
{
  val req = Decoupled(Bits(width = w))
  val resp = Valid(Bits(width = w)).flip
}

class MemSerdes(w: Int) extends Module
{
  val io = new Bundle {
    val wide = new MemIO().flip
    val narrow = new MemSerializedIO(w)
  }
  val abits = io.wide.req_cmd.bits.toBits.getWidth
  val dbits = io.wide.req_data.bits.toBits.getWidth
  val rbits = io.wide.resp.bits.getWidth
  val dbeats = params(MIFDataBeats)

  val out_buf = Reg(Bits())
  val in_buf = Reg(Bits())

  val s_idle :: s_read_addr :: s_write_addr :: s_write_idle :: s_write_data :: Nil = Enum(UInt(), 5)
  val state = Reg(init=s_idle)
  val send_cnt = Reg(init=UInt(0, log2Up((max(abits, dbits)+w-1)/w)))
  val data_send_cnt = Reg(init=UInt(0, log2Up(dbeats)))
  val adone = io.narrow.req.ready && send_cnt === UInt((abits-1)/w)
  val ddone = io.narrow.req.ready && send_cnt === UInt((dbits-1)/w)

  when (io.narrow.req.valid && io.narrow.req.ready) {
    send_cnt := send_cnt + UInt(1)
    out_buf := out_buf >> UInt(w)
  }
  when (io.wide.req_cmd.valid && io.wide.req_cmd.ready) {
    out_buf := io.wide.req_cmd.bits.toBits
  }
  when (io.wide.req_data.valid && io.wide.req_data.ready) {
    out_buf := io.wide.req_data.bits.toBits
  }

  io.wide.req_cmd.ready := state === s_idle
  io.wide.req_data.ready := state === s_write_idle
  io.narrow.req.valid := state === s_read_addr || state === s_write_addr || state === s_write_data
  io.narrow.req.bits := out_buf

  when (state === s_idle && io.wide.req_cmd.valid) {
    state := Mux(io.wide.req_cmd.bits.rw, s_write_addr, s_read_addr)
  }
  when (state === s_read_addr && adone) {
    state := s_idle
    send_cnt := UInt(0)
  }
  when (state === s_write_addr && adone) {
    state := s_write_idle
    send_cnt := UInt(0)
  }
  when (state === s_write_idle && io.wide.req_data.valid) {
    state := s_write_data
  }
  when (state === s_write_data && ddone) {
    data_send_cnt := data_send_cnt + UInt(1)
    state := Mux(data_send_cnt === UInt(dbeats-1), s_idle, s_write_idle)
    send_cnt := UInt(0)
  }

  val recv_cnt = Reg(init=UInt(0, log2Up((rbits+w-1)/w)))
  val data_recv_cnt = Reg(init=UInt(0, log2Up(dbeats)))
  val resp_val = Reg(init=Bool(false))

  resp_val := Bool(false)
  when (io.narrow.resp.valid) {
    recv_cnt := recv_cnt + UInt(1)
    when (recv_cnt === UInt((rbits-1)/w)) {
      recv_cnt := UInt(0)
      data_recv_cnt := data_recv_cnt + UInt(1)
      resp_val := Bool(true)
    }
    in_buf := Cat(io.narrow.resp.bits, in_buf((rbits+w-1)/w*w-1,w))
  }

  io.wide.resp.valid := resp_val
  io.wide.resp.bits := io.wide.resp.bits.fromBits(in_buf)
}

class MemDesserIO(w: Int) extends Bundle {
  val narrow = new MemSerializedIO(w).flip
  val wide = new MemIO
}

class MemDesser(w: Int) extends Module // test rig side
{
  val io = new MemDesserIO(w)
  val abits = io.wide.req_cmd.bits.toBits.getWidth
  val dbits = io.wide.req_data.bits.toBits.getWidth
  val rbits = io.wide.resp.bits.getWidth
  val dbeats = params(MIFDataBeats)

  require(dbits >= abits && rbits >= dbits)
  val recv_cnt = Reg(init=UInt(0, log2Up((rbits+w-1)/w)))
  val data_recv_cnt = Reg(init=UInt(0, log2Up(dbeats)))
  val adone = io.narrow.req.valid && recv_cnt === UInt((abits-1)/w)
  val ddone = io.narrow.req.valid && recv_cnt === UInt((dbits-1)/w)
  val rdone = io.narrow.resp.valid && recv_cnt === UInt((rbits-1)/w)

  val s_cmd_recv :: s_cmd :: s_data_recv :: s_data :: s_reply :: Nil = Enum(UInt(), 5)
  val state = Reg(init=s_cmd_recv)

  val in_buf = Reg(Bits())
  when (io.narrow.req.valid && io.narrow.req.ready || io.narrow.resp.valid) {
    recv_cnt := recv_cnt + UInt(1)
    in_buf := Cat(io.narrow.req.bits, in_buf((rbits+w-1)/w*w-1,w))
  }
  io.narrow.req.ready := state === s_cmd_recv || state === s_data_recv

  when (state === s_cmd_recv && adone) {
    state := s_cmd
    recv_cnt := UInt(0)
  }
  when (state === s_cmd && io.wide.req_cmd.ready) {
    state := Mux(io.wide.req_cmd.bits.rw, s_data_recv, s_reply)
  }
  when (state === s_data_recv && ddone) {
    state := s_data
    recv_cnt := UInt(0)
  }
  when (state === s_data && io.wide.req_data.ready) {
    state := s_data_recv
    when (data_recv_cnt === UInt(dbeats-1)) {
      state := s_cmd_recv
    }
    data_recv_cnt := data_recv_cnt + UInt(1)
  }
  when (rdone) { // state === s_reply
    when (data_recv_cnt === UInt(dbeats-1)) {
      state := s_cmd_recv
    }
    recv_cnt := UInt(0)
    data_recv_cnt := data_recv_cnt + UInt(1)
  }

  val req_cmd = in_buf >> UInt(((rbits+w-1)/w - (abits+w-1)/w)*w)
  io.wide.req_cmd.valid := state === s_cmd
  io.wide.req_cmd.bits := io.wide.req_cmd.bits.fromBits(req_cmd)

  io.wide.req_data.valid := state === s_data
  io.wide.req_data.bits.data := in_buf >> UInt(((rbits+w-1)/w - (dbits+w-1)/w)*w)

  val dataq = Module(new Queue(new MemResp, dbeats))
  dataq.io.enq <> io.wide.resp
  dataq.io.deq.ready := recv_cnt === UInt((rbits-1)/w)

  io.narrow.resp.valid := dataq.io.deq.valid
  io.narrow.resp.bits := dataq.io.deq.bits.toBits >> (recv_cnt * UInt(w))
}

//Adapter betweewn an UncachedTileLinkIO and a mem controller MemIO
class MemIOUncachedTileLinkIOConverter(qDepth: Int) extends Module {
  val io = new Bundle {
    val uncached = new UncachedTileLinkIO().flip
    val mem = new MemIO
  }
  val co = params(TLCoherence)
  val tbits = params(MIFTagBits)
  val dbits = params(MIFDataBits)
  val dbeats = params(MIFDataBeats)
  require(params(TLDataBits) == dbits*dbeats)
  //require(params(TLClientXactIdBits) <= params(MIFTagBits))

  val mem_cmd_q = Module(new Queue(new MemReqCmd, qDepth))
  val mem_data_q = Module(new Queue(new MemData, qDepth))
  val cnt_max = dbeats

  val cnt_out = Reg(UInt(width = log2Up(cnt_max+1)))
  val active_out = Reg(init=Bool(false))
  val cmd_sent_out = Reg(init=Bool(false))
  val buf_out = Reg(Bits())
  val tag_out = Reg(Bits())
  val addr_out = Reg(Bits())
  val has_data = Reg(init=Bool(false))

  val cnt_in = Reg(UInt(width = log2Up(cnt_max+1)))
  val active_in = Reg(init=Bool(false))
  val buf_in = Reg(Bits())
  val tag_in = Reg(UInt(width = tbits))
  
  // Decompose outgoing TL Acquires into MemIO cmd and data
  when(!active_out && io.uncached.acquire.valid) {
    active_out := Bool(true)
    cmd_sent_out := Bool(false)
    cnt_out := UInt(0)
    buf_out := io.uncached.acquire.bits.payload.data
    tag_out := io.uncached.acquire.bits.payload.client_xact_id
    addr_out := io.uncached.acquire.bits.payload.addr
    has_data := co.messageHasData(io.uncached.acquire.bits.payload)
  }
  when(active_out) {
    when(mem_cmd_q.io.enq.fire()) {
      cmd_sent_out := Bool(true)
    }
    when(mem_data_q.io.enq.fire()) {
      cnt_out := cnt_out + UInt(1)
      buf_out := buf_out >> UInt(dbits) 
    }
    when(cmd_sent_out && (!has_data || cnt_out === UInt(cnt_max))) {
      active_out := Bool(false)
    }
  }

  io.uncached.acquire.ready := !active_out
  mem_cmd_q.io.enq.valid := active_out && !cmd_sent_out
  mem_cmd_q.io.enq.bits.rw := has_data
  mem_cmd_q.io.enq.bits.tag := tag_out
  mem_cmd_q.io.enq.bits.addr := addr_out
  mem_data_q.io.enq.valid := active_out && has_data && cnt_out < UInt(cnt_max)
  mem_data_q.io.enq.bits.data := buf_out
  io.mem.req_cmd <> mem_cmd_q.io.deq
  io.mem.req_data <> mem_data_q.io.deq

  // Aggregate incoming MemIO responses into TL Grants
  io.mem.resp.ready := !active_in || cnt_in < UInt(cnt_max)
  io.uncached.grant.valid := active_in && (cnt_in === UInt(cnt_max))
  io.uncached.grant.bits.payload := Grant(UInt(0), tag_in, UInt(0), buf_in)
  when(!active_in && io.mem.resp.valid) {
    active_in := Bool(true)
    cnt_in := UInt(1)
    buf_in := io.mem.resp.bits.data << UInt(dbits*(cnt_max-1))
    tag_in := io.mem.resp.bits.tag
  }
  when(active_in) {
    when(io.uncached.grant.fire()) {
      active_in := Bool(false)
    }
    when(io.mem.resp.fire()) {
      buf_in := Cat(io.mem.resp.bits.data, buf_in(cnt_max*dbits-1,dbits))
      cnt_in := cnt_in + UInt(1)
    }
  }
}

class HellaFlowQueue[T <: Data](val entries: Int)(data: => T) extends Module
{
  val io = new QueueIO(data, entries)
  require(isPow2(entries) && entries > 1)

  val do_flow = Bool()
  val do_enq = io.enq.fire() && !do_flow
  val do_deq = io.deq.fire() && !do_flow

  val maybe_full = Reg(init=Bool(false))
  val enq_ptr = Counter(do_enq, entries)._1
  val deq_ptr = Counter(do_deq, entries)._1
  when (do_enq != do_deq) { maybe_full := do_enq }

  val ptr_match = enq_ptr === deq_ptr
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val atLeastTwo = full || enq_ptr - deq_ptr >= UInt(2)
  do_flow := empty && io.deq.ready

  val ram = Mem(data, entries, seqRead = true)
  val ram_addr = Reg(Bits())
  val ram_out_valid = Reg(Bool())
  ram_out_valid := Bool(false)
  when (do_enq) { ram(enq_ptr) := io.enq.bits }
  when (io.deq.ready && (atLeastTwo || !io.deq.valid && !empty)) {
    ram_out_valid := Bool(true)
    ram_addr := Mux(io.deq.valid, deq_ptr + UInt(1), deq_ptr)
  }

  io.deq.valid := Mux(empty, io.enq.valid, ram_out_valid)
  io.enq.ready := !full
  io.deq.bits := Mux(empty, io.enq.bits, ram(ram_addr))
}

class HellaQueue[T <: Data](val entries: Int)(data: => T) extends Module
{
  val io = new QueueIO(data, entries)

  val fq = Module(new HellaFlowQueue(entries)(data))
  io.enq <> fq.io.enq
  io.deq <> Queue(fq.io.deq, 1, pipe = true)
}

object HellaQueue
{
  def apply[T <: Data](enq: DecoupledIO[T], entries: Int) = {
    val q = Module((new HellaQueue(entries)) { enq.bits.clone })
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    q.io.deq
  }
}

class MemPipeIOMemIOConverter(numRequests: Int, refillCycles: Int) extends Module {
  val io = new Bundle {
    val cpu = new MemIO().flip
    val mem = new MemPipeIO
  }

  val numEntries = numRequests * refillCycles
  val size = log2Down(numEntries) + 1

  val inc = Bool()
  val dec = Bool()
  val count = Reg(init=UInt(numEntries, size))
  val watermark = count >= UInt(refillCycles)

  when (inc && !dec) {
    count := count + UInt(1)
  }
  when (!inc && dec) {
    count := count - UInt(refillCycles)
  }
  when (inc && dec) {
    count := count - UInt(refillCycles-1)
  }

  val cmdq_mask = io.cpu.req_cmd.bits.rw || watermark

  io.mem.req_cmd.valid := io.cpu.req_cmd.valid && cmdq_mask
  io.cpu.req_cmd.ready := io.mem.req_cmd.ready && cmdq_mask
  io.mem.req_cmd.bits := io.cpu.req_cmd.bits

  io.mem.req_data <> io.cpu.req_data

  val resp_dataq = Module((new HellaQueue(numEntries)) { new MemResp })
  resp_dataq.io.enq <> io.mem.resp
  io.cpu.resp <> resp_dataq.io.deq

  inc := resp_dataq.io.deq.fire()
  dec := io.mem.req_cmd.fire() && !io.mem.req_cmd.bits.rw
}

class MemPipeIOUncachedTileLinkIOConverter(outstanding: Int, refillCycles: Int) extends Module {
  val io = new Bundle {
    val uncached = new UncachedTileLinkIO().flip
    val mem = new MemPipeIO
  }
  
  val a = Module(new MemIOUncachedTileLinkIOConverter(2))
  val b = Module(new MemPipeIOMemIOConverter(outstanding, refillCycles))
  a.io.uncached <> io.uncached
  b.io.cpu.req_cmd <> Queue(a.io.mem.req_cmd, 2)
  b.io.cpu.req_data <> Queue(a.io.mem.req_data, refillCycles)
  a.io.mem.resp <> b.io.cpu.resp
  b.io.mem <> io.mem
}
