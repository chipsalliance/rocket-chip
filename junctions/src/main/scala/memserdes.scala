// See LICENSE for license details.

package junctions
import Chisel._
import scala.math._

case object PAddrBits extends Field[Int]
case object VAddrBits extends Field[Int]
case object PgIdxBits extends Field[Int]
case object PgLevels extends Field[Int]
case object PgLevelBits extends Field[Int]
case object ASIdBits extends Field[Int]
case object PPNBits extends Field[Int]
case object VPNBits extends Field[Int]

case object MIFAddrBits extends Field[Int]
case object MIFDataBits extends Field[Int]
case object MIFTagBits extends Field[Int]
case object MIFDataBeats extends Field[Int]

trait MIFParameters extends UsesParameters {
  val mifTagBits = params(MIFTagBits)
  val mifAddrBits = params(MIFAddrBits)
  val mifDataBits = params(MIFDataBits)
  val mifDataBeats = params(MIFDataBeats)
}
 
abstract class MIFBundle extends Bundle with MIFParameters
abstract class MIFModule extends Module with MIFParameters

trait HasMemData extends MIFBundle {
  val data = Bits(width = mifDataBits)
}

trait HasMemAddr extends MIFBundle {
  val addr = UInt(width = mifAddrBits)
}

trait HasMemTag extends MIFBundle {
  val tag = UInt(width = mifTagBits)
}

class MemReqCmd extends HasMemAddr with HasMemTag {
  val rw = Bool()
}

class MemTag extends HasMemTag
class MemData extends HasMemData
class MemResp extends HasMemData with HasMemTag

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

class MemSerializedIO(w: Int) extends Bundle {
  val req = Decoupled(Bits(width = w))
  val resp = Valid(Bits(width = w)).flip
}

class MemSerdes(w: Int) extends MIFModule
{
  val io = new Bundle {
    val wide = new MemIO().flip
    val narrow = new MemSerializedIO(w)
  }
  val abits = io.wide.req_cmd.bits.toBits.getWidth
  val dbits = io.wide.req_data.bits.toBits.getWidth
  val rbits = io.wide.resp.bits.getWidth

  val out_buf = Reg(Bits())
  val in_buf = Reg(Bits())

  val s_idle :: s_read_addr :: s_write_addr :: s_write_idle :: s_write_data :: Nil = Enum(UInt(), 5)
  val state = Reg(init=s_idle)
  val send_cnt = Reg(init=UInt(0, log2Up((max(abits, dbits)+w-1)/w)))
  val data_send_cnt = Reg(init=UInt(0, log2Up(mifDataBeats)))
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
    state := Mux(data_send_cnt === UInt(mifDataBeats-1), s_idle, s_write_idle)
    send_cnt := UInt(0)
  }

  val recv_cnt = Reg(init=UInt(0, log2Up((rbits+w-1)/w)))
  val data_recv_cnt = Reg(init=UInt(0, log2Up(mifDataBeats)))
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
  val mifDataBeats = params(MIFDataBeats)

  require(dbits >= abits && rbits >= dbits)
  val recv_cnt = Reg(init=UInt(0, log2Up((rbits+w-1)/w)))
  val data_recv_cnt = Reg(init=UInt(0, log2Up(mifDataBeats)))
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
    when (data_recv_cnt === UInt(mifDataBeats-1)) {
      state := s_cmd_recv
    }
    data_recv_cnt := data_recv_cnt + UInt(1)
  }
  when (rdone) { // state === s_reply
    when (data_recv_cnt === UInt(mifDataBeats-1)) {
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

  val dataq = Module(new Queue(new MemResp, mifDataBeats))
  dataq.io.enq <> io.wide.resp
  dataq.io.deq.ready := recv_cnt === UInt((rbits-1)/w)

  io.narrow.resp.valid := dataq.io.deq.valid
  io.narrow.resp.bits := dataq.io.deq.bits.toBits >> (recv_cnt * UInt(w))
}

class HellaFlowQueue[T <: Data](val entries: Int)(data: => T) extends Module
{
  val io = new QueueIO(data, entries)
  require(entries > 1)

  val do_flow = Wire(Bool())
  val do_enq = io.enq.fire() && !do_flow
  val do_deq = io.deq.fire() && !do_flow

  val maybe_full = Reg(init=Bool(false))
  val enq_ptr = Counter(do_enq, entries)._1
  val (deq_ptr, deq_done) = Counter(do_deq, entries)
  when (do_enq != do_deq) { maybe_full := do_enq }

  val ptr_match = enq_ptr === deq_ptr
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val atLeastTwo = full || enq_ptr - deq_ptr >= UInt(2)
  do_flow := empty && io.deq.ready

  val ram = SeqMem(data, entries)
  when (do_enq) { ram.write(enq_ptr, io.enq.bits) }

  val ren = io.deq.ready && (atLeastTwo || !io.deq.valid && !empty)
  val raddr = Mux(io.deq.valid, Mux(deq_done, UInt(0), deq_ptr + UInt(1)), deq_ptr)
  val ram_out_valid = Reg(next = ren)

  io.deq.valid := Mux(empty, io.enq.valid, ram_out_valid)
  io.enq.ready := !full
  io.deq.bits := Mux(empty, io.enq.bits, ram.read(raddr, ren))
}

class HellaQueue[T <: Data](val entries: Int)(data: => T) extends Module
{
  val io = new QueueIO(data, entries)

  val fq = Module(new HellaFlowQueue(entries)(data))
  fq.io.enq <> io.enq
  io.deq <> Queue(fq.io.deq, 1, pipe = true)
}

object HellaQueue
{
  def apply[T <: Data](enq: DecoupledIO[T], entries: Int) = {
    val q = Module((new HellaQueue(entries)) { enq.bits })
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    q.io.deq
  }
}

class MemIOArbiter(val arbN: Int) extends MIFModule {
  val io = new Bundle {
    val inner = Vec.fill(arbN){new MemIO}.flip
    val outer = new MemIO
  }

  if(arbN > 1) {
    val cmd_arb = Module(new RRArbiter(new MemReqCmd, arbN))
    val choice_q = Module(new Queue(cmd_arb.io.chosen, 4))
    val (data_cnt, data_done) = Counter(io.outer.req_data.fire(), mifDataBeats)

    io.inner.map(_.req_cmd).zipWithIndex.zip(cmd_arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.tag := Cat(req.bits.tag, UInt(id))
      req.ready := arb.ready
    }}
    io.outer.req_cmd.bits := cmd_arb.io.out.bits
    io.outer.req_cmd.valid := cmd_arb.io.out.valid && choice_q.io.enq.ready
    cmd_arb.io.out.ready := io.outer.req_cmd.ready && choice_q.io.enq.ready
    choice_q.io.enq.bits := cmd_arb.io.chosen
    choice_q.io.enq.valid := cmd_arb.io.out.fire() && cmd_arb.io.out.bits.rw

    io.outer.req_data.bits := io.inner(choice_q.io.deq.bits).req_data.bits
    io.outer.req_data.valid := io.inner(choice_q.io.deq.bits).req_data.valid && choice_q.io.deq.valid
    io.inner.map(_.req_data.ready).zipWithIndex.foreach {
      case(r, i) => r := UInt(i) === choice_q.io.deq.bits && choice_q.io.deq.valid
    }
    choice_q.io.deq.ready := data_done

    io.outer.resp.ready := Bool(false)
    for (i <- 0 until arbN) {
      io.inner(i).resp.valid := Bool(false)
      when(io.outer.resp.bits.tag(log2Up(arbN)-1,0).toUInt === UInt(i)) {
        io.inner(i).resp.valid := io.outer.resp.valid
        io.outer.resp.ready := io.inner(i).resp.ready
      }
      io.inner(i).resp.bits := io.outer.resp.bits
      io.inner(i).resp.bits.tag := io.outer.resp.bits.tag >> UInt(log2Up(arbN))
    }
  } else { io.outer <> io.inner.head }
}

object MemIOMemPipeIOConverter {
  def apply(in: MemPipeIO): MemIO = {
    val out = Wire(new MemIO())
    in.resp.valid := out.resp.valid
    in.resp.bits := out.resp.bits
    out.resp.ready := Bool(true)
    out.req_cmd.valid := in.req_cmd.valid
    out.req_cmd.bits := in.req_cmd.bits
    in.req_cmd.ready := out.req_cmd.ready
    out.req_data.valid := in.req_data.valid
    out.req_data.bits := in.req_data.bits
    in.req_data.ready := out.req_data.ready
    out
  }
}

class MemPipeIOMemIOConverter(numRequests: Int) extends MIFModule {
  val io = new Bundle {
    val cpu = new MemIO().flip
    val mem = new MemPipeIO
  }

  val numEntries = numRequests * mifDataBeats
  val size = log2Down(numEntries) + 1

  val inc = Wire(Bool())
  val dec = Wire(Bool())
  val count = Reg(init=UInt(numEntries, size))
  val watermark = count >= UInt(mifDataBeats)

  when (inc && !dec) {
    count := count + UInt(1)
  }
  when (!inc && dec) {
    count := count - UInt(mifDataBeats)
  }
  when (inc && dec) {
    count := count - UInt(mifDataBeats-1)
  }

  val cmdq_mask = io.cpu.req_cmd.bits.rw || watermark

  io.mem.req_cmd.valid := io.cpu.req_cmd.valid && cmdq_mask
  io.cpu.req_cmd.ready := io.mem.req_cmd.ready && cmdq_mask
  io.mem.req_cmd.bits := io.cpu.req_cmd.bits

  io.mem.req_data <> io.cpu.req_data

  // Have separate queues to allow for different mem implementations
  val resp_data_q = Module((new HellaQueue(numEntries)) { new MemData })
  resp_data_q.io.enq.valid := io.mem.resp.valid
  resp_data_q.io.enq.bits.data := io.mem.resp.bits.data

  val resp_tag_q = Module((new HellaQueue(numEntries)) { new MemTag })
  resp_tag_q.io.enq.valid := io.mem.resp.valid
  resp_tag_q.io.enq.bits.tag := io.mem.resp.bits.tag

  io.cpu.resp.valid := resp_data_q.io.deq.valid && resp_tag_q.io.deq.valid
  io.cpu.resp.bits.data := resp_data_q.io.deq.bits.data
  io.cpu.resp.bits.tag := resp_tag_q.io.deq.bits.tag
  resp_data_q.io.deq.ready := io.cpu.resp.ready
  resp_tag_q.io.deq.ready := io.cpu.resp.ready

  inc := resp_data_q.io.deq.fire() && resp_tag_q.io.deq.fire()
  dec := io.mem.req_cmd.fire() && !io.mem.req_cmd.bits.rw
}
