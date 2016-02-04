// See LICENSE for license details.

package junctions
import Chisel._
import scala.math._
import cde.{Parameters, Field}

case object MIFAddrBits extends Field[Int]
case object MIFDataBits extends Field[Int]
case object MIFTagBits extends Field[Int]
case object MIFDataBeats extends Field[Int]

trait HasMIFParameters {
  implicit val p: Parameters
  val mifTagBits = p(MIFTagBits)
  val mifAddrBits = p(MIFAddrBits)
  val mifDataBits = p(MIFDataBits)
  val mifDataBeats = p(MIFDataBeats)
}
 
abstract class MIFModule(implicit val p: Parameters) extends Module with HasMIFParameters
abstract class MIFBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasMIFParameters

trait HasMemData extends HasMIFParameters {
  val data = Bits(width = mifDataBits)
}

trait HasMemAddr extends HasMIFParameters {
  val addr = UInt(width = mifAddrBits)
}

trait HasMemTag extends HasMIFParameters {
  val tag = UInt(width = mifTagBits)
}

class MemReqCmd(implicit p: Parameters) extends MIFBundle()(p) with HasMemAddr with HasMemTag {
  val rw = Bool()
}

class MemTag(implicit p: Parameters) extends MIFBundle()(p) with HasMemTag
class MemData(implicit p: Parameters) extends MIFBundle()(p) with HasMemData
class MemResp(implicit p: Parameters) extends MIFBundle()(p) with HasMemData with HasMemTag

class MemIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemData)
  val resp     = Decoupled(new MemResp).flip
}

class MemPipeIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemData)
  val resp     = Valid(new MemResp).flip
}

class MemSerializedIO(w: Int)(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req = Decoupled(Bits(width = w))
  val resp = Valid(Bits(width = w)).flip
  override def cloneType = new MemSerializedIO(w)(p).asInstanceOf[this.type]
}

class MemSerdes(w: Int)(implicit p: Parameters) extends MIFModule
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

class MemDesserIO(w: Int)(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val narrow = new MemSerializedIO(w).flip
  val wide = new MemIO
}

class MemDesser(w: Int)(implicit p: Parameters) extends Module // test rig side
{
  val io = new MemDesserIO(w)
  val abits = io.wide.req_cmd.bits.toBits.getWidth
  val dbits = io.wide.req_data.bits.toBits.getWidth
  val rbits = io.wide.resp.bits.getWidth
  val mifDataBeats = p(MIFDataBeats)

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

class MemIOArbiter(val arbN: Int)(implicit p: Parameters) extends MIFModule {
  val io = new Bundle {
    val inner = Vec(arbN, new MemIO).flip
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
  def apply(in: MemPipeIO)(implicit p: Parameters): MemIO = {
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

class MemPipeIOMemIOConverter(numRequests: Int)(implicit p: Parameters) extends MIFModule {
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
