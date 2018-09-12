package freechips.rocketchip.DRAMModel
import Chisel._
import scala.math._

case class AddressSpaceConfiguration(paddrBits: Int, vaddrBits: Int, pgIdxBits: Int, asidBits: Int, permBits:Int) {
  val ppnBits = paddrBits - pgIdxBits
  val vpnBits = vaddrBits - pgIdxBits
}


abstract trait MemoryIFSubBundle extends Bundle {
  implicit val conf: MemoryIFConfiguration
  override def clone = this.getClass.getConstructors.head.newInstance(conf).asInstanceOf[this.type]
}

class MemReqCmd(implicit val conf: MemoryParameters) extends Bundle {
  val rw = Bool()
  val tag = UInt(width = conf.memIF.tagBits)
  val addr = UInt(width = conf.memIF.addrBits)
}

class MemResp(implicit val conf: MemoryParameters) extends Bundle{
  val data = Bits(width = conf.memIF.dataBits)
  val tag = UInt(width = conf.memIF.tagBits)
}

class MemData(implicit val conf: MemoryParameters) extends Bundle{
  val data = Bits(width = conf.memIF.dataBits)
}

class MemIO(implicit val conf: MemoryParameters) extends Bundle {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemData)
  val resp     = Decoupled(new MemResp).flip
}

class MemPipeIO(implicit val conf: MemoryParameters) extends Bundle {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemData)
  val resp     = Valid(new MemResp).flip
}

class MemSerializedIO(w: Int)(implicit val conf: MemoryParameters) extends Bundle
{
  val req = Decoupled(Bits(width = w))
  val resp = Valid(Bits(width = w)).flip
}

class MemSerdes(w: Int)(implicit val conf: MemoryParameters) extends Module
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
  val data_send_cnt = Reg(init=UInt(0, log2Up(conf.memIF.dataBeats)))
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
    state := Mux(data_send_cnt === UInt(conf.memIF.dataBeats-1), s_idle, s_write_idle)
    send_cnt := UInt(0)
  }

  val recv_cnt = Reg(init=UInt(0, log2Up((rbits+w-1)/w)))
  val data_recv_cnt = Reg(init=UInt(0, log2Up(conf.memIF.dataBeats)))
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

class MemDesserIO(w: Int)(implicit val conf: MemoryParameters) extends Bundle {
  val narrow = new MemSerializedIO(w).flip
  val wide = new MemIO
}

class MemDesser(w: Int)(implicit val conf: MemoryParameters) extends Module // test rig side
{
  val io = new MemDesserIO(w)
  val abits = io.wide.req_cmd.bits.toBits.getWidth
  val dbits = io.wide.req_data.bits.toBits.getWidth
  val rbits = io.wide.resp.bits.getWidth

  require(dbits >= abits && rbits >= dbits)
  val recv_cnt = Reg(init=UInt(0, log2Up((rbits+w-1)/w)))
  val data_recv_cnt = Reg(init=UInt(0, log2Up(conf.memIF.dataBeats)))
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
    when (data_recv_cnt === UInt(conf.memIF.dataBeats-1)) {
      state := s_cmd_recv
    }
    data_recv_cnt := data_recv_cnt + UInt(1)
  }
  when (rdone) { // state === s_reply
    when (data_recv_cnt === UInt(conf.memIF.dataBeats-1)) {
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

  val dataq = Module(new Queue(new MemResp, conf.memIF.dataBeats))
  dataq.io.enq <> io.wide.resp
  dataq.io.deq.ready := recv_cnt === UInt((rbits-1)/w)

  io.narrow.resp.valid := dataq.io.deq.valid
  io.narrow.resp.bits := dataq.io.deq.bits.toBits >> (recv_cnt * UInt(w))
}

/*
//Adapter betweewn an UncachedTileLinkIO and a mem controller MemIO
class MemIOUncachedTileLinkIOConverter(qDepth: Int)(implicit tlconf: TileLinkConfiguration, mifconf: MemoryIFConfiguration) extends Module {
  val io = new Bundle {
    val uncached = new UncachedTileLinkIO().flip
    val mem = new MemIO
  }

  require(tlconf.dataBits == mifconf.dataBits*mifconf.dataBeats)
  //require(tlconf.clientXactIdBits <= mifconf.tagBits)

  val mem_cmd_q = Module(new Queue(new MemReqCmd, qDepth))
  val mem_data_q = Module(new Queue(new MemData, qDepth))
  val cnt_max = mifconf.dataBeats

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
  val tag_in = Reg(UInt(width = mifconf.tagBits))
  
  // Decompose outgoing TL Acquires into MemIO cmd and data
  when(!active_out && io.uncached.acquire.valid) {
    active_out := Bool(true)
    cmd_sent_out := Bool(false)
    cnt_out := UInt(0)
    buf_out := io.uncached.acquire.bits.payload.data
    tag_out := io.uncached.acquire.bits.payload.client_xact_id
    addr_out := io.uncached.acquire.bits.payload.addr
    has_data := tlconf.co.needsOuterWrite(io.uncached.acquire.bits.payload.a_type, UInt(0))
  }
  when(active_out) {
    when(mem_cmd_q.io.enq.fire()) {
      cmd_sent_out := Bool(true)
    }
    when(mem_data_q.io.enq.fire()) {
      cnt_out := cnt_out + UInt(1)
      buf_out := buf_out >> UInt(mifconf.dataBits) 
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
    buf_in := io.mem.resp.bits.data << UInt(mifconf.dataBits*(cnt_max-1))
    tag_in := io.mem.resp.bits.tag
  }
  when(active_in) {
    when(io.uncached.grant.fire()) {
      active_in := Bool(false)
    }
    when(io.mem.resp.fire()) {
      buf_in := Cat(io.mem.resp.bits.data, buf_in(cnt_max*mifconf.dataBits-1,mifconf.dataBits))
      cnt_in := cnt_in + UInt(1)
    }
  }
}
*/
