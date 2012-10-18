package uncore

import Chisel._
import Node._
import Constants._
import scala.math._
import uncore._

class ioMemSerialized(w: Int) extends Bundle
{
  val req = (new FIFOIO) { Bits(width = w) }
  val resp = (new PipeIO) { Bits(width = w) }.flip
}

class MemSerdes(w: Int) extends Component
{
  val io = new Bundle {
    val wide = new ioMem().flip
    val narrow = new ioMemSerialized(w)
  }
  val abits = io.wide.req_cmd.bits.toBits.getWidth
  val dbits = io.wide.req_data.bits.toBits.getWidth
  val rbits = io.wide.resp.bits.getWidth

  val out_buf = Reg() { Bits() }
  val in_buf = Reg() { Bits() }

  val s_idle :: s_read_addr :: s_write_addr :: s_write_idle :: s_write_data :: Nil = Enum(5) { UFix() }
  val state = Reg(resetVal = s_idle)
  val send_cnt = Reg(resetVal = UFix(0, log2Up((max(abits, dbits)+w-1)/w)))
  val data_send_cnt = Reg(resetVal = UFix(0, log2Up(REFILL_CYCLES)))
  val adone = io.narrow.req.ready && send_cnt === UFix((abits-1)/w)
  val ddone = io.narrow.req.ready && send_cnt === UFix((dbits-1)/w)

  when (io.narrow.req.valid && io.narrow.req.ready) {
    send_cnt := send_cnt + UFix(1)
    out_buf := out_buf >> UFix(w)
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
    send_cnt := UFix(0)
  }
  when (state === s_write_addr && adone) {
    state := s_write_idle
    send_cnt := UFix(0)
  }
  when (state === s_write_idle && io.wide.req_data.valid) {
    state := s_write_data
  }
  when (state === s_write_data && ddone) {
    data_send_cnt := data_send_cnt + UFix(1)
    state := Mux(data_send_cnt === UFix(REFILL_CYCLES-1), s_idle, s_write_idle)
    send_cnt := UFix(0)
  }

  val recv_cnt = Reg(resetVal = UFix(0, log2Up((rbits+w-1)/w)))
  val data_recv_cnt = Reg(resetVal = UFix(0, log2Up(REFILL_CYCLES)))
  val resp_val = Reg(resetVal = Bool(false))

  resp_val := Bool(false)
  when (io.narrow.resp.valid) {
    recv_cnt := recv_cnt + UFix(1)
    when (recv_cnt === UFix((rbits-1)/w)) {
      recv_cnt := UFix(0)
      data_recv_cnt := data_recv_cnt + UFix(1)
      resp_val := Bool(true)
    }
    in_buf := Cat(io.narrow.resp.bits, in_buf((rbits+w-1)/w*w-1,w))
  }

  io.wide.resp.valid := resp_val
  io.wide.resp.bits.tag := in_buf(io.wide.resp.bits.tag.width-1,0)
  io.wide.resp.bits.data := in_buf >> UFix(io.wide.resp.bits.tag.width)
}

class MemDesserIO(w: Int) extends Bundle {
  val narrow = new ioMemSerialized(w).flip
  val wide = new ioMem
}

class MemDesser(w: Int) extends Component // test rig side
{
  val io = new MemDesserIO(w)
  val abits = io.wide.req_cmd.bits.toBits.getWidth
  val dbits = io.wide.req_data.bits.toBits.getWidth
  val rbits = io.wide.resp.bits.getWidth

  require(dbits >= abits && rbits >= dbits)
  val recv_cnt = Reg(resetVal = UFix(0, log2Up((rbits+w-1)/w)))
  val data_recv_cnt = Reg(resetVal = UFix(0, log2Up(REFILL_CYCLES)))
  val adone = io.narrow.req.valid && recv_cnt === UFix((abits-1)/w)
  val ddone = io.narrow.req.valid && recv_cnt === UFix((dbits-1)/w)
  val rdone = io.narrow.resp.valid && recv_cnt === UFix((rbits-1)/w)

  val s_cmd_recv :: s_cmd :: s_data_recv :: s_data :: s_reply :: Nil = Enum(5) { UFix() }
  val state = Reg(resetVal = s_cmd_recv)

  val in_buf = Reg() { Bits() }
  when (io.narrow.req.valid && io.narrow.req.ready || io.narrow.resp.valid) {
    recv_cnt := recv_cnt + UFix(1)
    in_buf := Cat(io.narrow.req.bits, in_buf((rbits+w-1)/w*w-1,w))
  }
  io.narrow.req.ready := state === s_cmd_recv || state === s_data_recv

  when (state === s_cmd_recv && adone) {
    state := s_cmd
    recv_cnt := UFix(0)
  }
  when (state === s_cmd && io.wide.req_cmd.ready) {
    state := Mux(io.wide.req_cmd.bits.rw, s_data_recv, s_reply)
  }
  when (state === s_data_recv && ddone) {
    state := s_data
    recv_cnt := UFix(0)
  }
  when (state === s_data && io.wide.req_data.ready) {
    state := s_data_recv
    when (data_recv_cnt === UFix(REFILL_CYCLES-1)) {
      state := s_cmd_recv
    }
    data_recv_cnt := data_recv_cnt + UFix(1)
  }
  when (rdone) { // state === s_reply
    when (data_recv_cnt === UFix(REFILL_CYCLES-1)) {
      state := s_cmd_recv
    }
    recv_cnt := UFix(0)
    data_recv_cnt := data_recv_cnt + UFix(1)
  }

  val req_cmd = in_buf >> UFix(((rbits+w-1)/w - (abits+w-1)/w)*w)
  io.wide.req_cmd.valid := state === s_cmd
  io.wide.req_cmd.bits := io.wide.req_cmd.bits.fromBits(req_cmd)

  io.wide.req_data.valid := state === s_data
  io.wide.req_data.bits.data := in_buf >> UFix(((rbits+w-1)/w - (dbits+w-1)/w)*w)

  val dataq = (new Queue(REFILL_CYCLES)) { new MemResp }
  dataq.io.enq <> io.wide.resp
  dataq.io.deq.ready := recv_cnt === UFix((rbits-1)/w)

  io.narrow.resp.valid := dataq.io.deq.valid
  io.narrow.resp.bits := dataq.io.deq.bits.toBits >> (recv_cnt * UFix(w))
}
