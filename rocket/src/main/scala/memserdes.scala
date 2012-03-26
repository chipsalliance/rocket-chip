package rocket

import Chisel._
import Node._
import Constants._
import scala.math._

class ioMemSerialized(w: Int) extends Bundle
{
  val req = (new ioDecoupled) { Bits(width = w) }
  val resp = (new ioPipe) { Bits(width = w) }.flip
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
  val send_cnt = Reg(resetVal = UFix(0, log2up(max(abits, dbits))))
  val data_send_cnt = Reg(resetVal = UFix(0, log2up(MEM_DATA_BITS)))
  val adone = io.narrow.req.ready && send_cnt === UFix((abits-1)/w)
  val ddone = io.narrow.req.ready && send_cnt === UFix((dbits-1)/w)

  when (state === s_idle) {
    when (io.wide.req_cmd.valid) {
      state := Mux(io.wide.req_cmd.bits.rw, s_write_addr, s_read_addr)
    }
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
  }

  when (io.narrow.req.valid && io.narrow.req.ready) {
    send_cnt := Mux(adone, UFix(0), send_cnt + UFix(1))
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

  val recv_cnt = Reg() { UFix(width = log2up(rbits)) }
  val data_recv_cnt = Reg(resetVal = UFix(0, log2up(MEM_DATA_BITS)))
  val resp_val = Reg(resetVal = Bool(false))

  resp_val := Bool(false)
  when (io.narrow.resp.valid) {
    recv_cnt := recv_cnt + UFix(1)
    when (recv_cnt === UFix((rbits-1)/w)) {
      recv_cnt := UFix(0)
      data_recv_cnt := data_recv_cnt + UFix(1)
      resp_val := Bool(true)
    }
    in_buf := Cat(io.narrow.resp.bits, in_buf(rbits-1,w))
  }

  io.wide.resp.valid := resp_val
  io.wide.resp.bits.tag := in_buf(io.wide.resp.bits.tag.width-1,0)
  io.wide.resp.bits.data := in_buf >> UFix(io.wide.resp.bits.tag.width)
}
