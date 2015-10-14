// See LICENSE for license details.

package uncore

import Chisel._
import Chisel.ImplicitConversions._
import junctions._

case object HtifKey extends Field[HtifParameters]

case class HtifParameters(width: Int, nCores: Int, offsetBits: Int, nSCR: Int = 64)

trait HasHtifParameters {
  implicit val p: Parameters
  val external = p(HtifKey)
  val dataBits = p(TLKey(p(TLId))).dataBitsPerBeat
  val dataBeats = p(TLKey(p(TLId))).dataBeats
  val w = external.width
  val nSCR = external.nSCR
  val scrAddrBits = log2Up(nSCR)
  val scrDataBits = 64
  val scrDataBytes = scrDataBits / 8
  val offsetBits = external.offsetBits
  val nCores = external.nCores
}

abstract class HtifModule(implicit val p: Parameters) extends Module with HasHtifParameters
abstract class HtifBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasHtifParameters

class HostIO(w: Int) extends Bundle {
  val clk = Bool(OUTPUT)
  val clk_edge = Bool(OUTPUT)
  val in = Decoupled(Bits(width = w)).flip
  val out = Decoupled(Bits(width = w))
  val debug_stats_csr = Bool(OUTPUT)
}

class HtifIO(implicit p: Parameters) extends HtifBundle()(p) {
  val reset = Bool(INPUT)
  val id = UInt(INPUT, log2Up(nCores))
  val csr = new SMIIO(scrDataBits, 12).flip
  val ipi_req = Decoupled(Bits(width = log2Up(nCores)))
  val ipi_rep = Decoupled(Bool()).flip
  val debug_stats_csr = Bool(OUTPUT)
    // wired directly to stats register
    // expected to be used to quickly indicate to testbench to do logging b/c in 'interesting' work
}

class Htif(csr_RESET: Int)(implicit val p: Parameters) extends Module with HasHtifParameters {
  val io = new Bundle {
    val host = new HostIO(w)
    val cpu = Vec(new HtifIO, nCores).flip
    val mem = new ClientUncachedTileLinkIO
    val scr = new SMIIO(scrDataBits, scrAddrBits)
  }

  io.host.debug_stats_csr := io.cpu.map(_.debug_stats_csr).reduce(_||_)
    // system is 'interesting' if any tile is 'interesting'

  val short_request_bits = 64
  val long_request_bits = short_request_bits + dataBits*dataBeats
  require(short_request_bits % w == 0)

  val rx_count_w = 13 + log2Up(64) - log2Up(w) // data size field is 12 bits
  val rx_count = Reg(init=UInt(0,rx_count_w))
  val rx_shifter = Reg(Bits(width = short_request_bits))
  val rx_shifter_in = Cat(io.host.in.bits, rx_shifter(short_request_bits-1,w))
  val next_cmd = rx_shifter_in(3,0)
  val cmd = Reg(Bits())
  val size = Reg(Bits())
  val pos = Reg(Bits())
  val seqno = Reg(Bits())
  val addr = Reg(Bits())
  when (io.host.in.valid && io.host.in.ready) {
    rx_shifter := rx_shifter_in
    rx_count := rx_count + UInt(1)
    when (rx_count === UInt(short_request_bits/w-1)) {
      cmd := next_cmd
      size := rx_shifter_in(15,4)
      pos := rx_shifter_in(15,4+offsetBits-3)
      seqno := rx_shifter_in(23,16)
      addr := rx_shifter_in(63,24)
    }
  }

  val rx_word_count = (rx_count >> log2Up(short_request_bits/w))
  val rx_word_done = io.host.in.valid && rx_count(log2Up(short_request_bits/w)-1,0).andR
  val packet_ram_depth = long_request_bits/short_request_bits-1
  val packet_ram = Mem(packet_ram_depth, Bits(width = short_request_bits))
  when (rx_word_done && io.host.in.ready) {
    packet_ram(rx_word_count(log2Up(packet_ram_depth)-1,0) - UInt(1)) := rx_shifter_in
  }

  val cmd_readmem :: cmd_writemem :: cmd_readcr :: cmd_writecr :: cmd_ack :: cmd_nack :: Nil = Enum(UInt(), 6)

  val csr_addr = addr(io.cpu(0).csr.req.bits.addr.getWidth-1, 0)
  val csr_coreid = addr(log2Up(nCores)-1+20+1,20)
  val csr_wdata = packet_ram(0)

  val bad_mem_packet = size(offsetBits-1-3,0).orR || addr(offsetBits-1-3,0).orR
  val nack = Mux(cmd === cmd_readmem || cmd === cmd_writemem, bad_mem_packet,
             Mux(cmd === cmd_readcr || cmd === cmd_writecr, size != UInt(1),
             Bool(true)))

  val tx_count = Reg(init=UInt(0, rx_count_w))
  val tx_subword_count = tx_count(log2Up(short_request_bits/w)-1,0)
  val tx_word_count = tx_count(rx_count_w-1, log2Up(short_request_bits/w))
  val packet_ram_raddr = tx_word_count(log2Up(packet_ram_depth)-1,0) - UInt(1)
  when (io.host.out.valid && io.host.out.ready) {
    tx_count := tx_count + UInt(1)
  }

  val rx_done = rx_word_done && Mux(rx_word_count === UInt(0), next_cmd != cmd_writemem && next_cmd != cmd_writecr, rx_word_count === size || rx_word_count(log2Up(packet_ram_depth)-1,0) === UInt(0))
  val tx_size = Mux(!nack && (cmd === cmd_readmem || cmd === cmd_readcr || cmd === cmd_writecr), size, UInt(0))
  val tx_done = io.host.out.ready && tx_subword_count.andR && (tx_word_count === tx_size || tx_word_count > UInt(0) && packet_ram_raddr.andR)

  val state_rx :: state_csr_req :: state_csr_resp :: state_mem_rreq :: state_mem_wreq :: state_mem_rresp :: state_mem_wresp :: state_tx :: Nil = Enum(UInt(), 8)
  val state = Reg(init=state_rx)

  val (cnt, cnt_done) = Counter((state === state_mem_wreq && io.mem.acquire.ready) ||
                                 (state === state_mem_rresp && io.mem.grant.valid), dataBeats)
  val rx_cmd = Mux(rx_word_count === UInt(0), next_cmd, cmd)
  when (state === state_rx && rx_done) {
    state := Mux(rx_cmd === cmd_readmem, state_mem_rreq,
             Mux(rx_cmd === cmd_writemem, state_mem_wreq,
             Mux(rx_cmd === cmd_readcr || rx_cmd === cmd_writecr, state_csr_req,
             state_tx)))
  }
  when (state === state_mem_wreq) {
    when (cnt_done) { state := state_mem_wresp }
  }
  when (state === state_mem_rreq) {
    when(io.mem.acquire.ready) { state := state_mem_rresp }
  }
  when (state === state_mem_wresp && io.mem.grant.valid) {
    state := Mux(cmd === cmd_readmem || pos === UInt(1),  state_tx, state_rx)
    pos := pos - UInt(1)
    addr := addr + UInt(1 << offsetBits-3)
  }
  when (state === state_mem_rresp && cnt_done) {
    state := Mux(cmd === cmd_readmem || pos === UInt(1),  state_tx, state_rx)
    pos := pos - UInt(1)
    addr := addr + UInt(1 << offsetBits-3)
  }
  when (state === state_tx && tx_done) {
    when (tx_word_count === tx_size) {
      rx_count := UInt(0)
      tx_count := UInt(0)
    }
    state := Mux(cmd === cmd_readmem && pos != UInt(0), state_mem_rreq, state_rx)
  }

  val n = dataBits/short_request_bits
  val mem_req_data = (0 until n).map { i =>
    val ui = UInt(i, log2Up(n))
    when (state === state_mem_rresp && io.mem.grant.valid) {
      packet_ram(Cat(io.mem.grant.bits.addr_beat, ui)) := 
        io.mem.grant.bits.data((i+1)*short_request_bits-1, i*short_request_bits)
    }
    packet_ram(Cat(cnt, ui))
  }.reverse.reduce(_##_)

  val init_addr = addr.toUInt >> (offsetBits-3)
  io.mem.acquire.valid := state === state_mem_rreq || state === state_mem_wreq
  io.mem.acquire.bits := Mux(cmd === cmd_writemem, 
    PutBlock(
      addr_block = init_addr,
      addr_beat = cnt,
      client_xact_id = UInt(0),
      data = mem_req_data),
    GetBlock(addr_block = init_addr))
  io.mem.grant.ready := Bool(true)

  val csrReadData = Reg(Bits(width = io.cpu(0).csr.resp.bits.getWidth))
  for (i <- 0 until nCores) {
    val my_reset = Reg(init=Bool(true))
    val my_ipi = Reg(init=Bool(false))

    val cpu = io.cpu(i)
    val me = csr_coreid === UInt(i)
    cpu.csr.req.valid := state === state_csr_req && me && csr_addr != UInt(csr_RESET)
    cpu.csr.req.bits.rw := cmd === cmd_writecr
    cpu.csr.req.bits.addr := csr_addr
    cpu.csr.req.bits.data := csr_wdata
    cpu.reset := my_reset

    when (cpu.ipi_rep.ready) {
      my_ipi := Bool(false)
    }
    cpu.ipi_rep.valid := my_ipi
    cpu.ipi_req.ready := Bool(true)
    for (j <- 0 until nCores) {
      when (io.cpu(j).ipi_req.valid && io.cpu(j).ipi_req.bits === UInt(i)) {
        my_ipi := Bool(true)
      }
    }

    when (cpu.csr.req.fire()) { state := state_csr_resp }

    when (state === state_csr_req && me && csr_addr === UInt(csr_RESET)) {
      when (cmd === cmd_writecr) {
        my_reset := csr_wdata(0)
      }
      csrReadData := my_reset.toBits
      state := state_tx
    }

    cpu.csr.resp.ready := Bool(true)
    when (state === state_csr_resp && cpu.csr.resp.valid) {
      csrReadData := cpu.csr.resp.bits
      state := state_tx
    }
  }

  io.scr.req.valid := (state === state_csr_req && csr_coreid.andR)
  io.scr.req.bits.addr := addr(scrAddrBits - 1, 0).toUInt
  io.scr.req.bits.data := csr_wdata
  io.scr.req.bits.rw := (cmd === cmd_writecr)
  io.scr.resp.ready := Bool(true)

  when (io.scr.req.fire()) { state := state_csr_resp }
  when (state === state_csr_resp && io.scr.resp.valid) {
    csrReadData := io.scr.resp.bits
    state := state_tx
  }

  val tx_cmd = Mux(nack, cmd_nack, cmd_ack)
  val tx_cmd_ext = Cat(Bits(0, 4-tx_cmd.getWidth), tx_cmd)
  val tx_header = Cat(addr, seqno, tx_size, tx_cmd_ext)
  val tx_data = Mux(tx_word_count === UInt(0), tx_header,
                Mux(cmd === cmd_readcr || cmd === cmd_writecr, csrReadData,
                packet_ram(packet_ram_raddr)))

  io.host.in.ready := state === state_rx
  io.host.out.valid := state === state_tx
  io.host.out.bits := tx_data >> Cat(tx_count(log2Up(short_request_bits/w)-1,0), Bits(0, log2Up(w)))
}
