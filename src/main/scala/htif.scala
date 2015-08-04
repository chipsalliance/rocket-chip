// See LICENSE for license details.

package uncore

import Chisel._
import Chisel.ImplicitConversions._
import uncore._

case object HTIFWidth extends Field[Int]
case object HTIFNSCR extends Field[Int]
case object HTIFOffsetBits extends Field[Int]
case object HTIFNCores extends Field[Int]

abstract trait HTIFParameters extends UsesParameters {
  val dataBits = params(TLDataBits)
  val dataBeats = params(TLDataBeats)
  val w = params(HTIFWidth)
  val nSCR = params(HTIFNSCR)
  val offsetBits = params(HTIFOffsetBits)
  val nCores = params(HTIFNCores)
}

abstract class HTIFBundle extends Bundle with HTIFParameters

class HostIO extends HTIFBundle
{
  val clk = Bool(OUTPUT)
  val clk_edge = Bool(OUTPUT)
  val in = Decoupled(Bits(width = w)).flip
  val out = Decoupled(Bits(width = w))
  val debug_stats_pcr = Bool(OUTPUT)
}

class PCRReq extends Bundle
{
  val rw = Bool()
  val addr = Bits(width = 12)
  val data = Bits(width = 64)
}

class HTIFIO extends HTIFBundle {
  val reset = Bool(INPUT)
  val id = UInt(INPUT, log2Up(nCores))
  val pcr_req = Decoupled(new PCRReq).flip
  val pcr_rep = Decoupled(Bits(width = 64))
  val ipi_req = Decoupled(Bits(width = log2Up(nCores)))
  val ipi_rep = Decoupled(Bool()).flip
  val debug_stats_pcr = Bool(OUTPUT)
    // wired directly to stats register
    // expected to be used to quickly indicate to testbench to do logging b/c in 'interesting' work
}

class SCRIO extends HTIFBundle {
  val rdata = Vec.fill(nSCR){Bits(INPUT, 64)}
  val wen = Bool(OUTPUT)
  val waddr = UInt(OUTPUT, log2Up(nSCR))
  val wdata = Bits(OUTPUT, 64)
}

class HTIFModuleIO extends HTIFBundle {
    val host = new HostIO
    val cpu = Vec.fill(nCores){new HTIFIO}.flip
    val mem = new ClientUncachedTileLinkIO
    val scr = new SCRIO
}

class HTIF(pcr_RESET: Int) extends Module with HTIFParameters {
  val io = new HTIFModuleIO

  io.host.debug_stats_pcr := io.cpu.map(_.debug_stats_pcr).reduce(_||_)
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
  val packet_ram = Mem(Bits(width = short_request_bits), packet_ram_depth)
  when (rx_word_done && io.host.in.ready) {
    packet_ram(rx_word_count(log2Up(packet_ram_depth)-1,0) - UInt(1)) := rx_shifter_in
  }

  val cmd_readmem :: cmd_writemem :: cmd_readcr :: cmd_writecr :: cmd_ack :: cmd_nack :: Nil = Enum(UInt(), 6)

  val pcr_addr = addr(io.cpu(0).pcr_req.bits.addr.getWidth-1, 0)
  val pcr_coreid = addr(log2Up(nCores)-1+20+1,20)
  val pcr_wdata = packet_ram(0)

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

  val state_rx :: state_pcr_req :: state_pcr_resp :: state_mem_rreq :: state_mem_wreq :: state_mem_rresp :: state_mem_wresp :: state_tx :: Nil = Enum(UInt(), 8)
  val state = Reg(init=state_rx)

  val (cnt, cnt_done) = Counter((state === state_mem_wreq && io.mem.acquire.ready) ||
                                 (state === state_mem_rresp && io.mem.grant.valid), dataBeats)
  val rx_cmd = Mux(rx_word_count === UInt(0), next_cmd, cmd)
  when (state === state_rx && rx_done) {
    state := Mux(rx_cmd === cmd_readmem, state_mem_rreq,
             Mux(rx_cmd === cmd_writemem, state_mem_wreq,
             Mux(rx_cmd === cmd_readcr || rx_cmd === cmd_writecr, state_pcr_req,
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

  // real-time counter (which doesn't really belong here...)
  val rtc = Reg(init=UInt(0,64))
  val rtc_tick = Counter(params(RTCPeriod)).inc()
  when (rtc_tick) { rtc := rtc + UInt(1) }

  val pcrReadData = Reg(Bits(width = io.cpu(0).pcr_rep.bits.getWidth))
  for (i <- 0 until nCores) {
    val my_reset = Reg(init=Bool(true))
    val my_ipi = Reg(init=Bool(false))

    val cpu = io.cpu(i)
    val me = pcr_coreid === UInt(i)
    cpu.pcr_req.valid := state === state_pcr_req && me && pcr_addr != UInt(pcr_RESET)
    cpu.pcr_req.bits.rw := cmd === cmd_writecr
    cpu.pcr_req.bits.addr := pcr_addr
    cpu.pcr_req.bits.data := pcr_wdata
    cpu.reset := my_reset

    // use pcr port to update core's rtc value periodically
    val rtc_sent = Reg(init=Bool(false))
    val rtc_outstanding = Reg(init=Bool(false))
    when (rtc_tick) { rtc_sent := Bool(false) }
    when (cpu.pcr_rep.valid) { rtc_outstanding := Bool(false) }
    when (rtc_outstanding) { cpu.pcr_req.valid := Bool(false) }
    when (state != state_pcr_req && state != state_pcr_resp && !rtc_sent && !rtc_outstanding) {
      cpu.pcr_req.valid := Bool(true)
      cpu.pcr_req.bits.rw := Bool(true)
      cpu.pcr_req.bits.addr := UInt(pcr_RESET) /* XXX this means write mtime */
      cpu.pcr_req.bits.data := rtc
      rtc_sent := cpu.pcr_req.ready
      rtc_outstanding := cpu.pcr_req.ready
    }

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

    when (state === state_pcr_req && cpu.pcr_req.fire()) {
      state := state_pcr_resp
    }
    when (state === state_pcr_req && me && pcr_addr === UInt(pcr_RESET)) {
      when (cmd === cmd_writecr) {
        my_reset := pcr_wdata(0)
      }
      pcrReadData := my_reset.toBits
      state := state_tx
    }

    cpu.pcr_rep.ready := Bool(true)
    when (state === state_pcr_resp && cpu.pcr_rep.valid) {
      pcrReadData := cpu.pcr_rep.bits
      state := state_tx
    }
  }

  val scr_addr = addr(log2Up(nSCR)-1, 0)
  val scr_rdata = Wire(Vec(Bits(width=64), io.scr.rdata.size))
  for (i <- 0 until scr_rdata.size)
    scr_rdata(i) := io.scr.rdata(i)
  scr_rdata(0) := UInt(nCores)
  scr_rdata(1) := UInt((BigInt(dataBits*dataBeats/8) << params(TLBlockAddrBits)) >> 20)

  io.scr.wen := Bool(false)
  io.scr.wdata := pcr_wdata
  io.scr.waddr := scr_addr.toUInt
  when (state === state_pcr_req && pcr_coreid.andR) {
    io.scr.wen := cmd === cmd_writecr
    pcrReadData := scr_rdata(scr_addr)
    state := state_tx
  }

  val tx_cmd = Mux(nack, cmd_nack, cmd_ack)
  val tx_cmd_ext = Cat(Bits(0, 4-tx_cmd.getWidth), tx_cmd)
  val tx_header = Cat(addr, seqno, tx_size, tx_cmd_ext)
  val tx_data = Mux(tx_word_count === UInt(0), tx_header,
                Mux(cmd === cmd_readcr || cmd === cmd_writecr, pcrReadData,
                packet_ram(packet_ram_raddr)))

  io.host.in.ready := state === state_rx
  io.host.out.valid := state === state_tx
  io.host.out.bits := tx_data >> Cat(tx_count(log2Up(short_request_bits/w)-1,0), Bits(0, log2Up(w)))
}
