package rocket

import Chisel._
import Node._;
import Constants._;

class ioHost(w: Int, view: List[String] = null) extends Bundle(view)
{
  val in = new ioDecoupled()(Bits(width = w)).flip
  val out = new ioDecoupled()(Bits(width = w))
}

class PCRReq extends Bundle
{
  val rw = Bool()
  val addr = Bits(width = 5)
  val data = Bits(width = 64)
}

class ioHTIF extends Bundle
{
  val reset = Bool(INPUT)
  val pcr_req = (new ioDecoupled) { new PCRReq }.flip
  val pcr_rep = (new ioPipe) { Bits(width = 64) }
}

class rocketHTIF(w: Int, ncores: Int) extends Component with FourStateCoherence
{
  val io = new Bundle {
    val host = new ioHost(w)
    val cpu = Vec(ncores) { new ioHTIF().flip }
    val mem = new ioTileLink
  }

  val short_request_bits = 64
  val long_request_bits = 576
  require(short_request_bits % w == 0)

  val rx_count_w = 13 + log2up(8) - log2up(w) // data size field is 12 bits
  val rx_count = Reg(resetVal = UFix(0,rx_count_w))
  val rx_shifter = Reg() { Bits(width = short_request_bits) }
  val header = Reg() { Bits() }
  val rx_shifter_in = Cat(io.host.in.bits, rx_shifter(short_request_bits-1,w))
  when (io.host.in.valid && io.host.in.ready) {
    rx_shifter := rx_shifter_in
    rx_count := rx_count + UFix(1)
    when (rx_count === UFix(short_request_bits/w-1)) {
      header := rx_shifter_in
    }
  }

  val rx_count_words = rx_count >> UFix(log2up(short_request_bits/w))
  val packet_ram_wen = rx_count(log2up(short_request_bits/w)-1,0).andR &&
                       io.host.in.valid && io.host.in.ready
  val packet_ram = Mem(long_request_bits/short_request_bits-1,
                       packet_ram_wen, rx_count_words - UFix(1), rx_shifter_in)

  val cmd_readmem :: cmd_writemem :: cmd_readcr :: cmd_writecr :: cmd_ack :: cmd_nack :: Nil = Enum(6) { UFix() }
  val cmd = header(3,0)
  val size = header(15,4)
  val seqno = header(23,16)
  val addr = header(63,24).toUFix

  val pcr_addr = addr(19,0)
  val pcr_coreid = addr(39,20)
  val pcr_wdata = packet_ram(UFix(0))

  val nack = Mux(cmd === cmd_readmem || cmd === cmd_writemem, size != UFix((1 << OFFSET_BITS)/8),
             Mux(cmd === cmd_readcr || cmd === cmd_writecr, size != UFix(1),
             Bool(true)))

  val tx_count = Reg(resetVal = UFix(0, log2up(long_request_bits/w+1)))
  val packet_ram_raddr = (tx_count >> UFix(log2up(short_request_bits/w)))
  when (io.host.out.valid && io.host.out.ready) {
    tx_count := tx_count + UFix(1)
  }

  val rx_size = Mux(cmd === cmd_writemem || cmd === cmd_writecr, size, UFix(0))
  val rx_done = rx_count >= UFix(short_request_bits/w) && rx_count_words-UFix(1) === rx_size
  val tx_size = Mux(!nack && cmd === cmd_readmem, UFix((1 << OFFSET_BITS)/8),
                Mux(!nack && cmd === cmd_readcr, UFix(1), UFix(0)))
  val tx_done = packet_ram_raddr - UFix(1) === tx_size

  val mem_acked = Reg(resetVal = Bool(false))
  val mem_gxid = Reg() { Bits() }
  val mem_needs_ack = Reg() { Bool() }
  val mem_nacked = Reg(resetVal = Bool(false))
  when (io.mem.xact_rep.valid) { 
    mem_acked := Bool(true)
    mem_gxid := io.mem.xact_rep.bits.global_xact_id
    mem_needs_ack := io.mem.xact_rep.bits.require_ack  
  }
  when (io.mem.xact_abort.valid) { mem_nacked := Bool(true) }

  val state_rx :: state_pcr :: state_mem_req :: state_mem_wdata :: state_mem_wresp :: state_mem_rdata :: state_mem_finish :: state_tx :: Nil = Enum(8) { UFix() }
  val state = Reg(resetVal = state_rx)

  when (state === state_rx && rx_done) {
    state := Mux(cmd === cmd_readmem || cmd === cmd_writemem, state_mem_req,
             Mux(cmd === cmd_readcr || cmd === cmd_writecr, state_pcr,
             state_tx))
  }

  val pcr_done = Reg() { Bool() }
  when (state === state_pcr && pcr_done) {
    state := state_tx
  }

  val mem_cnt = Reg(resetVal = UFix(0, log2up(REFILL_CYCLES)))
  when (state === state_mem_req && io.mem.xact_init.ready) {
    state := Mux(cmd === cmd_writemem, state_mem_wdata, state_mem_rdata)
  }
  when (state === state_mem_wdata && io.mem.xact_init_data.ready) {
    when (mem_cnt.andR)  {
      state := state_mem_wresp
    }
    mem_cnt := mem_cnt + UFix(1)
  }
  when (state === state_mem_wresp) {
    when (mem_nacked) {
      state := state_mem_req
      mem_nacked := Bool(false)
    }
    when (mem_acked) {
      state := state_mem_finish
      mem_acked := Bool(false)
    }
  }
  when (state === state_mem_rdata) {
    when (mem_nacked) {
      state := state_mem_req
      mem_nacked := Bool(false)
    }
    when (io.mem.xact_rep.valid) {
      when (mem_cnt.andR)  {
        state := state_mem_finish
      }
      mem_cnt := mem_cnt + UFix(1)
    }
    mem_acked := Bool(false)
  }
  when (state === state_mem_finish && io.mem.xact_finish.ready) {
    state := state_tx
  }
  when (state === state_tx && tx_done) {
    rx_count := UFix(0)
    tx_count := UFix(0)
    state := state_rx
  }

  var mem_req_data: Bits = null
  for (i <- 0 until MEM_DATA_BITS/short_request_bits) {
    val idx = Cat(mem_cnt, UFix(i, log2up(MEM_DATA_BITS/short_request_bits)))
    packet_ram.write(idx, io.mem.xact_rep.bits.data((i+1)*short_request_bits-1, i*short_request_bits),
                     state === state_mem_rdata && io.mem.xact_rep.valid)
    mem_req_data = Cat(packet_ram.read(idx), mem_req_data)
  }
  io.mem.xact_init.valid := state === state_mem_req
  io.mem.xact_init.bits.t_type := Mux(cmd === cmd_writemem, X_INIT_WRITE_UNCACHED, X_INIT_READ_UNCACHED)
  io.mem.xact_init.bits.address := addr >> UFix(OFFSET_BITS-3)
  io.mem.xact_init_data.valid:= state === state_mem_wdata
  io.mem.xact_init_data.bits.data := mem_req_data
  io.mem.xact_finish.valid := (state === state_mem_finish) && mem_needs_ack
  io.mem.xact_finish.bits.global_xact_id := mem_gxid

  val probe_q = (new queue(1)) { new ProbeReply }
  probe_q.io.enq.valid := io.mem.probe_req.valid
  io.mem.probe_req.ready := probe_q.io.enq.ready
  probe_q.io.enq.bits := newProbeReply(io.mem.probe_req.bits, newStateOnFlush())
  io.mem.probe_rep <> probe_q.io.deq
  io.mem.probe_rep_data.valid := Bool(false)

  pcr_done := Bool(false)
  val pcr_mux = (new Mux1H(ncores)) { Bits(width = 64) }
  for (i <- 0 until ncores) {
    val my_reset = Reg(resetVal = Bool(true))
    val rdata = Reg() { Bits() }

    val cpu = io.cpu(i)
    val me = pcr_coreid === UFix(i)
    cpu.pcr_req.valid := state === state_pcr && me
    cpu.pcr_req.bits.rw := cmd === cmd_writecr
    cpu.pcr_req.bits.addr := pcr_addr
    cpu.pcr_req.bits.data := pcr_wdata
    cpu.reset := my_reset

    when (cpu.pcr_req.valid && cpu.pcr_req.ready && cpu.pcr_req.bits.rw) {
      pcr_done := Bool(true)
      when (cpu.pcr_req.bits.addr === PCR_RESET) {
        my_reset := cpu.pcr_req.bits.data(0)
      }
    }
    when (cpu.pcr_rep.valid) {
      pcr_done := Bool(true)
      rdata := cpu.pcr_rep.bits
    }

    pcr_mux.io.sel(i) := me
    pcr_mux.io.in(i) := Mux(pcr_addr === PCR_RESET, my_reset, rdata)
  }

  val tx_cmd = Mux(nack, cmd_nack, cmd_ack)
  val tx_cmd_ext = Cat(Bits(0, 4-tx_cmd.getWidth), tx_cmd)
  val tx_size_ext = Cat(Bits(0, 12-tx_size.getWidth), tx_size)
  val tx_header = Cat(addr, seqno, tx_size_ext, tx_cmd_ext)
  val tx_data = Mux(packet_ram_raddr === UFix(0), tx_header,
                Mux(packet_ram_raddr === UFix(1) && cmd === cmd_readcr, pcr_mux.io.out,
                packet_ram(packet_ram_raddr - UFix(1))))

  io.host.in.ready := state === state_rx && !rx_done
  io.host.out.valid := state === state_tx && !tx_done
  io.host.out.bits := tx_data >> Cat(tx_count(log2up(short_request_bits/w)-1,0), Bits(0, log2up(w)))
}
