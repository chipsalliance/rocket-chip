package groundtest

import Chisel._
import junctions._
import junctions.NastiConstants._
import uncore._
import cde.Parameters

abstract class UnitTest extends Module {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
    val start = Bool(INPUT)
  }
}

class MemoryTestDriver(name: String, dataWidth: Int, burstLen: Int, nBursts: Int)
    (implicit p: Parameters) extends NastiModule {
  val io = new Bundle {
    val nasti = new NastiIO
    val finished = Bool(OUTPUT)
    val start = Bool(INPUT)
  }

  val dataBytes = dataWidth / 8
  val nastiDataBytes = nastiXDataBits / 8

  val (write_cnt, write_done) = Counter(io.nasti.w.fire(), burstLen)
  val (read_cnt, read_done) = Counter(io.nasti.r.fire(), burstLen)
  val (req_cnt, reqs_done) = Counter(read_done, nBursts)

  val req_addr = Cat(req_cnt, UInt(0, log2Up(burstLen * dataBytes)))

  val write_data    = UInt(0x10000000L, dataWidth) | Cat(req_cnt, write_cnt)
  val expected_data = UInt(0x10000000L, dataWidth) | Cat(req_cnt, read_cnt)

  val (s_idle :: s_write_addr :: s_write_data :: s_write_stall :: s_write_resp ::
       s_read_addr :: s_read_data :: s_read_stall :: s_done :: Nil) = Enum(Bits(), 9)
  val state = Reg(init = s_idle)

  val (stall_cnt, stall_done) = Counter(state === s_read_stall, 2)

  io.nasti.aw.valid := (state === s_write_addr)
  io.nasti.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = req_addr,
    size = UInt(log2Up(dataBytes)),
    len = UInt(burstLen - 1))

  io.nasti.w.valid := (state === s_write_data)
  io.nasti.w.bits := NastiWriteDataChannel(
    data = Cat(write_data, write_data),
    last = (write_cnt === UInt(burstLen - 1)))

  io.nasti.b.ready := (state === s_write_resp)

  io.nasti.ar.valid := (state === s_read_addr)
  io.nasti.ar.bits := NastiReadAddressChannel(
    id = UInt(0),
    addr = req_addr,
    size = UInt(log2Up(dataBytes)),
    len = UInt(burstLen - 1))

  io.nasti.r.ready := (state === s_read_data)

  io.finished := (state === s_done)

  when (state === s_idle && io.start) { state := s_write_addr }
  when (io.nasti.aw.fire()) { state := s_write_data }
  when (io.nasti.w.fire()) { state := s_write_stall }
  when (state === s_write_stall) { state := s_write_data }
  when (write_done) { state := s_write_resp }
  when (io.nasti.b.fire()) { state := s_read_addr }
  when (io.nasti.ar.fire()) { state := s_read_data }
  when (io.nasti.r.fire()) { state := s_read_stall }
  when (stall_done) { state := s_read_data }
  when (read_done) { state := s_write_addr }
  when (reqs_done) { state := s_done }

  val full_addr = req_addr + (read_cnt << UInt(log2Up(dataBytes)))
  val byteshift = full_addr(log2Up(nastiDataBytes) - 1, 0)
  val bitshift = Cat(byteshift, UInt(0, 3))
  val read_data = (io.nasti.r.bits.data >> bitshift) & Fill(dataWidth, UInt(1, 1))

  assert(!io.nasti.r.valid || read_data === expected_data,
    s"MemoryTestDriver for $name got wrong data")

  val ar_timeout = Timer(1024, io.nasti.ar.fire(), io.nasti.r.fire())
  val aw_timeout = Timer(1024, io.nasti.aw.fire(), io.nasti.b.fire())

  assert(!ar_timeout && !aw_timeout,
    s"MemoryTestDriver for $name timed out")
}

class HastiTest(implicit p: Parameters) extends UnitTest {
  val sram = Module(new HastiTestSRAM(8))
  val bus = Module(new HastiBus(Seq(a => Bool(true))))
  val conv = Module(new HastiMasterIONastiIOConverter)
  val driver = Module(new MemoryTestDriver("HastiTest", 32, 8, 2))

  bus.io.slaves(0) <> sram.io
  bus.io.master <> conv.io.hasti
  conv.io.nasti <> driver.io.nasti
  io.finished := driver.io.finished
  driver.io.start := io.start
}

class NastiDemuxDriver(n: Int)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val start = Bool(INPUT)
    val finished = Bool(OUTPUT)
    val nasti = new NastiIO
    val select = UInt(OUTPUT, log2Up(n))
  }

  val (s_idle :: s_write_addr :: s_write_data :: s_write_resp ::
       s_read_addr :: s_read_resp :: s_done :: Nil) = Enum(Bits(), 7)
  val state = Reg(init = s_idle)

  val select = Reg(init = UInt(0, log2Up(n)))

  when (state === s_idle && io.start) { state := s_write_addr }
  when (io.nasti.aw.fire()) { state := s_write_data }
  when (io.nasti.w.fire()) { state := s_write_resp }
  when (io.nasti.b.fire()) { state := s_read_addr }
  when (io.nasti.ar.fire()) { state := s_read_resp }
  when (io.nasti.r.fire()) {
    when (select === UInt(n - 1)) {
      state := s_done
    } .otherwise {
      select := select + UInt(1)
      state := s_write_addr
    }
  }

  io.nasti.aw.valid := (state === s_write_addr)
  io.nasti.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = UInt(0),
    size = UInt("b011"))
  io.nasti.w.valid := (state === s_write_data)
  io.nasti.w.bits := NastiWriteDataChannel(data = select)
  io.nasti.b.ready := (state === s_write_resp)
  io.nasti.ar.valid := (state === s_read_addr)
  io.nasti.ar.bits := NastiReadAddressChannel(
    id = UInt(0),
    addr = UInt(0),
    size = UInt("b011"))
  io.nasti.r.ready := (state === s_read_resp)

  io.finished := (state === s_done)
  io.select := select

  assert(!io.nasti.r.valid || io.nasti.r.bits.data === select,
    "NASTI DeMux test: Read data did not match")
}

class NastiDemuxSlave(implicit p: Parameters) extends NastiModule()(p) {
  val io = (new NastiIO).flip

  val (s_write_wait :: s_write_data :: s_write_resp ::
       s_read_wait :: s_read_resp :: s_done :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_write_wait)

  val value = Reg(UInt(width = 64))
  val id = Reg(UInt(width = nastiXIdBits))

  when (io.aw.fire()) {
    id := io.aw.bits.id
    state := s_write_data
  }

  when (io.w.fire()) {
    value := io.w.bits.data
    state := s_write_resp
  }

  when (io.b.fire()) { state := s_read_wait }

  when (io.ar.fire()) {
    id := io.ar.bits.id
    state := s_read_resp
  }

  when (io.r.fire()) { state := s_done }

  io.aw.ready := (state === s_write_wait)
  io.w.ready := (state === s_write_data)
  io.b.valid := (state === s_write_resp)
  io.b.bits := NastiWriteResponseChannel(id = id)
  io.ar.ready := (state === s_read_wait)
  io.r.valid := (state === s_read_resp)
  io.r.bits := NastiReadDataChannel(id = id, data = value)
}

class NastiMemoryDemuxTest(implicit p: Parameters) extends UnitTest {
  val nSlaves = 4

  val driver = Module(new NastiDemuxDriver(nSlaves))
  driver.io.start := io.start
  io.finished := driver.io.finished

  val demux = Module(new NastiMemoryDemux(nSlaves))
  demux.io.master <> driver.io.nasti
  demux.io.select := driver.io.select

  for (i <- 0 until nSlaves) {
    val slave = Module(new NastiDemuxSlave)
    slave.io <> demux.io.slaves(i)
  }
}

class NastiToHostTestDriver(htifW: Int)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
    val start = Bool(INPUT)
    val nasti = new NastiIO
    val reset = Bool(INPUT)
  }

  val DCOUNT_ADDR = 0x00
  val RFIFO_ADDR  = 0x04

  val WFIFO_ADDR = 0x00
  val RESET_ADDR = 0x7c

  val (s_idle :: s_fifo_wr_addr :: s_fifo_wr_data :: s_fifo_wr_resp ::
       s_rst_wr_addr :: s_rst_wr_data :: s_rst_wr_resp ::
       s_cnt_rd_addr :: s_cnt_rd_data :: s_fifo_rd_addr :: s_fifo_rd_data ::
       s_done :: Nil) = Enum(Bits(), 12)
  val state = Reg(init = s_idle)

  val nBeats = 4
  val (wr_cnt, wr_done) = Counter(
    state === s_fifo_wr_data && io.nasti.w.ready, nBeats)
  val (rd_cnt, rd_done) = Counter(
    state === s_fifo_rd_data && io.nasti.r.valid, nBeats)

  val test_data = Vec.tabulate(nBeats) { i => UInt(i * 0x304, 32) }

  io.nasti.ar.valid := (state === s_cnt_rd_addr) || (state === s_fifo_rd_addr)
  io.nasti.ar.bits := NastiReadAddressChannel(
    id = UInt(0),
    addr = Mux(state === s_cnt_rd_addr, UInt(DCOUNT_ADDR), UInt(RFIFO_ADDR)),
    len = Mux(state === s_cnt_rd_addr, UInt(0), UInt(nBeats - 1)),
    size = UInt("b010"),
    burst = BURST_FIXED)

  io.nasti.aw.valid := (state === s_fifo_wr_addr) || (state === s_rst_wr_addr)
  io.nasti.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = Mux(state === s_rst_wr_addr, UInt(RESET_ADDR), UInt(WFIFO_ADDR)),
    len = Mux(state === s_rst_wr_addr, UInt(0), UInt(nBeats - 1)),
    size = UInt("b010"),
    burst = BURST_FIXED)

  io.nasti.w.valid := (state === s_fifo_wr_data) || (state === s_rst_wr_data)
  io.nasti.w.bits := NastiWriteDataChannel(
    data = Mux(state === s_rst_wr_data, UInt(0), test_data(wr_cnt)),
    last = Mux(state === s_rst_wr_data, Bool(true), (wr_cnt === UInt(nBeats - 1))))

  io.nasti.r.ready := (state === s_fifo_rd_data) || (state === s_cnt_rd_data)
  io.nasti.b.ready := (state === s_fifo_wr_resp) || (state === s_rst_wr_resp)

  when (state === s_idle && io.start) { state := s_fifo_wr_addr }

  when (io.nasti.ar.fire()) {
    state := Mux(state === s_fifo_rd_addr, s_fifo_rd_data, s_cnt_rd_data)
  }

  when (io.nasti.aw.fire()) {
    state := Mux(state === s_fifo_wr_addr, s_fifo_wr_data, s_rst_wr_data)
  }

  when (wr_done) { state := s_fifo_wr_resp }
  when (state === s_rst_wr_data && io.nasti.w.ready) {
    state := s_rst_wr_resp
  }

  when (io.nasti.b.fire()) {
    state := Mux(state === s_fifo_wr_resp, s_rst_wr_addr, s_cnt_rd_addr)
  }

  when (state === s_cnt_rd_data && io.nasti.r.valid) {
    state := s_fifo_rd_addr
  }
  when (rd_done) { state := s_done }

  io.finished := (state === s_done)

  assert(state =/= s_fifo_rd_data || !io.nasti.r.valid ||
    io.nasti.r.bits.data === test_data(rd_cnt),
    "NastiIO to HostIO result does not match")

  assert(state =/= s_cnt_rd_data || !io.nasti.r.valid ||
    io.nasti.r.bits.data === UInt(nBeats),
    "NastiIO to HostIO count is not correct")

  assert(state =/= s_rst_wr_data || !io.nasti.w.ready || io.reset,
    "NastiIO to HostIO reset did not fire")
}

class NastiIOHostIOConverterTest(implicit p: Parameters) extends UnitTest {
  val conv = Module(new NastiIOHostIOConverter(16)(p.alterPartial({
    case NastiKey => NastiParameters(
      dataBits = 32,
      addrBits = p(PAddrBits),
      idBits = 5)
  })))
  val driver = Module(new NastiToHostTestDriver(16))
  conv.io.nasti <> driver.io.nasti
  conv.io.host.out.bits := conv.io.host.in.bits
  conv.io.host.out.valid := conv.io.host.in.valid
  conv.io.host.in.ready := conv.io.host.out.ready
  driver.io.reset := conv.io.reset
  driver.io.start := io.start
  io.finished := driver.io.finished
}

class MultiWidthFifoTest extends UnitTest {
  val big2little = Module(new MultiWidthFifo(16, 8, 8))
  val little2big = Module(new MultiWidthFifo(8, 16, 4))

  val bl_send = Reg(init = Bool(false))
  val lb_send = Reg(init = Bool(false))
  val bl_recv = Reg(init = Bool(false))
  val lb_recv = Reg(init = Bool(false))
  val bl_finished = Reg(init = Bool(false))
  val lb_finished = Reg(init = Bool(false))

  val bl_data = Vec.tabulate(4){i => UInt((2 * i + 1) * 256 + 2 * i, 16)}
  val lb_data = Vec.tabulate(8){i => UInt(i, 8)}

  val (bl_send_cnt, bl_send_done) = Counter(big2little.io.in.fire(), 4)
  val (lb_send_cnt, lb_send_done) = Counter(little2big.io.in.fire(), 8)

  val (bl_recv_cnt, bl_recv_done) = Counter(big2little.io.out.fire(), 8)
  val (lb_recv_cnt, lb_recv_done) = Counter(little2big.io.out.fire(), 4)

  big2little.io.in.valid := bl_send
  big2little.io.in.bits := bl_data(bl_send_cnt)
  big2little.io.out.ready := bl_recv

  little2big.io.in.valid := lb_send
  little2big.io.in.bits := lb_data(lb_send_cnt)
  little2big.io.out.ready := lb_recv

  val bl_recv_data_idx = bl_recv_cnt >> UInt(1)
  val bl_recv_data = Mux(bl_recv_cnt(0),
    bl_data(bl_recv_data_idx)(15, 8),
    bl_data(bl_recv_data_idx)(7, 0))

  val lb_recv_data = Cat(
    lb_data(Cat(lb_recv_cnt, UInt(1, 1))),
    lb_data(Cat(lb_recv_cnt, UInt(0, 1))))

  when (io.start) {
    bl_send := Bool(true)
    lb_send := Bool(true)
  }

  when (bl_send_done) {
    bl_send := Bool(false)
    bl_recv := Bool(true)
  }

  when (lb_send_done) {
    lb_send := Bool(false)
    lb_recv := Bool(true)
  }

  when (bl_recv_done) {
    bl_recv := Bool(false)
    bl_finished := Bool(true)
  }

  when (lb_recv_done) {
    lb_recv := Bool(false)
    lb_finished := Bool(true)
  }

  io.finished := bl_finished && lb_finished

  val bl_start_recv = Reg(next = bl_send_done)
  val lb_start_recv = Reg(next = lb_send_done)

  assert(!little2big.io.out.valid || little2big.io.out.bits === lb_recv_data,
    "Little to Big data mismatch")
  assert(!big2little.io.out.valid || big2little.io.out.bits === bl_recv_data,
    "Bit to Little data mismatch")

  assert(!lb_start_recv || little2big.io.count === UInt(4),
    "Little to Big count incorrect")
  assert(!bl_start_recv || big2little.io.count === UInt(8),
    "Big to Little count incorrect")
}

class TileLinkToSmiConverterTestDriver(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val mem = new ClientUncachedTileLinkIO
    val start = Bool(INPUT)
    val finished = Bool(OUTPUT)
  }

  val nChecks = 32
  val count = Reg(init = UInt(0, log2Up(nChecks)))
  val addr = Cat(count, UInt(0, 2))
  val data = Fill(4, count)

  val (s_idle :: s_wreq :: s_wresp :: s_rreq :: s_rresp ::
       s_finished :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  when (state === s_idle && io.start) { state := s_wreq }
  when (state === s_wreq && io.mem.acquire.ready) { state := s_wresp }
  when (state === s_wresp && io.mem.grant.valid) {
    count := count + UInt(1)
    when (count === UInt(nChecks - 1)) {
      state := s_rreq
    } .otherwise {
      state := s_wreq
    }
  }

  when (state === s_rreq && io.mem.acquire.ready) { state := s_rresp }
  when (state === s_rresp && io.mem.grant.valid) {
    count := count + UInt(1)
    when (count === UInt(nChecks - 1)) {
      state := s_finished
    } .otherwise {
      state := s_rreq
    }
  }

  val blockOffsetBits = p(CacheBlockOffsetBits)
  val byteAddrBits = log2Up(p(TLKey(p(TLId))).writeMaskBits)

  io.mem.acquire.valid := (state === s_wreq) || (state === s_rreq)
  io.mem.acquire.bits := Mux(state === s_wreq,
    Put(
      client_xact_id = UInt(0),
      addr_block = addr >> UInt(blockOffsetBits),
      addr_beat = addr(blockOffsetBits - 1, byteAddrBits),
      data = Mux(count(0), data << UInt(32), data),
      wmask = FillInterleaved(4, UIntToOH(count(0)))),
    Get(
      client_xact_id = UInt(0),
      addr_block = addr >> UInt(blockOffsetBits),
      addr_beat = addr(blockOffsetBits - 1, byteAddrBits),
      addr_byte = addr(byteAddrBits - 1, 0),
      operand_size = MT_W,
      alloc = Bool(false)))
  io.mem.grant.ready := (state === s_wresp) || (state === s_rresp)

  assert(!io.mem.grant.valid || !io.mem.grant.bits.hasData() ||
    Mux(count(0),
      io.mem.grant.bits.data(63, 32) === data,
      io.mem.grant.bits.data(31, 0) === data),
    "Test Driver got incorrect data")

  io.finished := (state === s_finished)
}

class TileLinkToSmiConverterTest(implicit p: Parameters) extends UnitTest {
  val outermostParams = p.alterPartial({ case TLId => "Outermost" })

  val smimem = Module(new SmiMem(32, 64))
  val conv = Module(new SmiIOTileLinkIOConverter(32, 6)(outermostParams))
  val driver = Module(new TileLinkToSmiConverterTestDriver()(outermostParams))

  conv.io.tl <> driver.io.mem
  smimem.io <> conv.io.smi
  driver.io.start := io.start
  io.finished := driver.io.finished
}

class AtosConverterTestFrontend(implicit p: Parameters) extends NastiModule()(p) {
  val io = new Bundle {
    val nasti = new NastiIO
    val finished = Bool(OUTPUT)
  }

  val n_words = 4
  val test_data = Vec.tabulate(n_words) { i => UInt(i * 48) }

  val (s_idle :: s_waddr :: s_wdata :: s_wresp ::
       s_raddr :: s_rresp :: s_done :: Nil) = Enum(Bits(), 7)
  val state = Reg(init = s_idle)

  when (state === s_idle) { state := s_waddr }
  when (io.nasti.aw.fire()) { state := s_wdata }
  when (io.nasti.w.fire() && io.nasti.w.bits.last) { state := s_wresp }
  when (io.nasti.b.fire()) { state := s_raddr }
  when (io.nasti.ar.fire()) { state := s_rresp }
  when (io.nasti.r.fire() && io.nasti.r.bits.last) { state := s_done }

  val (w_count, w_last) = Counter(io.nasti.w.fire(), n_words)

  io.nasti.aw.valid := (state === s_waddr)
  io.nasti.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = UInt(0),
    size = UInt(log2Up(nastiXDataBits / 8)),
    len = UInt(n_words - 1))

  io.nasti.w.valid := (state === s_wdata)
  io.nasti.w.bits := NastiWriteDataChannel(
    data = test_data(w_count),
    last = w_count === UInt(n_words - 1))

  io.nasti.ar.valid := (state === s_raddr)
  io.nasti.ar.bits := NastiReadAddressChannel(
    id = UInt(0),
    addr = UInt(0),
    size = UInt(log2Up(nastiXDataBits / 8)),
    len = UInt(n_words - 1))

  io.nasti.b.ready := (state === s_wresp)
  io.nasti.r.ready := (state === s_rresp)

  io.finished := (state === s_done)

  val (r_count, r_last) = Counter(io.nasti.r.fire(), n_words)

  assert(!io.nasti.r.valid || io.nasti.r.bits.data === test_data(r_count),
    "AtosConverterTest: returned data doesn't match expected")
}

class AtosConverterTestBackend(implicit p: Parameters) extends NastiModule()(p) {
  val io = new Bundle {
    val nasti = (new NastiIO).flip
    val finished = Bool(OUTPUT)
  }

  val (s_waddr :: s_wdata :: s_wresp ::
       s_raddr :: s_rresp :: s_done :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_waddr)

  val n_words = 4
  val test_data = Reg(Vec(n_words, UInt(width = nastiXDataBits)))
  val req_id = Reg(UInt(width = nastiXIdBits))

  val (w_count, w_last) = Counter(io.nasti.w.fire(), n_words)
  val (r_count, r_last) = Counter(io.nasti.r.fire(), n_words)

  when (io.nasti.aw.fire()) {
    req_id := io.nasti.aw.bits.id
    state := s_wdata
  }
  when (io.nasti.w.fire()) {
    test_data(w_count) := io.nasti.w.bits.data
    when (io.nasti.w.bits.last) { state := s_wresp }
  }
  when (io.nasti.b.fire()) { state := s_raddr }
  when (io.nasti.ar.fire()) {
    req_id := io.nasti.ar.bits.id
    state := s_rresp
  }
  when (io.nasti.r.fire() && io.nasti.r.bits.last) { state := s_done }

  io.nasti.aw.ready := (state === s_waddr)
  io.nasti.w.ready := (state === s_wdata)
  io.nasti.ar.ready := (state === s_raddr)

  io.nasti.b.valid := (state === s_wresp)
  io.nasti.b.bits := NastiWriteResponseChannel(id = req_id)

  io.nasti.r.valid := (state === s_rresp)
  io.nasti.r.bits := NastiReadDataChannel(
    id = req_id,
    data = test_data(r_count),
    last = r_last)

  io.finished := (state === s_done)
}

class AtosConverterTest(implicit p: Parameters) extends UnitTest {
  val frontend = Module(new AtosConverterTestFrontend)
  val backend = Module(new AtosConverterTestBackend)

  val serdes = Module(new AtosSerdes(8))
  val desser = Module(new AtosDesser(8))

  val client_conv = Module(new AtosClientConverter)
  val manager_conv = Module(new AtosManagerConverter)

  client_conv.io.nasti <> frontend.io.nasti
  serdes.io.wide <> client_conv.io.atos
  desser.io.narrow <> serdes.io.narrow
  manager_conv.io.atos <> desser.io.wide
  backend.io.nasti <> manager_conv.io.nasti

  io.finished := frontend.io.finished && backend.io.finished
}

class UnitTestSuite(implicit p: Parameters) extends GroundTest()(p) {
  val tests = Seq(
    Module(new MultiWidthFifoTest),
    Module(new NastiIOHostIOConverterTest),
    Module(new TileLinkToSmiConverterTest),
    Module(new AtosConverterTest),
    Module(new NastiMemoryDemuxTest),
    Module(new HastiTest))

  val s_idle :: s_start :: s_wait :: Nil = Enum(Bits(), 3)
  val state = Reg(init = s_idle)

  when (state === s_idle) { state := s_start }
  when (state === s_start) { state := s_wait }

  tests.foreach { mod => mod.io.start := (state === s_start) }
  io.finished := tests.map(_.io.finished).reduce(_ && _)
}
