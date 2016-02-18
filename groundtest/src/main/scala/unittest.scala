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

class UnitTestSuite(implicit p: Parameters) extends GroundTest()(p) {
  disablePorts()

  val tests = Seq(
    Module(new MultiWidthFifoTest),
    Module(new NastiIOHostIOConverterTest))

  val s_idle :: s_start :: s_wait :: Nil = Enum(Bits(), 3)
  val state = Reg(init = s_idle)

  when (state === s_idle) { state := s_start }
  when (state === s_start) { state := s_wait }

  tests.foreach { mod => mod.io.start := (state === s_start) }
  io.finished := tests.map(_.io.finished).reduce(_ && _)
}
