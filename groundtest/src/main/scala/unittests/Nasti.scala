package groundtest.unittests

import Chisel._
import junctions._
import junctions.NastiConstants._
import groundtest.common._
import cde.Parameters

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

class NastiDriver(dataWidth: Int, burstLen: Int, nBursts: Int)
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
    s"NastiDriver got wrong data")

  val ar_timeout = Timer(1024, io.nasti.ar.fire(), io.nasti.r.fire())
  val aw_timeout = Timer(1024, io.nasti.aw.fire(), io.nasti.b.fire())

  assert(!ar_timeout && !aw_timeout,
    s"NastiDriver for $name timed out")
}

class HastiTest(implicit p: Parameters) extends UnitTest {
  val sram = Module(new HastiTestSRAM(8))
  val bus = Module(new HastiBus(Seq(a => Bool(true))))
  val conv = Module(new HastiMasterIONastiIOConverter)
  val driver = Module(new NastiDriver(32, 8, 2))

  bus.io.slaves(0) <> sram.io
  bus.io.master <> conv.io.hasti
  conv.io.nasti <> driver.io.nasti
  io.finished := driver.io.finished
  driver.io.start := io.start
}

class AtosConverterTest(implicit val p: Parameters) extends UnitTest
    with HasNastiParameters {
  val frontend = Module(new NastiDriver(nastiXDataBits, 4, 1))
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


