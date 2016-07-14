package groundtest.unittests

import Chisel._
import junctions._
import junctions.NastiConstants._
import cde.Parameters

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


