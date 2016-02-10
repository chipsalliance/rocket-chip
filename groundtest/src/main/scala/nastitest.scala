package groundtest

import Chisel._
import uncore._
import junctions._
import cde.Parameters

abstract class NastiTest(implicit val p: Parameters) extends Module
    with HasNastiParameters with HasMIFParameters {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
    val mem = new NastiIO
  }
}

class NastiBlockTest(implicit p: Parameters) extends NastiTest()(p) {

  val s_start :: s_write :: s_read :: s_wait :: s_finish :: Nil = Enum(Bits(), 5)
  val state = Reg(init = s_start)

  val addr_sent = Reg(init = Bool(false))
  val data_sent = Reg(init = UInt(0, mifDataBeats))
  val write_acked = Reg(init = Bool(false))
  val data_beats = Vec.tabulate(mifDataBeats) { i => UInt(i * 0x20) }

  val (r_count, r_done) = Counter(io.mem.r.fire(), mifDataBeats)
  val (w_count, w_done) = Counter(io.mem.w.fire(), mifDataBeats)

  io.mem.aw.valid := (state === s_write) && !addr_sent
  io.mem.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = UInt(0),
    len = UInt(mifDataBeats - 1),
    size = UInt(log2Up(mifDataBits / 8)))

  io.mem.w.valid := (state === s_write) && !data_sent.andR
  io.mem.w.bits := NastiWriteDataChannel(
    data = data_beats(w_count),
    last = w_count === UInt(mifDataBeats - 1))

  io.mem.ar.valid := (state === s_read)
  io.mem.ar.bits := NastiReadAddressChannel(
    id = UInt(1),
    addr = UInt(0),
    len = UInt(mifDataBeats - 1),
    size = UInt(log2Up(mifDataBits / 8)))

  io.mem.r.ready := (state === s_wait)
  io.mem.b.ready := Bool(true)

  when (state === s_start) { state := s_write }

  when (io.mem.aw.fire()) { addr_sent := Bool(true) }
  when (io.mem.w.fire()) { data_sent := data_sent | UIntToOH(w_count) }
  when (w_done) { state := s_read }
  when (io.mem.ar.fire()) { state := s_wait }
  when (r_done) { state := s_finish }

  io.finished := (state === s_finish)

  assert(!io.mem.r.valid || io.mem.r.bits.data === data_beats(r_count),
    "NASTI Block Test: results do not match")
}

class NastiSmallTest(implicit p: Parameters) extends NastiTest()(p) {

  val (s_start :: s_write_addr :: s_write_data ::
       s_read :: s_wait :: s_finish :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_start)

  val write_acked = Reg(init = Bool(false))
  val ref_data = UInt(0x35abffcd, mifDataBits)

  io.mem.aw.valid := (state === s_write_addr)
  io.mem.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = UInt(0x20C),
    len = UInt(0),
    size = UInt("b010"))

  io.mem.w.valid := (state === s_write_data)
  io.mem.w.bits := NastiWriteDataChannel(
    data = ref_data,
    last = Bool(true))

  io.mem.ar.valid := (state === s_read)
  io.mem.ar.bits := NastiReadAddressChannel(
    id = UInt(1),
    addr = UInt(0x20C),
    len = UInt(0),
    size = UInt("b010"))

  io.mem.r.ready := (state === s_wait)
  io.mem.b.ready := Bool(true)

  when (state === s_start) { state := s_write_addr }
  when (io.mem.aw.fire()) { state := s_write_data  }
  when (io.mem.w.fire()) { state := s_read }
  when (io.mem.ar.fire()) { state := s_wait }
  when (io.mem.r.fire()) { state := s_finish }

  io.finished := (state === s_finish)

  assert(!io.mem.r.valid || io.mem.r.bits.data === ref_data,
    "NASTI Small Test: results do not match")
}

class NastiConverterTest(implicit p: Parameters) extends GroundTest()(p)
    with HasNastiParameters {
  disablePorts(mem = false)

  val tests = Seq(Module(new NastiBlockTest), Module(new NastiSmallTest))

  val arbiter = Module(new NastiArbiter(tests.size))
  val converter = Module(new TileLinkIONastiIOConverter()(
    p.alterPartial { case TLId => "Outermost" }))
  val widener = Module(new TileLinkIOWidener("Outermost", "L1toL2"))

  arbiter.io.master <> tests.map(_.io.mem)
  converter.io.nasti <> arbiter.io.slave
  widener.io.in <> converter.io.tl
  io.mem <> widener.io.out

  io.finished := tests.map(_.io.finished).reduce(_ && _)
}
