package groundtest

import Chisel._
import uncore.tilelink._
import uncore.converters._
import junctions._
import cde.Parameters

abstract class NastiTest(implicit val p: Parameters) extends Module
    with HasNastiParameters with HasMIFParameters with HasAddrMapParameters {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
    val mem = new NastiIO
  }
  val memStart = addrMap("mem").start
}

class NastiBlockTest(implicit p: Parameters) extends NastiTest()(p) {

  val (s_start :: s_write_req :: s_write_resp ::
       s_read_req :: s_read_resp :: s_finish :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_start)

  val addr_sent = Reg(init = Bool(false))
  val data_sent = Reg(init = UInt(0, mifDataBeats))
  val data_beats = Vec.tabulate(mifDataBeats) { i => UInt(i * 0x20) }

  val (r_count, r_done) = Counter(io.mem.r.fire(), mifDataBeats)
  val (w_count, w_done) = Counter(io.mem.w.fire(), mifDataBeats)

  io.mem.aw.valid := (state === s_write_req) && !addr_sent
  io.mem.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = UInt(memStart),
    len = UInt(mifDataBeats - 1),
    size = UInt(log2Up(mifDataBits / 8)))

  io.mem.w.valid := (state === s_write_req) && !data_sent.andR
  io.mem.w.bits := NastiWriteDataChannel(
    data = data_beats(w_count),
    last = w_count === UInt(mifDataBeats - 1))

  io.mem.b.ready := (state === s_write_resp)

  io.mem.ar.valid := (state === s_read_req)
  io.mem.ar.bits := NastiReadAddressChannel(
    id = UInt(1),
    addr = UInt(memStart),
    len = UInt(mifDataBeats - 1),
    size = UInt(log2Up(mifDataBits / 8)))

  io.mem.r.ready := (state === s_read_resp)

  when (state === s_start) { state := s_write_req }

  when (io.mem.aw.fire()) { addr_sent := Bool(true) }
  when (io.mem.w.fire()) { data_sent := data_sent | UIntToOH(w_count) }
  when (w_done) { state := s_write_resp }
  when (io.mem.b.fire()) { state := s_read_req }

  when (io.mem.ar.fire()) { state := s_read_resp }
  when (r_done) { state := s_finish }

  io.finished := (state === s_finish)

  assert(!io.mem.r.valid || io.mem.r.bits.data === data_beats(r_count),
    "NASTI Block Test: results do not match")

  val timeout = Timer(8192, state === s_start, io.finished)
  assert(!timeout, "NastiBlockTest timed out")
}

class NastiSmallTest(implicit p: Parameters) extends NastiTest()(p) {
  val mifDataBytes = mifDataBits / 8

  val (s_start :: s_write_addr :: s_write_data ::
       s_read  :: s_wait :: s_finish :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_start)

  val nTests = 8
  val ref_data = Vec.tabulate(nTests) { i => UInt(0x35abffcd + i, 32) }

  val (write_idx, write_done) = Counter(io.mem.w.fire(), nTests)
  val write_addr = UInt(memStart + 0x200) + Cat(write_idx, UInt(0, 2))
  val write_data = Fill(mifDataBits / 32, ref_data(write_idx))
  val write_align = write_addr(log2Up(mifDataBytes) - 1, 0)
  val write_mask = UInt("h0f") << write_align

  val (read_idx, read_done) = Counter(io.mem.ar.fire(), nTests)
  val read_addr = UInt(memStart + 0x200) + Cat(read_idx, UInt(0, 2))
  val (read_resp_idx,  read_resp_done)  = Counter(io.mem.r.fire(), nTests)

  io.mem.aw.valid := (state === s_write_addr)
  io.mem.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = write_addr,
    len = UInt(0),
    size = UInt("b010"))

  io.mem.w.valid := (state === s_write_data)
  io.mem.w.bits := NastiWriteDataChannel(
    data = write_data,
    strb = Some(write_mask),
    last = Bool(true))

  io.mem.ar.valid := (state === s_read)
  io.mem.ar.bits := NastiReadAddressChannel(
    id = UInt(0),
    addr = read_addr,
    len = UInt(0),
    size = UInt("b010"))

  io.mem.r.ready := Bool(true)
  io.mem.b.ready := Bool(true)

  when (state === s_start) { state := s_write_addr }
  when (io.mem.aw.fire()) { state := s_write_data  }
  when (io.mem.w.fire()) { state := s_write_addr }
  when (write_done) { state := s_read }
  when (read_done) { state := s_wait }
  when (read_resp_done) { state := s_finish }

  io.finished := (state === s_finish)

  val readShiftBits = log2Ceil(mifDataBits / 32)
  val read_shift = Cat(read_resp_idx(readShiftBits - 1, 0), UInt(0, 5))
  val read_data = (io.mem.r.bits.data >> read_shift)(31, 0)

  assert(!io.mem.r.valid || read_data === ref_data(read_resp_idx),
    "NASTI Small Test: results do not match")

  val timeout = Timer(8192, state === s_start, io.finished)
  assert(!timeout, "NastiSmallTest timed out")
}

class NastiSequencer(n: Int)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val in = Vec(n, new NastiIO).flip
    val out = new NastiIO()
    val finished = Vec(n, Bool(INPUT))
  }

  val selection = Reg(init = UInt(0, log2Up(n)))
  val all_finished = io.finished.reduce(_ && _)

  when (!all_finished && io.finished(selection)) {
    selection := selection + UInt(1)
  }

  io.out.ar.valid := io.in(selection).ar.valid
  io.out.ar.bits := io.in(selection).ar.bits

  io.out.aw.valid := io.in(selection).aw.valid
  io.out.aw.bits := io.in(selection).aw.bits

  io.out.w.valid := io.in(selection).w.valid
  io.out.w.bits := io.in(selection).w.bits

  io.out.b.ready := io.in(selection).b.ready

  io.out.r.ready := io.in(selection).r.ready

  for ((in, i) <- io.in.zipWithIndex) {
    val me = selection === UInt(i)
    in.ar.ready := io.out.ar.ready && me
    in.aw.ready := io.out.aw.ready && me
    in.w.ready  := io.out.w.ready && me
    in.b.valid  := io.out.b.valid && me
    in.b.bits   := io.out.b.bits
    in.r.valid  := io.out.r.valid && me
    in.r.bits   := io.out.r.bits
  }

  val r_timer = Module(new Timer(1000, 2))
  r_timer.io.start.valid := io.out.ar.fire()
  r_timer.io.start.bits := io.out.ar.bits.id
  r_timer.io.stop.valid := io.out.r.fire() && io.out.r.bits.last
  r_timer.io.stop.bits := io.out.r.bits.id
  assert(!r_timer.io.timeout, "NASTI Read timed out")

  val w_timer = Module(new Timer(1000, 2))
  w_timer.io.start.valid := io.out.aw.fire()
  w_timer.io.start.bits := io.out.aw.bits.id
  w_timer.io.stop.valid := io.out.b.fire()
  w_timer.io.stop.bits := io.out.b.bits.id
  assert(!w_timer.io.timeout, "NASTI Write timed out")
}

class NastiConverterTest(implicit p: Parameters) extends GroundTest()(p)
    with HasNastiParameters {
  val tests = Seq(Module(new NastiBlockTest), Module(new NastiSmallTest))

  val sequencer = Module(new NastiSequencer(tests.size))
  val converter = Module(new TileLinkIONastiIOConverter()(
    p.alterPartial { case TLId => "Outermost" }))

  sequencer.io.in <> tests.map(_.io.mem)
  sequencer.io.finished := tests.map(_.io.finished)
  converter.io.nasti <> sequencer.io.out
  TileLinkWidthAdapter(io.mem.head, converter.io.tl)

  io.finished := tests.map(_.io.finished).reduce(_ && _)
}
