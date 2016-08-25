package groundtest

import Chisel._
import uncore.tilelink._
import uncore.converters._
import junctions._
import cde.Parameters

class NastiGenerator(id: Int)(implicit val p: Parameters) extends Module
    with HasNastiParameters
    with HasMIFParameters
    with HasAddrMapParameters
    with HasGeneratorParameters {

  val io = new Bundle {
    val status = new GroundTestStatus
    val mem = new NastiIO
  }

  val mifDataBytes = mifDataBits / 8

  val (s_start :: s_write_addr :: s_write_data ::
       s_read  :: s_wait :: s_finish :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_start)

  def ref_data(idx: UInt) = UInt(0x35abffcd, genWordBits) + (idx << UInt(3))

  val part_of_addr =
    if (log2Ceil(nGens) > 0) {
      Cat(UInt(id, log2Ceil(nGens)),
          UInt(0, wordOffset))
    } else {
      UInt(0, wordOffset)
    }

  val (write_idx, write_done) = Counter(io.mem.w.fire(), maxRequests)
  val write_addr = UInt(startAddress) + Cat(write_idx, part_of_addr)
  val write_data = Fill(mifDataBits / genWordBits, ref_data(write_idx))
  val write_align = write_addr(log2Up(mifDataBytes) - 1, 0)
  val write_mask = UInt((1 << genWordBytes) - 1, nastiWStrobeBits) << write_align

  val (read_idx, read_done) = Counter(io.mem.ar.fire(), maxRequests)
  val read_addr = UInt(startAddress) + Cat(read_idx, part_of_addr)

  io.mem.aw.valid := (state === s_write_addr)
  io.mem.aw.bits := NastiWriteAddressChannel(
    id = write_idx(nastiXIdBits - 1, 0),
    addr = write_addr,
    len = UInt(0),
    size = UInt(log2Ceil(genWordBytes)))

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
    size = UInt(log2Ceil(genWordBytes)))

  io.mem.r.ready := Bool(true)
  io.mem.b.ready := Bool(true)

  io.status.finished := (state === s_finish)

  val (read_resp_idx,  read_resp_done)  = Counter(io.mem.r.fire(), maxRequests)
  val read_resp_addr = UInt(startAddress) + Cat(read_resp_idx, part_of_addr)
  val read_offset = read_resp_addr(log2Up(nastiXDataBits / 8) - 1, 0)
  val read_shift = Cat(read_offset, UInt(0, 3))
  val read_data = (io.mem.r.bits.data >> read_shift)(genWordBits - 1, 0)

  val data_mismatch = io.mem.r.valid && read_data =/= ref_data(read_resp_idx)
  assert(!data_mismatch, "NASTI Test: results do not match")
  io.status.error.valid := data_mismatch
  io.status.error.bits := UInt(1)

  when (state === s_start) { state := s_write_addr }
  when (io.mem.aw.fire()) { state := s_write_data  }
  when (io.mem.w.fire()) { state := s_write_addr }
  when (write_done) { state := s_read }
  when (read_done) { state := s_wait }
  when (read_resp_done) { state := s_finish }

  val r_timer = Module(new Timer(1000, 2))
  r_timer.io.start.valid := io.mem.ar.fire()
  r_timer.io.start.bits := io.mem.ar.bits.id
  r_timer.io.stop.valid := io.mem.r.fire() && io.mem.r.bits.last
  r_timer.io.stop.bits := io.mem.r.bits.id
  assert(!r_timer.io.timeout.valid, "NASTI Read timed out")

  val w_timer = Module(new Timer(1000, 2))
  w_timer.io.start.valid := io.mem.aw.fire()
  w_timer.io.start.bits := io.mem.aw.bits.id
  w_timer.io.stop.valid := io.mem.b.fire()
  w_timer.io.stop.bits := io.mem.b.bits.id
  assert(!w_timer.io.timeout.valid, "NASTI Write timed out")

  io.status.timeout.valid := r_timer.io.timeout.valid || w_timer.io.timeout.valid
  io.status.timeout.bits := Mux(r_timer.io.timeout.valid, UInt(1), UInt(2))
}

class NastiConverterTest(implicit p: Parameters) extends GroundTest()(p)
    with HasNastiParameters {
  require(tileSettings.uncached == 1 && tileSettings.cached == 0)

  val genId = p(GroundTestKey).take(tileId)
    .map(settings => settings.cached + settings.uncached)
    .foldLeft(0)(_ + _)

  val test = Module(new NastiGenerator(genId))
  val converter = Module(new TileLinkIONastiIOConverter()(
    p.alterPartial { case TLId => "Outermost" }))

  converter.io.nasti <> test.io.mem
  TileLinkWidthAdapter(io.mem.head, converter.io.tl)
  io.status := test.io.status
}
