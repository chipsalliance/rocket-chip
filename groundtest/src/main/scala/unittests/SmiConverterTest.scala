package groundtest.unittests

import Chisel._
import junctions._
import junctions.NastiConstants._
import uncore.tilelink._
import uncore.converters._
import uncore.constants._
import cde.Parameters

class SmiConverterTestDriver(implicit p: Parameters) extends Module {
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
      wmask = Some(FillInterleaved(4, UIntToOH(count(0))))),
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

class SmiConverterTest(implicit p: Parameters) extends UnitTest {
  val outermostParams = p.alterPartial({ case TLId => "Outermost" })

  val smimem = Module(new SmiMem(32, 64))
  val conv = Module(new SmiIOTileLinkIOConverter(32, 6)(outermostParams))
  val driver = Module(new SmiConverterTestDriver()(outermostParams))

  conv.io.tl <> driver.io.mem
  smimem.io <> conv.io.smi
  driver.io.start := io.start
  io.finished := driver.io.finished
}


