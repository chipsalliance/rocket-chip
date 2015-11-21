package groundtest

import Chisel._
import uncore._
import junctions._
import rocket._
import scala.util.Random
import cde.{Parameters, Field}

case object NGenerators extends Field[Int]
case object GenerateUncached extends Field[Boolean]
case object GenerateCached extends Field[Boolean]
case object MaxGenerateRequests extends Field[Int]
case object GeneratorStartAddress extends Field[Int]

trait HasGeneratorParams {
  implicit val p: Parameters
  val nGens = p(NGenerators)
  val genUncached = p(GenerateUncached)
  val genCached = p(GenerateCached)
  val genTimeout = 4096
  val maxRequests = p(MaxGenerateRequests)
  val startAddress = p(GeneratorStartAddress)
  val genWordBits = p(WordBits)
  val genWordBytes = genWordBits / 8
  val wordOffset = log2Up(genWordBytes)

  require(startAddress % genWordBytes == 0)
}

class UncachedTileLinkGenerator(id: Int)
    (implicit p: Parameters) extends TLModule()(p) with HasGeneratorParams {

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits

  val io = new Bundle {
    val mem = new ClientUncachedTileLinkIO
    val finished = Bool(OUTPUT)
  }

  val (s_start :: s_put :: s_get :: s_finished :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_start)

  val (req_cnt, req_wrap) = Counter(io.mem.grant.fire(), maxRequests)

  val sending = Reg(init = Bool(false))

  when (state === s_start) {
    sending := Bool(true)
    state := s_put
  }

  when (io.mem.acquire.fire()) { sending := Bool(false) }
  when (io.mem.grant.fire()) { sending := Bool(true) }
  when (req_wrap) { state := Mux(state === s_put, s_get, s_finished) }

  val timeout = Timer(genTimeout, io.mem.acquire.fire(), io.mem.grant.fire())
  assert(!timeout, s"Uncached generator ${id} timed out waiting for grant")

  io.finished := (state === s_finished)

  val full_addr = UInt(startAddress) + Cat(
    req_cnt, UInt(id, log2Ceil(nGens)),
    (if (genCached) UInt(0, 1) else UInt(0, 0)),
    UInt(0, wordOffset))

  val addr_block = full_addr >> UInt(tlBlockOffset)
  val addr_beat = full_addr(tlBlockOffset - 1, tlByteAddrBits)
  val addr_byte = full_addr(tlByteAddrBits - 1, 0)

  val data_prefix = Cat(UInt(id, log2Up(nGens)), req_cnt)
  val word_data = Wire(UInt(width = genWordBits))
  word_data := Cat(data_prefix, full_addr)
  val beat_data = Fill(tlDataBits / genWordBits, word_data)
  val wshift = Cat(full_addr(tlByteAddrBits - 1, wordOffset), UInt(0, wordOffset))
  val wmask = Fill(genWordBits / 8, Bits(1, 1)) << wshift

  val put_acquire = Put(
    client_xact_id = UInt(0),
    addr_block = addr_block,
    addr_beat = addr_beat,
    data = beat_data,
    wmask = Some(wmask),
    alloc = Bool(false))

  val get_acquire = Get(
    client_xact_id = UInt(0),
    addr_block = addr_block,
    addr_beat = addr_beat,
    addr_byte = addr_byte,
    operand_size = MT_D,
    alloc = Bool(false))

  io.mem.acquire.valid := sending && !io.finished
  io.mem.acquire.bits := Mux(state === s_put, put_acquire, get_acquire)
  io.mem.grant.ready := !sending && !io.finished

  def wordFromBeat(addr: UInt, dat: UInt) = {
    val offset = addr(tlByteAddrBits - 1, wordOffset)
    val shift = Cat(offset, UInt(0, wordOffset + 3))
    (dat >> shift)(genWordBits - 1, 0)
  }

  assert(!io.mem.grant.valid || state =/= s_get ||
    wordFromBeat(full_addr, io.mem.grant.bits.data) === word_data,
    s"Get received incorrect data in uncached generator ${id}")
}

class HellaCacheGenerator(id: Int)
    (implicit p: Parameters) extends L1HellaCacheModule()(p) with HasGeneratorParams {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
    val mem = new HellaCacheIO
  }

  val timeout = Timer(genTimeout, io.mem.req.fire(), io.mem.resp.valid)
  assert(!timeout, s"Cached generator ${id} timed out waiting for response")

  val (s_start :: s_write :: s_read :: s_finished :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_start)
  val sending = Reg(init = Bool(false))

  val (req_cnt, req_wrap) = Counter(io.mem.resp.valid, maxRequests)

  val req_addr = UInt(startAddress) + Cat(
    req_cnt, UInt(id, log2Ceil(nGens)),
    (if (genUncached) UInt(1, 1) else UInt(0, 0)),
    UInt(0, wordOffset))
  val req_data = Cat(UInt(id, log2Up(nGens)), req_cnt, req_addr)

  io.mem.req.valid := sending && !io.finished
  io.mem.req.bits.addr := req_addr
  io.mem.req.bits.data := req_data
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.cmd  := Mux(state === s_write, M_XWR, M_XRD)
  io.mem.req.bits.tag  := UInt(0)
  io.mem.req.bits.kill := Bool(false)
  io.mem.req.bits.phys := Bool(true)

  when (state === s_start) { sending := Bool(true); state := s_write }

  when (io.mem.req.fire()) { sending := Bool(false) }
  when (io.mem.resp.valid) { sending := Bool(true) }

  when (req_wrap) { state := Mux(state === s_write, s_read, s_finished) }

  io.finished := (state === s_finished)

  assert(!io.mem.resp.valid || !io.mem.resp.bits.has_data ||
    io.mem.resp.bits.data === req_data,
    s"Received incorrect data in cached generator ${id}")
}

class GeneratorTest(id: Int)(implicit val p: Parameters)
    extends GroundTest()(p) with HasGeneratorParams {

  val gen_finished = Wire(Vec(2, Bool()))

  if (genUncached) {
    val uncacheGen = Module(new UncachedTileLinkGenerator(id))
    io.mem <> uncacheGen.io.mem
    gen_finished(0) := uncacheGen.io.finished
  } else {
    io.mem.acquire.valid := Bool(false)
    io.mem.grant.ready := Bool(false)
    gen_finished(0) := Bool(true)
  }

  if (genCached) {
    val cacheGen = Module(new HellaCacheGenerator(id))
    io.cache <> cacheGen.io.mem
    gen_finished(1) := cacheGen.io.finished
  } else {
    io.cache.req.valid := Bool(false)
    gen_finished(1) := Bool(true)
  }

  io.finished := gen_finished.reduce(_ && _)
}
