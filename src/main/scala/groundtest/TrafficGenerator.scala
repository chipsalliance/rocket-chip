// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package groundtest

import Chisel._
import uncore.tilelink._
import uncore.devices.NTiles
import uncore.constants._
import junctions._
import rocket._
import util.SimpleTimer
import scala.util.Random
import config._

case class TrafficGeneratorParameters(
  maxRequests: Int,
  startAddress: BigInt)
case object GeneratorKey extends Field[TrafficGeneratorParameters]

trait HasTrafficGeneratorParameters extends HasGroundTestParameters {
  implicit val p: Parameters

  val genParams = p(GeneratorKey)
  val nGens = p(GroundTestKey).map(
    cs => cs.uncached + cs.cached).reduce(_ + _)
  val genTimeout = 8192
  val maxRequests = genParams.maxRequests
  val startAddress = genParams.startAddress

  val genWordBits = 32
  val genWordBytes = genWordBits / 8
  val wordOffset = log2Ceil(genWordBytes)
  val wordSize = UInt(log2Ceil(genWordBytes))

  require(startAddress % BigInt(genWordBytes) == 0)
}

class UncachedTileLinkGenerator(id: Int)
    (implicit p: Parameters) extends TLModule()(p) with HasTrafficGeneratorParameters {

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits

  val io = new Bundle {
    val mem = new ClientUncachedTileLinkIO
    val status = new GroundTestStatus
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

  val timeout = SimpleTimer(genTimeout, io.mem.acquire.fire(), io.mem.grant.fire())
  assert(!timeout, s"Uncached generator ${id} timed out waiting for grant")

  io.status.finished := (state === s_finished)
  io.status.timeout.valid := timeout
  io.status.timeout.bits := UInt(id)

  val part_of_full_addr =
    if (log2Ceil(nGens) > 0) {
      Cat(UInt(id, log2Ceil(nGens)),
          UInt(0, wordOffset))
    } else {
      UInt(0, wordOffset)
    }
  val full_addr = UInt(startAddress) + Cat(req_cnt, part_of_full_addr)

  val addr_block = full_addr >> UInt(tlBlockOffset)
  val addr_beat = full_addr(tlBlockOffset - 1, tlByteAddrBits)
  val addr_byte = full_addr(tlByteAddrBits - 1, 0)

  val data_prefix = Cat(UInt(id, log2Up(nGens)), req_cnt)
  val word_data = Wire(UInt(width = genWordBits))
  word_data := Cat(data_prefix, part_of_full_addr)
  val beat_data = Fill(tlDataBits / genWordBits, word_data)
  val wshift = Cat(beatOffset(full_addr), UInt(0, wordOffset))
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
    operand_size = wordSize,
    alloc = Bool(false))

  io.mem.acquire.valid := sending && !io.status.finished
  io.mem.acquire.bits := Mux(state === s_put, put_acquire, get_acquire)
  io.mem.grant.ready := !sending && !io.status.finished

  def wordFromBeat(addr: UInt, dat: UInt) = {
    val shift = Cat(beatOffset(addr), UInt(0, wordOffset + 3))
    (dat >> shift)(genWordBits - 1, 0)
  }

  val data_mismatch = io.mem.grant.fire() && state === s_get &&
    wordFromBeat(full_addr, io.mem.grant.bits.data) =/= word_data

  io.status.error.valid := data_mismatch
  io.status.error.bits := UInt(id)

  assert(!data_mismatch,
    s"Get received incorrect data in uncached generator ${id}")

  def beatOffset(addr: UInt) = // TODO zero-width
    if (tlByteAddrBits > wordOffset) addr(tlByteAddrBits - 1, wordOffset)
    else UInt(0)
}

class HellaCacheGenerator(id: Int)
    (implicit p: Parameters) extends L1HellaCacheModule()(p) with HasTrafficGeneratorParameters {
  val io = new Bundle {
    val mem = new HellaCacheIO
    val status = new GroundTestStatus
  }

  val timeout = SimpleTimer(genTimeout, io.mem.req.fire(), io.mem.resp.valid)
  assert(!timeout, s"Cached generator ${id} timed out waiting for response")
  io.status.timeout.valid := timeout
  io.status.timeout.bits := UInt(id)

  val (s_start :: s_write :: s_read :: s_finished :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_start)
  val sending = Reg(init = Bool(false))

  val (req_cnt, req_wrap) = Counter(io.mem.resp.valid, maxRequests)

  val part_of_req_addr =
    if (log2Ceil(nGens) > 0) {
      Cat(UInt(id, log2Ceil(nGens)),
          UInt(0, wordOffset))
    } else {
      UInt(0, wordOffset)
    }
  val req_addr = UInt(startAddress) + Cat(req_cnt, part_of_req_addr)
  val req_data = Cat(UInt(id, log2Up(nGens)), req_cnt, part_of_req_addr)

  io.mem.req.valid := sending && !io.status.finished
  io.mem.req.bits.addr := req_addr
  io.mem.req.bits.data := req_data
  io.mem.req.bits.typ  := wordSize
  io.mem.req.bits.cmd  := Mux(state === s_write, M_XWR, M_XRD)
  io.mem.req.bits.tag  := UInt(0)

  when (state === s_start) { sending := Bool(true); state := s_write }

  when (io.mem.req.fire()) { sending := Bool(false) }
  when (io.mem.resp.valid) { sending := Bool(true) }

  when (req_wrap) { state := Mux(state === s_write, s_read, s_finished) }

  io.status.finished := (state === s_finished)

  def data_match(recv: Bits, expected: Bits): Bool = {
    val recv_resized = Wire(Bits(width = genWordBits))
    val exp_resized = Wire(Bits(width = genWordBits))

    recv_resized := recv
    exp_resized := expected
    recv_resized === exp_resized
  }

  val data_mismatch = io.mem.resp.valid && io.mem.resp.bits.has_data &&
    !data_match(io.mem.resp.bits.data, req_data)

  io.status.error.valid := data_mismatch
  io.status.error.bits := UInt(id)

  assert(!data_mismatch,
    s"Received incorrect data in cached generator ${id}")
}

class GeneratorTest(implicit p: Parameters)
    extends GroundTest()(p) with HasTrafficGeneratorParameters {

  val idStart = p(GroundTestKey).take(p(TileId))
    .map(settings => settings.cached + settings.uncached)
    .foldLeft(0)(_ + _)

  val cached = List.tabulate(nCached) { i =>
    val realId = idStart + i
    Module(new HellaCacheGenerator(realId))
  }

  val uncached = List.tabulate(nUncached) { i =>
    val realId = idStart + nCached + i
    Module(new UncachedTileLinkGenerator(realId))
  }

  io.cache <> cached.map(_.io.mem)
  io.mem <> uncached.map(_.io.mem)

  val gen_debug = cached.map(_.io.status) ++ uncached.map(_.io.status)
  io.status := DebugCombiner(gen_debug)
}
