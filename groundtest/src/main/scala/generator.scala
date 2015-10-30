package groundtest

import Chisel._
import uncore._
import junctions._
import rocket._
import scala.util.Random
import cde.{Parameters, Field}

case object NGeneratorsPerTile extends Field[Int]
case object NGeneratorTiles extends Field[Int]

trait HasGeneratorParams {
  implicit val p: Parameters
  val nGensPerTile = p(NGeneratorsPerTile)
  val nGenTiles = p(NGeneratorTiles)
  val nGens = nGensPerTile * nGenTiles
}

class UncachedTileLinkGenerator(id: Int)
    (implicit p: Parameters) extends TLModule()(p) with HasGeneratorParams {

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits
  private val maxAddress = (p(MMIOBase) >> tlBlockOffset).toInt / 2
  private val totalRequests = maxAddress / nGens

  val io = new Bundle {
    val mem = new ClientUncachedTileLinkIO
    val finished = Bool(OUTPUT)
  }

  val (s_start :: s_put :: s_get :: s_finished :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_start)

  val (acq_beat, acq_done) = Counter(io.mem.acquire.fire() && state === s_put, tlDataBeats)
  val (gnt_beat, gnt_done) = Counter(io.mem.grant.fire() && state === s_get, tlDataBeats)
  val (req_cnt, req_wrap) = Counter(gnt_done && state === s_get, totalRequests)

  val addr_block = Cat(req_cnt, UInt(id, log2Up(nGens)))

  val sending = Reg(init = Bool(false))

  when (state === s_start) {
    sending := Bool(true)
    state := s_put
  }

  when (state === s_put) {
    when (acq_done) { sending := Bool(false) }
    when (io.mem.grant.fire()) { sending := Bool(true); state := s_get }
  }

  when (state === s_get) {
    when (io.mem.acquire.fire()) { sending := Bool(false) }
    when (gnt_done) {
      sending := Bool(true)
      state := Mux(req_wrap, s_finished, s_put)
    }
  }

  io.finished := (state === s_finished)

  val acq_addr = Cat(addr_block, acq_beat, UInt(0, tlByteAddrBits))
  val gnt_addr = Cat(addr_block, gnt_beat, UInt(0, tlByteAddrBits))
  val data_prefix = Cat(UInt(id, log2Up(nGens)), req_cnt)
  val put_data = Cat(data_prefix, acq_addr)
  val get_data = Cat(data_prefix, gnt_addr)

  val put_acquire = PutBlock(
    client_xact_id = UInt(0),
    addr_block = addr_block,
    addr_beat = acq_beat,
    data = put_data)

  val get_acquire = GetBlock(
    client_xact_id = UInt(0),
    addr_block = addr_block)

  io.mem.acquire.valid := sending
  io.mem.acquire.bits := Mux(state === s_put, put_acquire, get_acquire)
  io.mem.grant.ready := !sending

  assert(!io.mem.grant.valid || state != s_get ||
    io.mem.grant.bits.data === get_data,
    s"Get received incorrect data in uncached generator ${id}")
}

class HellaCacheGenerator(id: Int)
    (implicit p: Parameters) extends L1HellaCacheModule()(p) with HasGeneratorParams {

  private val wordOffset = log2Up(coreDataBits / 8)
  private val maxAddress = (p(MMIOBase) >> wordOffset).toInt
  private val startAddress = maxAddress / 2
  private val totalRequests = (maxAddress - startAddress) / nGens

  val io = new Bundle {
    val finished = Bool(OUTPUT)
    val mem = new HellaCacheIO
  }

  val (s_start :: s_write :: s_read :: s_finished :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_start)
  val sending = Reg(init = Bool(false))

  val (req_cnt, req_wrap) = Counter(
    io.mem.resp.valid && io.mem.resp.bits.has_data, totalRequests)

  val req_addr = UInt(startAddress) +
                 Cat(req_cnt, UInt(id, log2Up(nGens)), UInt(0, wordOffset))
  val req_data = Cat(UInt(id, log2Up(nGens)), req_cnt, req_addr)

  io.mem.req.valid := sending
  io.mem.req.bits.addr := req_addr
  io.mem.req.bits.data := req_data
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.cmd  := Mux(state === s_write, M_XWR, M_XRD)
  io.mem.req.bits.tag  := UInt(0)
  io.mem.req.bits.kill := Bool(false)
  io.mem.req.bits.phys := Bool(true)

  when (state === s_start) { sending := Bool(true); state := s_write }

  when (io.mem.req.fire()) { sending := Bool(false) }

  when (io.mem.resp.valid) {
    sending := Bool(true)
    state := Mux(state === s_write, s_read, s_write)
  }

  when (req_wrap) { sending := Bool(false); state := s_finished }

  assert(!io.mem.resp.valid || !io.mem.resp.bits.has_data ||
    io.mem.resp.bits.data === req_data,
    s"Received incorrect data in cached generator ${id}")
}
