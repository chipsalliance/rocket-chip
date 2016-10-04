package uncore.tilelink

import Chisel._
import junctions._
import uncore.constants._
import uncore.util._
import util._
import cde.Parameters

abstract class Driver(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val mem = new ClientUncachedTileLinkIO
    val start = Bool(INPUT)
    val finished = Bool(OUTPUT)
  }
}

/**
 * Tests that single-beat Gets of decreasing size return subsets of the
 * data returned by larger Gets
 */
class GetMultiWidthDriver(implicit p: Parameters) extends Driver()(p) {
  val s_start :: s_send :: s_recv :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_start)

  val w = 64
  val initialSize = UInt(log2Ceil(w/8))
  val size = Reg(UInt(width = log2Ceil(log2Ceil(w/8)+1)))
  val ref = Reg(UInt(width = w))
  val bytemask = (UInt(1) << (UInt(1) << size)) - UInt(1)
  val bitmask = FillInterleaved(8, bytemask)

  io.mem.acquire.valid := (state === s_send)
  io.mem.acquire.bits := Get(
    client_xact_id = UInt(0),
    addr_block = UInt(0),
    addr_beat = UInt(0),
    addr_byte = UInt(0),
    operand_size = size,
    alloc = Bool(false))
  io.mem.grant.ready := (state === s_recv)

  when (state === s_start && io.start) {
    size := initialSize
    state := s_send
  }

  when (io.mem.acquire.fire()) { state := s_recv }
  when (io.mem.grant.fire()) {
    when (size === initialSize) { ref := io.mem.grant.bits.data }
    size := size - UInt(1)
    state := Mux(size === UInt(0), s_done, s_send)
  }

  io.finished := state === s_done

  assert(!io.mem.grant.valid || size === initialSize ||
         (io.mem.grant.bits.data & bitmask) === (ref & bitmask),
         "GetMultiWidth: smaller get does not match larger get")
}

/**
 * Tests that single-beat Gets across a range of memory return
 * the expected data.
 * @param expected The values of the data expected to be read.
 *                 Each element is the data for one beat.
 */
class GetSweepDriver(expected: Seq[BigInt])
                    (implicit p: Parameters) extends Driver()(p) {

  val s_start :: s_send :: s_recv :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_start)

  val nReqs = expected.size
  val (req_cnt, req_done) = Counter(io.mem.grant.fire(), nReqs)

  when (state === s_start && io.start) { state := s_send }
  when (io.mem.acquire.fire()) { state := s_recv }
  when (io.mem.grant.fire()) { state := s_send }
  when (req_done) { state := s_done }

  val (addr_block, addr_beat) = if (nReqs > tlDataBeats) {
    (req_cnt(log2Up(nReqs) - 1, tlBeatAddrBits),
     req_cnt(tlBeatAddrBits - 1, 0))
  } else {
    (UInt(0), req_cnt)
  }

  val exp_data = Vec(expected.map(e => UInt(e, tlDataBits)))

  io.mem.acquire.valid := (state === s_send)
  io.mem.acquire.bits := Get(
    client_xact_id = UInt(0),
    addr_block = addr_block,
    addr_beat = addr_beat)
  io.mem.grant.ready := (state === s_recv)
  io.finished := state === s_done

  assert(!io.mem.grant.valid || io.mem.grant.bits.data === exp_data(req_cnt),
         "GetSweep: data does not match expected")
}

/**
 * Tests that multi-beat GetBlocks across a range of memory return
 * the expected data.
 * @param expected The values of the data expected to be read.
 *                 Each element is the data for one beat.
 */
class GetBlockSweepDriver(expected: Seq[BigInt])
                         (implicit p: Parameters) extends Driver()(p) {
  val s_start :: s_send :: s_recv :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_start)

  val nReqs = ((expected.size - 1) / tlDataBeats + 1) * tlDataBeats
  val (req_cnt, req_done) = Counter(io.mem.grant.fire(), nReqs)
  val (addr_beat, beats_done) = Counter(io.mem.grant.fire(), tlDataBeats)

  val tlBlockOffset = tlByteAddrBits + tlBeatAddrBits
  val addr_block =
    if (nReqs > tlDataBeats) req_cnt(log2Up(nReqs) - 1, tlBlockOffset)
    else UInt(0)

  io.mem.acquire.valid := (state === s_send)
  io.mem.acquire.bits := GetBlock(
    client_xact_id = UInt(0),
    addr_block = addr_block)
  io.mem.grant.ready := (state === s_recv)
  io.finished := state === s_done

  when (state === s_start && io.start) { state := s_send }
  when (io.mem.acquire.fire()) { state := s_recv }
  when (beats_done) { state := s_send }
  when (req_done) { state := s_done }

  val exp_data = Vec(expected.map(e => UInt(e, tlDataBits)))

  assert(!io.mem.grant.valid || req_cnt >= UInt(expected.size) ||
         io.mem.grant.bits.data === exp_data(req_cnt),
         "GetBlockSweep: data does not match expected")
}

/**
 * Tests that single-beat Puts across a range of memory persists correctly.
 * @param n the number of beats to put
 */
class PutSweepDriver(val n: Int)(implicit p: Parameters) extends Driver()(p) {
  val (s_idle :: s_put_req :: s_put_resp ::
       s_get_req :: s_get_resp :: s_done :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  val (put_cnt, put_done) = Counter(state === s_put_resp && io.mem.grant.valid, n)
  val (get_cnt, get_done) = Counter(state === s_get_resp && io.mem.grant.valid, n)

  val (put_block, put_beat) = if (n > tlDataBeats) {
    (put_cnt(log2Up(n) - 1, tlBeatAddrBits),
     put_cnt(tlBeatAddrBits - 1, 0))
  } else {
    (UInt(0), put_cnt)
  }
  val (get_block, get_beat) = if (n > tlDataBeats) {
    (get_cnt(log2Up(n) - 1, tlBeatAddrBits),
     get_cnt(tlBeatAddrBits - 1, 0))
  } else {
    (UInt(0), get_cnt)
  }

  val dataRep = (tlDataBits - 1) / log2Up(n) + 1
  val put_data = Fill(dataRep, put_cnt)(tlDataBits - 1, 0)
  val get_data = Fill(dataRep, get_cnt)(tlDataBits - 1, 0)

  io.mem.acquire.valid := state.isOneOf(s_put_req, s_get_req)
  io.mem.acquire.bits := Mux(state === s_put_req,
    Put(
      client_xact_id = UInt(0),
      addr_block = put_block,
      addr_beat = put_beat,
      data = put_data),
    Get(
      client_xact_id = UInt(0),
      addr_block = get_block,
      addr_beat = get_beat))
  io.mem.grant.ready := state.isOneOf(s_put_resp, s_get_resp)

  when (state === s_idle && io.start) { state := s_put_req }
  when (state === s_put_req && io.mem.acquire.ready) { state := s_put_resp }
  when (state === s_put_resp && io.mem.grant.valid) {
    state := Mux(put_done, s_get_req, s_put_req)
  }
  when (state === s_get_req && io.mem.acquire.ready) { state := s_get_resp }
  when (state === s_get_resp && io.mem.grant.valid) {
    state := Mux(get_done, s_done, s_get_req)
  }

  io.finished := (state === s_done)

  assert(!io.mem.grant.valid || !io.mem.grant.bits.hasData() ||
         io.mem.grant.bits.data === get_data,
         "PutSweepDriver: data does not match")
}

/**
 * Tests that write-masked single-beat puts work correctly by putting
 * data with steadily smaller write-masks to the same beat.
 * @param minBytes the smallest number of bytes that can be in the writemask
 */
class PutMaskDriver(minBytes: Int = 1)(implicit p: Parameters) extends Driver()(p) {
  val (s_idle :: s_put_req :: s_put_resp ::
       s_get_req :: s_get_resp :: s_done :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)
  val nbytes = Reg(UInt(width = log2Up(tlWriteMaskBits) + 1))
  val wmask = (UInt(1) << nbytes) - UInt(1)
  val wdata = Fill(tlDataBits / 8, Wire(UInt(width = 8), init = nbytes))
  // TL data bytes down to minBytes logarithmically by 2
  val expected = (log2Ceil(tlDataBits / 8) to log2Ceil(minBytes) by -1)
    .map(1 << _).foldLeft(UInt(0, tlDataBits)) {
      // Change the lower nbytes of the value
      (value, nbytes) => {
        val mask = UInt((BigInt(1) << (nbytes * 8)) - BigInt(1), tlDataBits)
        val wval = Fill(tlDataBits / 8, UInt(nbytes, 8))
        (value & ~mask) | (wval & mask)
      }
    }

  when (state === s_idle && io.start) {
    state := s_put_req
    nbytes := UInt(8)
  }
  when (state === s_put_req && io.mem.acquire.ready) {
    state := s_put_resp
  }
  when (state === s_put_resp && io.mem.grant.valid) {
    nbytes := nbytes >> UInt(1)
    state := Mux(nbytes === UInt(minBytes), s_get_req, s_put_req)
  }
  when (state === s_get_req && io.mem.acquire.ready) {
    state := s_get_resp
  }
  when (state === s_get_resp && io.mem.grant.valid) {
    state := s_done
  }

  io.finished := (state === s_done)
  io.mem.acquire.valid := state.isOneOf(s_put_req, s_get_req)
  io.mem.acquire.bits := Mux(state === s_put_req,
    Put(
      client_xact_id = UInt(0),
      addr_block = UInt(0),
      addr_beat = UInt(0),
      data = wdata,
      wmask = Some(wmask)),
    Get(
      client_xact_id = UInt(0),
      addr_block = UInt(0),
      addr_beat = UInt(0)))
  io.mem.grant.ready := state.isOneOf(s_put_resp, s_get_resp)

  assert(!io.mem.grant.valid || state =/= s_get_resp ||
         io.mem.grant.bits.data === expected,
         "PutMask: data does not match expected")
}

class PutBlockSweepDriver(val n: Int)(implicit p: Parameters)
    extends Driver()(p) {
  val (s_idle :: s_put_req :: s_put_resp ::
       s_get_req :: s_get_resp :: s_done :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  val (put_beat, put_beat_done) = Counter(
    state === s_put_req && io.mem.acquire.ready, tlDataBeats)
  val (put_cnt, put_done) = Counter(
    state === s_put_resp && io.mem.grant.valid, n)
  val (get_beat, get_beat_done) = Counter(
    state === s_get_resp && io.mem.grant.valid, tlDataBeats)
  val (get_cnt, get_done) = Counter(get_beat_done, n)

  val dataRep = (tlDataBits - 1) / (log2Up(n) + tlBeatAddrBits) + 1
  val put_data = Fill(dataRep, Cat(put_cnt, put_beat))(tlDataBits - 1, 0)
  val get_data = Fill(dataRep, Cat(get_cnt, get_beat))(tlDataBits - 1, 0)

  when (state === s_idle && io.start) { state := s_put_req }
  when (put_beat_done) { state := s_put_resp }
  when (state === s_put_resp && io.mem.grant.valid) {
    state := Mux(put_done, s_get_req, s_put_req)
  }
  when (state === s_get_req && io.mem.acquire.ready) { state := s_get_resp }
  when (get_beat_done) { state := Mux(get_done, s_done, s_get_req) }

  val put_acquire = PutBlock(
    client_xact_id = UInt(0),
    addr_block = put_cnt,
    addr_beat = put_beat,
    data = put_data)

  val get_acquire = GetBlock(
    client_xact_id = UInt(0),
    addr_block = get_cnt)

  io.finished := (state === s_done)
  io.mem.acquire.valid := state.isOneOf(s_put_req, s_get_req)
  io.mem.acquire.bits := Mux(state === s_put_req, put_acquire, get_acquire)
  io.mem.grant.ready := state.isOneOf(s_put_resp, s_get_resp)

  assert(!io.mem.grant.valid || state =/= s_get_resp ||
         io.mem.grant.bits.data === get_data,
         "PutBlockSweep: data does not match expected")
}

class PutAtomicDriver(implicit p: Parameters) extends Driver()(p) {
  val s_idle :: s_put :: s_atomic :: s_get :: s_done :: Nil = Enum(Bits(), 5)
  val state = Reg(init = s_idle)
  val sending = Reg(init = Bool(false))

  val put_acquire = Put(
    client_xact_id = UInt(0),
    addr_block = UInt(0),
    addr_beat = UInt(0),
    // Put 15 in bytes 7:4
    data = UInt(15L << 32),
    wmask = Some(UInt(0xf0)))

  val amo_acquire = PutAtomic(
    client_xact_id = UInt(0),
    addr_block = UInt(0),
    addr_beat = UInt(0),
    addr_byte = UInt(4),
    atomic_opcode = M_XA_ADD,
    operand_size = UInt(log2Ceil(32 / 8)),
    data = UInt(3L << 32))

  val get_acquire = Get(
    client_xact_id = UInt(0),
    addr_block = UInt(0),
    addr_beat = UInt(0))

  io.finished := (state === s_done)
  io.mem.acquire.valid := sending
  io.mem.acquire.bits := MuxLookup(state, get_acquire, Seq(
    s_put -> put_acquire,
    s_atomic -> amo_acquire,
    s_get -> get_acquire))
  io.mem.grant.ready := !sending

  when (io.mem.acquire.fire()) { sending := Bool(false) }

  when (state === s_idle && io.start) {
    state := s_put
    sending := Bool(true)
  }
  when (io.mem.grant.fire()) {
    when (state === s_put) { sending := Bool(true); state := s_atomic }
    when (state === s_atomic) { sending := Bool(true); state := s_get }
    when (state === s_get) { state := s_done }
  }

  assert(!io.mem.grant.valid || state =/= s_get ||
         io.mem.grant.bits.data(63, 32) === UInt(18))
}

class PrefetchDriver(implicit p: Parameters) extends Driver()(p) {
  val s_idle :: s_put_pf :: s_get_pf :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)
  val sending = Reg(init = Bool(false))

  when (state === s_idle) {
    sending := Bool(true)
    state := s_put_pf
  }

  when (io.mem.acquire.fire()) { sending := Bool(false) }
  when (io.mem.grant.fire()) {
    when (state === s_put_pf) { sending := Bool(true); state := s_get_pf }
    when (state === s_get_pf) { state := s_done }
  }

  io.finished := (state === s_done)
  io.mem.acquire.valid := sending
  io.mem.acquire.bits := Mux(state === s_put_pf,
    PutPrefetch(
      client_xact_id = UInt(0),
      addr_block = UInt(0)),
    GetPrefetch(
      client_xact_id = UInt(0),
      addr_block = UInt(0)))
  io.mem.grant.ready := !sending
}

class DriverSet(driverGen: Parameters => Seq[Driver])(implicit p: Parameters)
    extends Driver()(p) {
  val s_start :: s_run :: s_done :: Nil = Enum(Bits(), 3)
  val state = Reg(init = s_start)

  val drivers = driverGen(p)
  val idx = Reg(init = UInt(0, log2Up(drivers.size)))
  val finished = Wire(init = Bool(false))

  when (state === s_start && io.start) { state := s_run }
  when (state === s_run && finished) {
    when (idx === UInt(drivers.size - 1)) { state := s_done }
    idx := idx + UInt(1)
  }

  io.finished := state === s_done

  io.mem.acquire.valid := Bool(false)
  io.mem.grant.ready := Bool(false)

  drivers.zipWithIndex.foreach { case (driv, i) =>
    val me = idx === UInt(i)

    driv.io.start := me && state === s_run
    driv.io.mem.acquire.ready := io.mem.acquire.ready && me
    driv.io.mem.grant.valid := io.mem.grant.valid && me
    driv.io.mem.grant.bits := io.mem.grant.bits

    when (me) {
      io.mem.acquire.valid := driv.io.mem.acquire.valid
      io.mem.acquire.bits := driv.io.mem.acquire.bits
      io.mem.grant.ready := driv.io.mem.grant.ready
      finished := driv.io.finished
    }
  }
}
