// See LICENSE.SiFive for license details.

package groundtest

import Chisel._
import uncore.tilelink._
import uncore.constants._
import junctions._
import rocket._
import util.Timer
import scala.util.Random
import config._
import util._

case class ComparatorParameters(
  targets:    Seq[Long], 
  width:      Int,
  operations: Int,
  atomics:    Boolean,
  prefetches: Boolean)
case object ComparatorKey extends Field[ComparatorParameters]

trait HasComparatorParameters {
  implicit val p: Parameters
  val comparatorParams = p(ComparatorKey)
  val targets     = comparatorParams.targets
  val nTargets    = targets.size
  val targetWidth = comparatorParams.width
  val nOperations = comparatorParams.operations
  val atomics     = comparatorParams.atomics
  val prefetches  = comparatorParams.prefetches
}

object LFSR64
{
  private var counter = 0
  private def next: Int = {
    counter += 1
    counter
  }
  
  def apply(increment: Bool = Bool(true), seed: Int = next): UInt =
  {
    val wide = 64
    val lfsr = RegInit(UInt((seed * 0xDEADBEEFCAFEBAB1L) >>> 1, width = wide))
    val xor = lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4)
    when (increment) { lfsr := Cat(xor, lfsr(wide-1,1)) }
    lfsr
  }
}

object NoiseMaker
{
  def apply(wide: Int, increment: Bool = Bool(true)): UInt = {
    val lfsrs = Seq.fill((wide+63)/64) { LFSR64(increment) }
    Cat(lfsrs)(wide-1,0)
  }
}

object MaskMaker
{
  def apply(wide: Int, bits: UInt): UInt = 
    Vec.tabulate(wide) {UInt(_) < bits} .asUInt
}

class ComparatorSource(implicit val p: Parameters) extends Module
    with HasComparatorParameters
    with HasTileLinkParameters
{
  val io = new Bundle {
    val out = Decoupled(new Acquire)
    val finished = Bool(OUTPUT)
  }
  
  // Output exactly nOperations of Acquires
  val finished = RegInit(Bool(false))
  val valid    = RegInit(Bool(false))
  
  valid := Bool(true)
  
  io.finished  := finished
  io.out.valid := !finished && valid
  
  // Generate random operand sizes
  val inc = io.out.fire()
  val raw_operand_size = NoiseMaker(2, inc) | UInt(0, M_SZ)
  val max_operand_size = UInt(log2Up(tlDataBytes))
  val get_operand_size = Mux(raw_operand_size > max_operand_size, max_operand_size, raw_operand_size)
  val atomic_operand_size = UInt(2) + NoiseMaker(1, inc) // word or dword
  
  // Generate random, but valid addr_bytes
  val raw_addr_byte = NoiseMaker(tlByteAddrBits, inc)
  val get_addr_byte    = raw_addr_byte & ~MaskMaker(tlByteAddrBits, get_operand_size)
  val atomic_addr_byte = raw_addr_byte & ~MaskMaker(tlByteAddrBits, atomic_operand_size)
  
  // Only allow some of the possible choices (M_XA_MAXU untested)
  val atomic_opcode = MuxLookup(NoiseMaker(3, inc), M_XA_SWAP, Array(
    UInt("b000") -> M_XA_ADD,
    UInt("b001") -> M_XA_XOR,
    UInt("b010") -> M_XA_OR,
    UInt("b011") -> M_XA_AND,
    UInt("b100") -> M_XA_MIN,
    UInt("b101") -> M_XA_MAX,
    UInt("b110") -> M_XA_MINU,
    UInt("b111") -> M_XA_SWAP))
  
  // Addr_block range
  val addr_block_mask = MaskMaker(tlBlockAddrBits, UInt(targetWidth-tlBeatAddrBits-tlByteAddrBits))
  
  // Generate some random values
  val addr_block = NoiseMaker(tlBlockAddrBits, inc) & addr_block_mask
  val addr_beat  = NoiseMaker(tlBeatAddrBits, inc)
  val wmask      = NoiseMaker(tlDataBytes, inc)
  val data       = NoiseMaker(tlDataBits, inc)
  val client_xact_id = UInt(0) // filled by Client
  
  // Random transactions
  val get         = Get(client_xact_id, addr_block, addr_beat, get_addr_byte, get_operand_size, Bool(false))
  val getBlock    = GetBlock(client_xact_id, addr_block)
  val put         = Put(client_xact_id, addr_block, addr_beat, data, Some(wmask))
  val putBlock    = PutBlock(client_xact_id, addr_block, UInt(0), data)
  val putAtomic   = if (atomics)
    PutAtomic(client_xact_id, addr_block, addr_beat,
      atomic_addr_byte, atomic_opcode, atomic_operand_size, data)
    else put
  val putPrefetch = if (prefetches)
    PutPrefetch(client_xact_id, addr_block)
    else put
  val getPrefetch = if (prefetches)
    GetPrefetch(client_xact_id, addr_block)
    else get
  val a_type_sel  = NoiseMaker(3, inc)

  // We must initially putBlock all of memory to have a consistent starting state
  val final_addr_block = addr_block_mask + UInt(1)
  val wipe_addr_block  = RegInit(UInt(0, width = tlBlockAddrBits))
  val done_wipe        = wipe_addr_block === final_addr_block

  io.out.bits := Mux(!done_wipe,
    // Override whatever else we were going to do if we are wiping
    PutBlock(client_xact_id, wipe_addr_block, UInt(0), data),
    // Generate a random a_type
    MuxLookup(a_type_sel, get, Array(
      UInt("b000") -> get,
      UInt("b001") -> getBlock,
      UInt("b010") -> put,
      UInt("b011") -> putBlock,
      UInt("b100") -> putAtomic,
      UInt("b101") -> getPrefetch,
      UInt("b110") -> putPrefetch)))
  
  val idx = Reg(init = UInt(0, log2Up(nOperations)))
  when (io.out.fire()) {
    when (idx === UInt(nOperations - 1)) { finished := Bool(true) }
    when (!done_wipe) {
      printf("[acq %d]: PutBlock(addr_block = %x, data = %x)\n",
        idx, wipe_addr_block, data)
      wipe_addr_block := wipe_addr_block + UInt(1)
    } .otherwise {
      switch (a_type_sel) {
        is (UInt("b000")) {
          printf("[acq %d]: Get(addr_block = %x, addr_beat = %x, addr_byte = %x, op_size = %x)\n",
            idx, addr_block, addr_beat, get_addr_byte, get_operand_size)
        }
        is (UInt("b001")) {
          printf("[acq %d]: GetBlock(addr_block = %x)\n", idx, addr_block)
        }
        is (UInt("b010")) {
          printf("[acq %d]: Put(addr_block = %x, addr_beat = %x, data = %x, wmask = %x)\n",
            idx, addr_block, addr_beat, data, wmask)
        }
        is (UInt("b011")) {
          printf("[acq %d]: PutBlock(addr_block = %x, data = %x)\n", idx, addr_block, data)
        }
        is (UInt("b100")) {
          if (atomics) {
            printf("[acq %d]: PutAtomic(addr_block = %x, addr_beat = %x, addr_byte = %x, " +
                   "opcode = %x, op_size = %x, data = %x)\n",
                   idx, addr_block, addr_beat, atomic_addr_byte,
                   atomic_opcode, atomic_operand_size, data)
          } else {
            printf("[acq %d]: Put(addr_block = %x, addr_beat = %x, data = %x, wmask = %x)\n",
              idx, addr_block, addr_beat, data, wmask)
          }
        }
        is (UInt("b101")) {
          if (prefetches) {
            printf("[acq %d]: GetPrefetch(addr_block = %x)\n", idx, addr_block)
          } else {
            printf("[acq %d]: Get(addr_block = %x, addr_beat = %x, addr_byte = %x, op_size = %x)\n",
              idx, addr_block, addr_beat, get_addr_byte, get_operand_size)
          }
        }
        is (UInt("b110")) {
          if (prefetches) {
            printf("[acq %d]: PutPrefetch(addr_block = %x)\n", idx, addr_block)
          } else {
            printf("[acq %d]: Put(addr_block = %x, addr_beat = %x, data = %x, wmask = %x)\n",
              idx, addr_block, addr_beat, data, wmask)
          }
        }
        is (UInt("b111")) {
          printf("[acq %d]: Get(addr_block = %x, addr_beat = %x, addr_byte = %x, op_size = %x)\n",
            idx, addr_block, addr_beat, get_addr_byte, get_operand_size)
        }
      }
    }
    idx := idx + UInt(1)
  }
}

class ComparatorClient(val target: Long)(implicit val p: Parameters) extends Module
    with HasComparatorParameters
    with HasTileLinkParameters
{
  val io = new Bundle {
    val in  = Decoupled(new Acquire).flip
    val tl  = new ClientUncachedTileLinkIO()
    val out = Decoupled(new Grant)
    val finished = Bool(OUTPUT)
    val timeout = Bool(OUTPUT)
  }

  val xacts = tlMaxClientXacts
  val offset = (UInt(target) >> UInt(tlBeatAddrBits+tlByteAddrBits))

  // Track the status of inflight requests
  val issued  = RegInit(Vec.fill(xacts) {Bool(false)})
  val ready   = RegInit(Vec.fill(xacts) {Bool(false)})
  val result  = Reg(Vec(xacts, new Grant))
  
  val buffer = Queue(io.in, xacts)
  val queue  = Module(new Queue(io.tl.acquire.bits.client_xact_id, xacts))
  
  val isMultiOut = buffer.bits.hasMultibeatData()
  val isMultiIn  = io.tl.grant.bits.hasMultibeatData()
  
  val beatOut  = RegInit(UInt(0, width = tlBeatAddrBits))
  val lastBeat = UInt(tlDataBeats-1)
  val isFirstBeatOut= Mux(isMultiOut, beatOut === UInt(0),  Bool(true))
  val isLastBeatOut = Mux(isMultiOut, beatOut === lastBeat, Bool(true))
  val isLastBeatIn  = Mux(isMultiIn,  io.tl.grant.bits.addr_beat === lastBeat, Bool(true))

  // Potentially issue a request, using a free xact id
  // NOTE: we may retract valid and change xact_id on a !ready (allowed by spec)
  val allow_acq = NoiseMaker(1)(0) && issued.map(!_).reduce(_ || _)
  val xact_id   = PriorityEncoder(issued.map(!_)) holdUnless isFirstBeatOut
  buffer.ready        := allow_acq && io.tl.acquire.ready && isLastBeatOut
  io.tl.acquire.valid := allow_acq && buffer.valid
  io.tl.acquire.bits  := buffer.bits
  io.tl.acquire.bits.addr_block := buffer.bits.addr_block + offset
  io.tl.acquire.bits.client_xact_id := xact_id
  when (isMultiOut) {
    val dataOut = (buffer.bits.data << beatOut) + buffer.bits.data // mix the data up a bit
    io.tl.acquire.bits.addr_beat := beatOut
    io.tl.acquire.bits.data := dataOut
  }
  
  when (io.tl.acquire.fire()) {
    issued(xact_id) := isLastBeatOut
    when (isMultiOut) { beatOut := beatOut + UInt(1) }
  }
  
  // Remember the xact ID so we can return results in-order
  queue.io.enq.valid := io.tl.acquire.fire() && isLastBeatOut
  queue.io.enq.bits  := xact_id
  assert (queue.io.enq.ready || !queue.io.enq.valid) // should be big enough
  
  // Capture the results from the manager
  io.tl.grant.ready := NoiseMaker(1)(0)
  when (io.tl.grant.fire()) {
    val id = io.tl.grant.bits.client_xact_id
    assert (!ready(id)) // got same xact_id twice?
    ready(id) := isLastBeatIn
    result(id) := io.tl.grant.bits
  }
  
  // Bad xact_id returned if ready but not issued!
  assert ((ready zip issued) map {case (r,i) => i || !r} reduce (_ && _))
  
  // When we have the next grant result, send it to the sink
  val next_id = queue.io.deq.bits
  queue.io.deq.ready := io.out.ready && ready(next_id) // TODO: only compares last getBlock
  io.out.valid := queue.io.deq.valid && ready(next_id)
  io.out.bits  := result(queue.io.deq.bits)
  
  when (io.out.fire()) {
    ready(next_id) := Bool(false)
    issued(next_id) := Bool(false)
  }
  
  io.finished := !buffer.valid && !issued.reduce(_ || _)

  val (idx, acq_done) = Counter(
    io.tl.acquire.fire() && io.tl.acquire.bits.last(), nOperations)
  debug(idx)

  val timer = Module(new Timer(8192, xacts))
  timer.io.start.valid := io.tl.acquire.fire() && io.tl.acquire.bits.first()
  timer.io.start.bits  := xact_id
  timer.io.stop.valid  := io.tl.grant.fire() && io.tl.grant.bits.first()
  timer.io.stop.bits   := io.tl.grant.bits.client_xact_id
  assert(!timer.io.timeout.valid, "Comparator TL client timed out")
  io.timeout := timer.io.timeout.valid
}

class ComparatorSink(implicit val p: Parameters) extends Module
    with HasComparatorParameters
    with HasTileLinkParameters
    with HasGroundTestConstants
{
  val io = new Bundle {
    val in = Vec(nTargets, Decoupled(new Grant)).flip
    val finished = Bool(OUTPUT)
    val error = Valid(UInt(width = errorCodeBits))
  }
  
  // could use a smaller Queue here, but would couple targets flow controls together
  val queues = io.in.map(Queue(_, nOperations))
  
  io.finished := queues.map(!_.valid).reduce(_ && _)
  val all_valid = queues.map(_.valid).reduce(_ && _)
  queues.foreach(_.ready := all_valid)
  
  val base = queues(0).bits
  val idx = Reg(init = UInt(0, log2Up(nOperations)))

  def check(g: Grant) = {
    when (g.hasData() && base.data =/= g.data) {
      printf("%d: %x =/= %x, g_type = %x\n", idx, base.data, g.data, g.g_type)
    }

    val assert_conds = Seq(
      g.is_builtin_type,
      base.g_type === g.g_type,
      base.addr_beat === g.addr_beat || !g.hasData(),
      base.data === g.data || !g.hasData())

    // TL1 likes to duplicate 32-bits into both halves of a 64-bit value
    // TL2 doesn't do this, so they compare differently when they are the same
    def isDupd(x: UInt) = if (tlDataBytes != 8) Bool(false) else x(31, 0) === x(63, 32)
    def safeCompare(x: UInt, y: UInt) =
      Mux(!isDupd(x) && !isDupd(y), x === y, x(63,32) === y(63,32) || x(31,0) === y(31,0))

    assert (g.is_builtin_type, "grant not builtin")
    assert (base.g_type === g.g_type, "g_type mismatch")
    assert (base.addr_beat === g.addr_beat || !g.hasMultibeatData(), "addr_beat mismatch")
    assert (base.data === g.data || !g.hasMultibeatData(), "multibeat data mismatch")
    assert (safeCompare(base.data, g.data) || !g.hasData(),  "singlebeat data mismatch")

    assert_conds.zipWithIndex.foreach { case (cond, i) =>
      when (!cond) {
        io.error.valid := Bool(true)
        io.error.bits := UInt(i)
      }
    }
  }
  when (all_valid) {
    when (base.hasData()) {
      printf("[gnt %d]: g_type = %x, addr_beat = %x, data = %x\n",
        idx, base.g_type, base.addr_beat, base.data)
    } .otherwise {
      printf("[gnt %d]: g_type = %x\n", idx, base.g_type)
    }
    queues.drop(1).map(_.bits).foreach(check)
    idx := idx + UInt(1)
  }
}

class ComparatorCore(implicit p: Parameters) extends GroundTest()(p)
    with HasComparatorParameters
    with HasTileLinkParameters {

  require (io.mem.size == nTargets)
  
  val source = Module(new ComparatorSource)
  val sink   = Module(new ComparatorSink)
  val broadcast = Broadcaster(source.io.out, nTargets)
  val clients = targets.zipWithIndex.map { case (target, index) =>
    val client = Module(new ComparatorClient(target))
    client.io.in <> broadcast(index)
    io.mem(index) <> client.io.tl
    sink.io.in(index) <> client.io.out
    client
  }
  val client_timeouts = clients.map(_.io.timeout)
  
  io.status.finished := source.io.finished && sink.io.finished && clients.map(_.io.finished).reduce(_ && _)
  io.status.timeout.valid := client_timeouts.reduce(_ || _)
  io.status.timeout.bits := MuxCase(UInt(0),
    client_timeouts.zipWithIndex.map {
      case (timeout, i) => (timeout -> UInt(i))
    })
  io.status.error := sink.io.error
}
