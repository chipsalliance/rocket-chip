// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/////////////////////////////// Abstract API for the Queue /////////////////////////

class LanePositionedDecoupledIO[T <: Data](private val gen: T, val maxValid: Int, val maxReady: Int) extends Bundle {
  val validBits1 = log2Ceil(maxValid+1) // [0, maxValid]
  val readyBits1 = log2Ceil(maxReady+1) // [0, maxReady]
  val lanes = maxValid min maxReady // at most this many element flow per-cycle

  val ready = Input (UInt(readyBits1.W))
  val valid = Output(UInt(validBits1.W))
  val bits  = Output(Vec(lanes, gen))

  def cap_ready(l: Int = lanes) = if (l == maxReady) {
    ready
  } else {
    val lo = if (l==1) 0.U else ready(log2Ceil(l)-1,0)
    Mux(ready >= l.U, l.U, lo)
  }

  def cap_valid(l: Int = lanes) = if (l == maxValid) {
    valid
  } else {
    val lo = if (l==1) 0.U else valid(log2Ceil(l)-1,0)
    Mux(valid >= l.U, l.U, lo)
  }

  def min(x: UInt, y: UInt, lanes: Int) = {
    val xlo = if (lanes==1) 0.U else x(log2Ceil(lanes)-1, 0)
    val ylo = if (lanes==1) 0.U else y(log2Ceil(lanes)-1, 0)
    val xhi = x >= lanes.U
    val yhi = y >= lanes.U
    val xly = xlo <= ylo
    Mux(xhi && yhi, lanes.U, Mux(Mux(xly, !xhi, yhi), xlo, ylo))
  }

  def clamp_ready(x: UInt): Unit = { ready := min(x, valid, lanes) }
  def clamp_valid(x: UInt): Unit = { valid := min(x, ready, lanes) }

  // Feed from LPQ from another
  def driveWith(x: LanePositionedDecoupledIO[T], selfRotation: UInt = 0.U, xRotation: UInt = 0.U): Unit = {
    val limit = lanes min x.lanes
    val moved = min(ready, x.valid, limit)
    valid := moved
    x.ready := moved

    if (lanes == x.lanes) {
      bits := RotateVector.left(x.bits, selfRotation - xRotation)
    } else {
      val right = RotateVector.right(x.bits, xRotation)
      val padding = Wire(Vec(lanes, gen))
      val pad  = (right ++ padding).take(lanes)
      val left = RotateVector.left(pad, selfRotation)
      padding := DontCare
      bits := left
    }
  }
}

case class LanePositionedQueueArgs(
  lanes:  Int,
  rows:   Int,
  flow:   Boolean = false, // single cycle feedback from enq=>deq
  pipe:   Boolean = false, // single cycle feedback from deq=>enq
  free:   Boolean = false, // split deq pointer from free pointer + enable free IO
  commit: Boolean = false, // split enq pointer from commit pointer + enable commit IO
  rewind: Boolean = false, // enable the 'rewind' feature
  abort:  Boolean = false) // enable the 'abort' feature
{
  require (!rewind || free)
  require (!abort  || commit)
}

class LanePositionedQueueIO[T <: Data](private val gen: T, val args: LanePositionedQueueArgs) extends Bundle {
  val lanes = args.lanes
  val depth = args.rows * lanes
  val laneBitsU = log2Up(lanes)
  val depthBitsU = log2Up(depth)

  // enq.valid elements are enqueued; enq.valid must be <= enq.ready min lanes
  val enq = Flipped(new LanePositionedDecoupledIO(gen, lanes, depth))
  // deq.ready elements are dequeued; deq.ready must be <= deq.valid min lanes
  val deq = new LanePositionedDecoupledIO(gen, depth, lanes)

  // The 0th element enqueued comes from enq.bits(enq_0_lane), later elements wrap
  val enq_0_lane = Output(UInt(laneBitsU.W))
  val deq_0_lane = Output(UInt(laneBitsU.W))

  // Free-up space for enq; ready only if free.bits would not pass deq
  // free.ready includes same-cycle space from deq.ready
  val free = if (args.free) Some(Flipped(Decoupled(UInt(depthBitsU.W)))) else None
  // Rewind the deq/read pointer to the free pointer
  // Same-cycle free is included in the 'rewound' pointer state
  // deq.valid may go to 0 for a few cycles after rewind
  val rewind = if (args.rewind) Some(Input(Bool())) else None

  // Advance the commited space; ready only if commit.bits would not pass enq
  // commit.ready includes same-cycle space from enq.valid
  val commit = if (args.commit) Some(Flipped(Decoupled(UInt(depthBitsU.W)))) else None
  // Restore the enq/write pointer to the commit pointer
  // Same-cycle commit is included into the 'aborted' pointer state
  // enq.ready may go to 0 for a few cycles after abort
  val abort = if (args.abort) Some(Input(Bool())) else None

  // Connect two LPQs (enq <= deq)
  def driveWith(x: LanePositionedQueueIO[T]): Unit = { enq.driveWith(x.deq, enq_0_lane, x.deq_0_lane) }
}

trait LanePositionedQueueModule[T <: Data] extends Module {
  val io: LanePositionedQueueIO[T]
}

trait LanePositionedQueue {
  def apply[T <: Data](gen: T, args: LanePositionedQueueArgs): LanePositionedQueueModule[T]
  def apply[T <: Data](
    gen:    T,
    lanes:  Int,
    rows:   Int,
    flow:   Boolean = false,
    pipe:   Boolean = false,
    free:   Boolean = false,
    commit: Boolean = false,
    rewind: Boolean = false,
    abort:  Boolean = false): LanePositionedQueueModule[T] =
    apply(gen, LanePositionedQueueArgs(lanes, rows, flow, pipe, free, commit, rewind, abort))
}

/////////////////////////////// Index math implementation //////////////////////////

// A shared base class that keeps track of the indexing and flow control
class LanePositionedQueueBase[T <: Data](val gen: T, args: LanePositionedQueueArgs) extends Module with LanePositionedQueueModule[T] {
  val LanePositionedQueueArgs(lanes, rows, flow, pipe, free, commit, rewind, abort) = args

  require (rows >= 1)
  require (lanes >= 1)

  val io = IO(new LanePositionedQueueIO(gen, args))

  val capacity  = rows * lanes
  val rowBits   = log2Ceil(rows)
  val laneBits  = log2Ceil(lanes)
  val laneBits1 = log2Ceil(lanes+1) // [0, lanes]

  def lane(add: UInt): (UInt, Bool) = {
    if (lanes == 1) (0.U, add(0)) else {
      val out = RegInit(0.U(laneBits.W))
      val z = out +& add
      if (isPow2(lanes)) {
        out := z
        (out, z(laneBits))
      } else {
        val s = z - lanes.U
        val wrap = s.asSInt >= 0.S
        out := Mux(wrap, s, z)
        (out, wrap)
      }
    }
  }

  def row(inc: Bool): (UInt, UInt) = {
    if (rows == 1) (0.U, 0.U) else {
      val out = RegInit(0.U(rowBits.W))
      val out1 = WireInit(out + 1.U)
      if (!isPow2(rows)) {
        when (out === (rows-1).U) { out1 := 0.U }
      }
      when (inc) { out := out1 }
      (out, out1)
    }
  }

  val (enq_lane, enq_wrap) = lane(io.enq.valid)
  val (deq_lane, deq_wrap) = lane(io.deq.ready)
  val (enq_row,  enq_row1) = row(enq_wrap)
  val (deq_row,  deq_row1) = row(deq_wrap)

  val capBits1 = log2Ceil(capacity+1)
  val nDeq    = RegInit(       0.U(capBits1.W)) // deq.valid
  val nFree   = RegInit(       0.U(capBits1.W)) // free.ready
  val nEnq    = RegInit(capacity.U(capBits1.W)) // enq.ready
  val nCommit = RegInit(       0.U(capBits1.W)) // commit.ready

  val freed     = io.free  .map(x => Mux(x.fire, x.bits, 0.U)).getOrElse(io.deq.ready)
  val committed = io.commit.map(x => Mux(x.fire, x.bits, 0.U)).getOrElse(io.enq.valid)
  io.free.foreach   { x => x.ready := x.bits <= nFree   + io.deq.ready }
  io.commit.foreach { x => x.ready := x.bits <= nCommit + io.enq.valid }

  // Note: can be negative (abort coincides with commit of all remaining enq)
  val abortSize    = nCommit - committed
  val rewindSize   = nFree   - freed

  val nDeq_next    = nDeq    + committed    - io.deq.ready
  val nFree_next   = rewindSize             + io.deq.ready
  val nEnq_next    = nEnq    + freed        - io.enq.valid
  val nCommit_next = abortSize              + io.enq.valid

  nDeq    := nDeq_next
  nFree   := nFree_next
  nEnq    := nEnq_next
  nCommit := nCommit_next

  val doAbort  = io.abort .getOrElse(false.B)
  val doRewind = io.rewind.getOrElse(false.B)

  // Updating the indexing for non-power of two is too hard for now.
  require (!abort  || (isPow2(lanes) && isPow2(rows)))
  val enq_set = (if (lanes == 1) enq_row else Cat(enq_row, enq_lane)) - abortSize
  when (doAbort) {
    if (lanes > 1) enq_lane := enq_set(laneBits-1, 0)
    if (rows  > 1) enq_row  := enq_set >> laneBits
    nEnq    := nEnq_next + nCommit_next
    nCommit := 0.U
  }

  require (!rewind || (isPow2(lanes) && isPow2(rows)))
  val deq_set = (if (lanes == 1) deq_row else Cat(deq_row, deq_lane)) - rewindSize
  when (doRewind) {
    if (lanes > 1) deq_lane := deq_set(laneBits-1, 0)
    if (rows  > 1) deq_row  := deq_set >> laneBits
    nDeq  := nDeq_next + nFree_next
    nFree := 0.U
  }

  // These variables are only used for the assertion below
  val enq_pos = enq_row * lanes.U + enq_lane
  val deq_pos = deq_row * lanes.U + deq_lane
  val diff_pos = enq_pos + Mux(enq_pos >= deq_pos, 0.U, capacity.U) - deq_pos
  assert (nDeq + nCommit === diff_pos || (diff_pos === 0.U && nDeq + nCommit === capacity.U))
  assert (nDeq + nFree + nEnq + nCommit === capacity.U)

  io.enq_0_lane := enq_lane
  io.deq_0_lane := deq_lane
  io.enq.ready := (if (pipe) nEnq +& freed     else nEnq)
  io.deq.valid := (if (flow) nDeq +& committed else nDeq)

  // Constraints the user must uphold
  assert (io.enq.valid <= io.enq.ready)
  assert (io.deq.ready <= io.deq.valid)
  assert (io.enq.valid <= lanes.U)
  assert (io.deq.ready <= lanes.U)

  val enq_vmask = UIntToOH1(io.enq.valid +& enq_lane, 2*lanes-1).pad(2*lanes)
  val enq_lmask = (if (lanes==1) 0.U else UIntToOH1( enq_lane, lanes-1)).pad(2*lanes)
  val enq_mask  = enq_vmask & ~enq_lmask

  val deq_rmask = UIntToOH1(io.deq.ready +& deq_lane, 2*lanes-1).pad(2*lanes)
  val deq_lmask = (if (lanes==1) 0.U else UIntToOH1(deq_lane, lanes-1)).pad(2*lanes)
  val deq_mask  = deq_rmask & ~deq_lmask

  val deq_bits = Wire(Vec(lanes, gen))
  io.deq.bits := deq_bits

  // Bypass data when enq/deq rows overlap
  if (flow) {
    val logL = log2Floor((rows-1)*lanes) - 1
    val L = BigInt(1) << logL
    val maybe_empty = if (lanes <= L && L <= (rows-1)*lanes/2) {
      (free.B && (nFree>>logL).orR) || (nEnq>>logL).orR // nFree >= L || nEnq >= L
    } else if (free) {
      nFree + nEnq >= lanes.U
    } else {
      nEnq >= lanes.U
    }

    val row0 = deq_row  === enq_row && maybe_empty
    val row1 = deq_row1 === enq_row
    for (i <- 0 until lanes) {
      val set = Mux(deq_lmask(i),
        Mux(row0, false.B, Mux(row1, enq_lmask(i), true.B)),
        Mux(row1, true.B,  Mux(row0, enq_lmask(i), true.B)))
      when (!set) { io.deq.bits(i) := io.enq.bits(i) }
    }
  }
}

/////////////////////////////// Registered implementation //////////////////////////

class FloppedLanePositionedQueueModule[T <: Data](gen: T, args: LanePositionedQueueArgs)
  extends LanePositionedQueueBase(gen, args) {

  require (rows % 2 == 0)
  val bank = Seq.fill(2) { Mem(rows/2, Vec(lanes, gen)) }

  val b0_out = bank(0).read(deq_row1 >> 1)
  val b1_out = bank(1).read(deq_row  >> 1)
  for (l <- 0 until lanes) {
    deq_bits(l) := Mux(deq_row(0) ^ deq_lmask(l), b1_out(l), b0_out(l))
  }

  val hi_mask = enq_mask(2*lanes-1, lanes)
  val lo_mask = enq_mask(lanes-1, 0)
  val b0_mask = Mux(enq_row(0), hi_mask, lo_mask).asBools
  val b1_mask = Mux(enq_row(0), lo_mask, hi_mask).asBools
  val b0_row  = enq_row1 >> 1
  val b1_row  = enq_row  >> 1

  bank(0).write(b0_row, io.enq.bits, b0_mask)
  bank(1).write(b1_row, io.enq.bits, b1_mask)
}

object FloppedLanePositionedQueue extends LanePositionedQueue {
  def apply[T <: Data](gen: T, args: LanePositionedQueueArgs) =
    new FloppedLanePositionedQueueModule(gen, args)
}

/////////////////////////////// One port implementation ////////////////////////////

class OnePortLanePositionedQueueModule[T <: Data](ecc: Code)(gen: T, args: LanePositionedQueueArgs)
    extends LanePositionedQueueBase(gen, args) {

  require (rows >= 8 && rows % 4 == 0)
  // If rows <= 8, use FloppedLanePositionedQueue instead

  // Make the SRAM twice as wide, so that we can use 1RW port
  // Read accesses have priority, but we never do two back-back
  val codeBits = ecc.width(2*lanes*gen.getWidth)
  val ram = SyncReadMem(rows/2, UInt(codeBits.W))

  val deq_push = deq_wrap && deq_row(0)
  val enq_push = enq_wrap && enq_row(0)
  val pre_enq_row = Mux(enq_wrap, enq_row1, enq_row)
  val pre_deq_row = Mux(deq_wrap, deq_row1, deq_row)

  // Enq refill FSM schedule
  val enq_refill = RegInit(0.U(3.W))
  val enq_gap    = RegInit(false.B)
  val enq_refill_idle  = enq_refill === 0.U
  val enq_refill_wait0 = enq_refill === 1.U
  val enq_refill_wait1 = enq_refill === 2.U
  val enq_refill_ren0  = enq_refill === 3.U
  val enq_refill_flop0 = enq_refill === 4.U
  val enq_refill_ren1  = enq_refill === 5.U
  val enq_refill_flop1 = enq_refill === 6.U

  // Deq refill FSM schedule
  val deq_refill = RegInit(0.U(3.W))
  val deq_refill_idle  = deq_refill === 0.U
  val deq_refill_wait0 = deq_refill === 1.U
  val deq_refill_ren0  = deq_refill === 2.U
  val deq_refill_flop0 = deq_refill === 3.U
  val deq_refill_ren1  = deq_refill === 4.U
  val deq_refill_flop1 = deq_refill === 5.U

  // The contract between the abort FSM and deq readers is:
  //   If enq_refill_idle, then all valid data is in SRAM
  //   If !enq_refill_idle, then partial data must be bypassed

  // Advance enq refill FSM conditionally
  io.abort.foreach { a =>
    enq_gap := false.B
    when (enq_refill_idle) {
      when (a) {
        when (enq_set(laneBits+rowBits-1, laneBits+1) === (pre_enq_row >> 1) && nCommit_next < (2*lanes).U) {
          // If we have unwritten data, don't overwrite those values
          enq_gap := true.B
        } .otherwise {
          enq_refill := 1.U
        }
      }
    } .elsewhen (enq_refill_wait0) {
      enq_refill := 2.U
    } .elsewhen (enq_refill_wait1) {
      enq_refill := 3.U
    } .elsewhen (enq_refill_ren0) {
      when (!deq_push) { enq_refill := 4.U }
    } .elsewhen (enq_refill_flop0) {
      enq_refill := 5.U
    } .elsewhen (enq_refill_ren1) {
      when (!deq_push) { enq_refill := 6.U }
    } .otherwise {
      enq_refill := 0.U
    }
  }

  // Advance deq_refill FSM on rewind until complete
  io.rewind.foreach { r =>
    when (deq_refill_idle) {
      when (r) { deq_refill := 1.U }
    } .elsewhen (deq_refill_wait0) {
      deq_refill := 2.U
    } .elsewhen (deq_refill_ren0) {
      when (enq_refill_idle) { deq_refill := 3.U }
    } .elsewhen (deq_refill_flop0) {
      deq_refill := 4.U
    } .elsewhen (deq_refill_ren1) {
      when (enq_refill_idle) { deq_refill := 5.U }
    } .otherwise {
      deq_refill := 0.U
    }
  }

  // Block deq while deq_refilling after rewinding
  when (!deq_refill_idle) { io.deq.valid := 0.U }
  // Block enq while enq_refilling after abort
  when (!enq_refill_idle) { io.enq.ready := 0.U }
  // Also block enq while pipeline clears
  when (enq_gap) { io.enq.ready := 0.U }

  val enq_buffer = Reg(Vec(4, Vec(lanes, gen)))
  val deq_buffer = Reg(Vec(4, Vec(lanes, gen)))

  val logL = log2Floor((rows-2)*lanes) - 1 // rows >= 8
  val L = BigInt(1) << logL
  val next_maybe_empty = if (2*lanes <= L && L <= (rows-2)*lanes/2) {
    (free.B && (nFree_next>>logL).orR) || (nEnq_next>>logL).orR // nFree >= L || nEnq >= L
    // free <= deq <= commit <= enq <= free + rows*lanes
    //     nFree  nDeq    nCommit  nEnq
    // next_maybe_empty
    //   =>  nFree >= L || nEnq >= L
    //   =>  deq+capacity - enq >= nFree+nEnq >= L >= 2*lanes
    //   =>  (deq+capacity)/(2*lanes) != enq/(2*lanes)
    //   =>  not full
    // !next_maybe_empty
    //   =>  nFree < L && nEnq < L
    //   =>  enq-deq = nCommit+nDeq = rows*lanes - nFree - nEnq >= rows*lanes - 2L >= 2*lanes
    //   =>  enq/(2*lanes) != deq/(2*lanes)
    //   =>  not empty
  } else if (free) {
    nEnq_next + nFree_next >= (2*lanes).U
  } else {
    nEnq_next >= (2*lanes).U
  }

  val pre_gap = (pre_enq_row >> 1).zext - (pre_deq_row >> 1).zext
  val pre_gap0 = pre_gap === 0.S && next_maybe_empty
  val pre_gap1 = pre_gap0 || pre_gap === (1-rows/2).S || pre_gap === 1.S
  val pre_gap2 = pre_gap1 || pre_gap === (2-rows/2).S || pre_gap === 2.S
  val gap0 = RegNext(pre_gap0)
  val gap1 = RegNext(pre_gap1)
  val gap2 = RegNext(pre_gap2)

  val ren = (deq_push && (!enq_refill_idle || !pre_gap2)) || (enq_refill_idle && (deq_refill_ren0 || deq_refill_ren1)) || enq_refill_ren0 || enq_refill_ren1
  val wen = RegInit(false.B)

  when (!ren)                            { wen := false.B }
  when (enq_push && (rewind.B || !gap1)) { wen := true.B }

  val deq_row_half = deq_row >> 1
  val enq_row_half = enq_row >> 1
  val write_row = RegEnable(enq_row_half, enq_push)
  val ram_i = Mux(write_row(0),
    VecInit(enq_buffer(2) ++ enq_buffer(3)),
    VecInit(enq_buffer(0) ++ enq_buffer(1)))
  val read_row =
    Mux(enq_refill_ren0 && !deq_push, enq_row_half,
    Mux(enq_refill_ren1 && !deq_push, enq_row_half - 1.U, // requires isPow2(rows)
    Mux(deq_refill_ren0, deq_row_half,
    Mux(deq_refill_ren1, deq_row_half + 1.U, // requires isPow2(rows)
    Mux(!isPow2(rows).B && deq_row_half === (rows/2-1).U, 1.U,
    Mux(!isPow2(rows).B && deq_row_half === (rows/2-2).U, 0.U,
    deq_row_half + 2.U))))))
  val ram_o = ecc.decode(ram.read(read_row, ren)).corrected.asTypeOf(Vec(2*lanes, gen))
  when (wen && !ren) { ram.write(write_row, ecc.encode(ram_i.asUInt)) }

  val abort_bypass = if (abort) { wen && write_row === read_row } else { false.B }
  val bypass  = RegNext((deq_push &&  pre_gap2 && enq_refill_idle) || (deq_refill_ren1 &&  pre_gap2)   || (deq_refill_ren0 &&  pre_gap1) || abort_bypass)
  val dlatch0 = RegNext((deq_push && !deq_row(1))                  || (deq_refill_ren1 &&  deq_row(1)) || (deq_refill_ren0 && !deq_row(1)))
  val dlatch1 = RegNext((deq_push &&  deq_row(1))                  || (deq_refill_ren1 && !deq_row(1)) || (deq_refill_ren0 &&  deq_row(1)))
  val elatch0 = RegNext((enq_refill_ren1 &&  enq_row(1)) || (enq_refill_ren0 && !enq_row(1)))
  val elatch1 = RegNext((enq_refill_ren1 && !enq_row(1)) || (enq_refill_ren0 &&  enq_row(1)))

  for (l <- 0 until lanes) {
    when (dlatch0) {
      deq_buffer(0)(l) := Mux(bypass, enq_buffer(0)(l), ram_o(l))
      deq_buffer(1)(l) := Mux(bypass, enq_buffer(1)(l), ram_o(l+lanes))
    }
    when (dlatch1) {
      deq_buffer(2)(l) := Mux(bypass, enq_buffer(2)(l), ram_o(l))
      deq_buffer(3)(l) := Mux(bypass, enq_buffer(3)(l), ram_o(l+lanes))
    }
    when (elatch0) {
      enq_buffer(0)(l) := ram_o(l)
      enq_buffer(1)(l) := ram_o(l+lanes)
    }
    when (elatch1) {
      enq_buffer(2)(l) := ram_o(l)
      enq_buffer(3)(l) := ram_o(l+lanes)
    }
    for (r <- 0 until 4) {
      val rn = (r+3) % 4
      when ((enq_mask(l)       && enq_row(1,0) === r.U) ||
            (enq_mask(l+lanes) && enq_row(1,0) === rn.U)) {
        enq_buffer(r)(l) := io.enq.bits(l)
      }
      val gap = if (r % 2 == 0) gap0 else gap1
      when ((gap1 && enq_mask(l)       && enq_row(1,0) === r.U) ||
            (gap  && enq_mask(l+lanes) && enq_row(1,0) === rn.U)) {
        deq_buffer(r)(l) := io.enq.bits(l)
      }
    }
  }

  val deq_buf0 = deq_buffer(deq_row(1,0))
  val deq_buf1 = VecInit.tabulate(4) { i => deq_buffer((i+1) % 4) } (deq_row(1,0))
  for (l <- 0 until lanes) {
    deq_bits(l) := Mux(deq_lmask(l), deq_buf1(l), deq_buf0(l))
  }
}

case class OnePortLanePositionedQueue(ecc: Code) extends LanePositionedQueue {
  def apply[T <: Data](gen: T, args: LanePositionedQueueArgs) =
    new OnePortLanePositionedQueueModule(ecc)(gen, args)
}

/////////////////////////////// Black Box Unit Testing /////////////////////////////

import freechips.rocketchip.unittest._
import freechips.rocketchip.tilelink.LFSR64

class PositionedQueueTest(queueFactory: LanePositionedQueue, lanes: Int, rows: Int, rewind: Boolean, abort: Boolean, cycles: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val ids = (cycles+1) * lanes
  val bits = log2Ceil(ids+1)

  val q = Module(queueFactory(UInt(bits.W), lanes, rows, true, false, rewind, abort, rewind, abort))

  val enq = RegInit(0.U(bits.W))
  val deq = RegInit(0.U(bits.W))
  val com = RegInit(0.U(bits.W))
  val abt = RegInit(0.U(bits.W))
  val done = RegInit(false.B)

  def cap(x: UInt) = Mux(x > lanes.U, lanes.U, x) +& 1.U
  q.io.enq.valid := (LFSR64() * cap(q.io.enq.ready)) >> 64
  q.io.deq.ready := (LFSR64() * cap(q.io.deq.valid)) >> 64

  enq := enq + q.io.enq.valid
  deq := deq + q.io.deq.ready

  q.io.commit.foreach { c =>
    val legal = enq + q.io.enq.valid - com
    assert (c.ready || c.bits > legal)
    c.valid := LFSR64()(0)
    c.bits  := ((legal + 1.U) * LFSR64()) >> 63 // 50% likely to be legal
    when (c.fire) { com := com + c.bits }
  }

  q.io.free.foreach { f =>
    val legal = deq + q.io.deq.ready - abt
    assert (f.ready || f.bits > legal)
    f.valid := LFSR64()(0)
    f.bits  := ((legal + 1.U) * LFSR64()) >> 63
    when (f.fire) { abt := abt + f.bits }
  }

  q.io.rewind.foreach { r =>
    val f = q.io.free.get
    r := (LFSR64() & 0xf.U) === 0.U
    when (r) { deq := Mux(f.fire, abt + f.bits, abt) }
  }

  q.io.abort .foreach { a =>
    val c = q.io.commit.get
    a := (LFSR64() & 0xf.U) === 0.U
    when (a) { enq := Mux(c.fire, com + c.bits, com) }
  }

  when (enq >= (cycles*lanes).U) { done := true.B }
  io.finished := done

  q.io.enq.bits := VecInit.tabulate(lanes) { i =>
    val pos = Mux(i.U >= q.io.enq_0_lane, i.U, (i + lanes).U) - q.io.enq_0_lane
    Mux (pos >= q.io.enq.valid, 0.U, enq + pos)
  }

  q.io.deq.bits.zipWithIndex.foreach { case (d, i) =>
    val pos = Mux(i.U >= q.io.deq_0_lane, i.U, (i + lanes).U) - q.io.deq_0_lane
    assert (pos >= q.io.deq.valid || d === deq + pos)
  }
}
