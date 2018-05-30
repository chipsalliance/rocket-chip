// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tile.{HasCoreParameters, CoreModule, CoreBundle}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import chisel3.experimental.dontTouch
import TLMessages._

trait HasSPFParameters extends HasCoreParameters {
  val blockIdxBits   = paddrBits - lgCacheBlockBytes    // # of bits to address one cache block in PA space
  val pgBlockIdxBits = pgIdxBits - lgCacheBlockBytes    // # of bits to address one cache block in a single page
}

class SPFReq(implicit p: Parameters) extends CoreBundle()(p) with HasSPFParameters {
  val addr = UInt(width = blockIdxBits)
}

object SPFCRs {
  val ctrl = 0x00
}

trait SPFParamsBase {
  val nStreams:       Int
  val address:        Option[BigInt]
  val size:           Int
  val prefQueueSize:  Int
  val pageCross:      Boolean
  val ageOut:         Boolean
  val ageBits:        Int
  val wordBytes:      Int
  val windowBits:     Int
  val accessTime:     Boolean
  val accessTimeBits: Int
  val distBits:       Int
  val distDefault:    Int
}

case class SPFParams (
  nStreams:       Int = 4,                // # of independent steams to track
  address:        Option[BigInt] = None,  // memory-mapped address of SPF TL slave interface for control registers
  size:           Int = 0x1000,           // Size in bytes of TL slave address space
  prefQueueSize:  Int = 8,                // # of entries in prefetch queue
  pageCross:      Boolean = false,        // Allow prefetching across page boundaries?
  wordBytes:      Int = 4,                // # of words in TL slave access
  ageOut:         Boolean = true,         // Support timers to age out (and mark invalid) inactive entries
  ageBits:        Int = 13,               // # of bits to encode age, for aging out
  windowBits:     Int = 6,                // # log2(# of cache blocks) to look forward/backward to detect address match
                                          //   also max # bits to encode stride
  accessTime:     Boolean = true,         // Support timing memory access latency, to tune prefetch aggressiveness
  accessTimeBits: Int = 8,                // # of bits to use for timing memory access latency
  distBits:       Int = 3,                // # of bits to encode prefetch distance field in control register
  distDefault:    Int = 3)                // Default value for prefetch distance
  extends SPFParamsBase {

  require(distDefault > 0)
}

abstract class SPFBundle(val c: SPFParamsBase) extends Bundle

// Layout for memory-mapped control register
class SPFControl(c: SPFParamsBase) extends SPFBundle(c) {
  val en          = Bool()
  val crossPageEn = Bool()
  val ageOutEn    = Bool()
  val timeEn      = Bool()
  val dist        = UInt(width=c.distBits)
  val approxL2Lat = UInt(width=c.accessTimeBits)
}

object SPFControl {
  def init(c: SPFParamsBase): SPFControl = {
    val ctrl = Wire(new SPFControl(c))

    ctrl.en           := Bool(false)
    ctrl.crossPageEn  := Bool(false)
    ctrl.ageOutEn     := Bool(false)
    ctrl.timeEn       := Bool(false)
    ctrl.dist         := UInt(c.distDefault)
    ctrl.approxL2Lat  := UInt(0, c.accessTimeBits)
    ctrl
  }
}

// Create free-listed array of prefetch requests. Requests sent out over TL may be ack'ed out-of-order,
// and we need to hold onto request until it is ACK'ed.
// Note: This won't guarantee that requests will be sent out in the same order generated
class SPFReqArray(nEntries: Int) (implicit p: Parameters) extends Module {
  val io = new Bundle {
    val enq     = Flipped(Decoupled(new SPFReq))                // Incoming request to "enqueue"
    val req     = Decoupled(new SPFReq)                         // Picked request to send out over TL
    val reqIdx  = UInt(width=log2Up(nEntries)).asOutput         // Index of picked request

    val rsp     = Flipped(Valid(UInt(width=log2Up(nEntries))))  // HintAck response along with entry index
  }

  // State for each entry
  val valids = Reg(init = UInt(0, nEntries))
  val sent   = Reg(init = UInt(0, nEntries))
  val data   = Reg(Vec(nEntries, new SPFReq))

  // Replace by finding lowest-indexed invalid entry
  val replEntryIdx = PriorityEncoder(~valids)
  val replEntryVec = PriorityEncoderOH(~valids) & Fill(nEntries, io.enq.fire())

  // Request is "ready" if it is valid and has not already sent a request
  // Pick lowest-indexed ready entry for next request
  val readyVec = valids & ~sent
  val pickVec  = PriorityEncoderOH(readyVec) & Fill(nEntries, io.req.fire())
  val ackVec   = UIntToOH(io.rsp.bits) & Fill(nEntries, io.rsp.valid)

  io.enq.ready := !(valids.andR)
  when (io.enq.fire()) {
    data(replEntryIdx) := io.enq.bits
  }
  valids := (valids | replEntryVec) & ~ackVec
  sent   := (sent   | pickVec)      & ~ackVec

  io.req.valid     := readyVec.orR
  io.req.bits.data := PriorityMux(readyVec, data)
  io.reqIdx        := PriorityEncoder(readyVec)
}

// Class for various state and control signals generated by each prefetcher entry and 
// used by the main prefetching logic.
class SPFEntryState(val blockIdxBits: Int, val windowBits: Int) extends Bundle {
  val prevAddr    = UInt(width = blockIdxBits)      // Previously seen lookup address
  val strideValid = Bool()                          // Stride has been computed for this entry (maybe only seen once so far)
  val trained     = Bool()                          // Stride has been detected and seen more than once
  val stride      = UInt(width = windowBits)        // Detected stride value
  val strideSign  = UInt(width = 1)                 // Sign of detected stride
  val addrMatch   = Bool()                          // This entry matches lookup address
}

// Class for various control signals sent to each prefetcher entry.  Some signals
// are generated specific to each entry, and some fanout to all entries but are only used by the lookup match entry
// or picked armed entry.
class SPFEntryUpdate (val blockIdxBits: Int, val pgIdxBits: Int) extends Bundle {
  val allocate        = Bool()                      // Allocate this entry for new potential stream
  val invalidate      = Bool()                      // Invalidate this entry
  val strideMatch     = Bool()                      // Current computed stride is same as matching entry
  val addrPpnP1Match  = Bool()                      // Lookup address matches PPN +/- 1 of matching entry
  val curStride       = UInt(width = pgIdxBits)     // Current computed stride based on matching entry
  val curStrideSign   = UInt(width = 1)             // Sign of current computed stride
  val baseAddrInc     = UInt(width = blockIdxBits)  // Incremented(/decremented) prefetch address based on matching entry
  val increaseAggr    = Bool()                      // Increase "aggressiveness" of this entry
  val crossPagePause  = Bool()                      // Prefetched addresses crossed a page, so pause this entry
}

class SPFEntry(c: SPFParamsBase) (implicit p: Parameters) extends CoreModule()(p) with HasSPFParameters {
  val io = new Bundle {
    // Control signals (these could be bundled into SPFEntryUpdate class...)
    val ageOutEn    = Bool().asInput
    val dist        = UInt(width=c.distBits).asInput

    val state       = Valid(new SPFEntryState(blockIdxBits, c.windowBits))
    val update      = new SPFEntryUpdate(blockIdxBits, pgIdxBits).flip
    val lookupReq   = Flipped(Valid(new SPFReq))  // - Note: outside logic determines stride match (so logic isn't duplicated per entry)
    val prefReq     = Decoupled(new SPFReq)       // - Note: outside logic determines crossPage and baseAddrInc (so logic isn't duplicated per entry)
  }

  val addrMatch             = Wire(Bool())
  val lookupAddrMatch       = Wire(Bool())
  val validLookupAddrMatch  = Wire(Bool())
  val agingOut              = Wire(Bool())
  val loadPrevAddr          = Wire(Bool())
  val loadSentAddr          = Wire(Bool())
  val initEntry             = Wire(Bool())
  val prefetchFire          = Wire(init = false.B)

  // State machine states
  val s_invalid :: s_detect1 :: s_detect2 :: s_trained :: s_armed :: s_paused :: Nil = Enum(UInt(), 6)

  // State elements
  val nextState   = Wire(init = s_invalid)
  val state       = RegNext(nextState)
  val prevState   = RegNext(state)
  val prevAddr    = RegEnable(io.lookupReq.bits.addr,   loadPrevAddr)
  val sentAddr    = RegEnable(io.update.baseAddrInc,    loadSentAddr)
  val stride      = RegEnable(io.update.curStride,      validLookupAddrMatch)
  val strideSign  = RegEnable(io.update.curStrideSign,  loadPrevAddr)
  val genCount    = Reg(UInt(width = c.distBits))
  val age         = Reg(UInt(width = c.ageBits))
  val aggr        = Reg(UInt(width = c.distBits))

  val entValid = (state =/= s_invalid)

  // ------------------------------------------------------
  // Entry state and status needed by central prefetcher logic
  // ------------------------------------------------------
  io.state.valid            := entValid
  io.state.bits.prevAddr    := prevAddr
  io.state.bits.strideValid := (state >= s_detect2)
  io.state.bits.trained     := (state >  s_detect2)
  io.state.bits.stride      := stride
  io.state.bits.strideSign  := strideSign
  io.state.bits.addrMatch   := addrMatch

  // ------------------------------------------------------
  // Lookup request
  // ------------------------------------------------------
  // - Consider lookup an address match if: (prevAddr - X) < lookupAddr < (prevAddr + X)
  //   where X is 2^windowBits.
  // - Note that this means that multiple streams with addresses close to each other 
  //   will cause conflicts in prefetcher.
  addrMatch := ((prevAddr - (1 << c.windowBits)) < io.lookupReq.bits.addr) &&
               ((prevAddr + (1 << c.windowBits)) > io.lookupReq.bits.addr)

  lookupAddrMatch       := io.lookupReq.valid && addrMatch
  validLookupAddrMatch  := entValid && lookupAddrMatch

  initEntry     := io.update.allocate
  loadPrevAddr  := initEntry || validLookupAddrMatch
  loadSentAddr  := prefetchFire

  // ------------------------------------------------------
  // State machine logic
  // ------------------------------------------------------
  when (io.update.invalidate || agingOut) {
    // Entry is being invalidated for one of a few reasons:
    // 1) Multiple address matches were detected.  Likely one stream overlapped with another stale stream.
    //    In this case we invalidate all match entries except the most recent matching one.
    // 2) Prefetcher is being disabled by software.
    // 3) Entry is aging out, meaning that a certain number of cycles elapsed without this entry generating prefetches.
    //    This may be useful in preventing old entries from generating spurious prefetches when later memory accesses happen
    //    to overlap in same address space.
    nextState := s_invalid
  } .elsewhen (io.update.allocate) {
    // Entry can be re-allocated even while valid, due to the replacement policy
    nextState := s_detect1
  } .otherwise {
    switch (state) {
      // Implicit Invalid state:
      // Only transition out of here is by being allocated, which is handled above

      // In the Detect1 state:
      // - We received one address (prevAddr), and we need to wait for a second address to compute a stride.
      //   If we detect an address match, record the stride and move to Detect2 state.
      is (s_detect1) {
        when (lookupAddrMatch) {
          nextState := s_detect2
        } .otherwise {
          nextState := s_detect1
        }
      }

      // In the Detect2 state:
      // - An initial stride has been computed.  Wait for the next lookup address match and see if the stride matches.
      //   If stride matches, move to armed state to generate first prefetches.  Otherwise, go back to Detect1 state.
      is (s_detect2) {
        when (lookupAddrMatch) {
          nextState := Mux(io.update.strideMatch, s_armed, s_detect1)
        } .otherwise {
          nextState := s_detect2
        }
      }

      // In the trained state:
      // - We have seen a repeated stride, so wait for the next lookup address match.  If the stride no longer matches,
      //   move back to Detect1 state.  If the stride matches, move to Armed state to generate prefetch requests.
      // TODO: It's more consistent with detect2 state if we transition to detect2 on stride mismatch.  This just matches
      //   the old code's behavior.
      is (s_trained) {
        when (lookupAddrMatch) {
          nextState := Mux(io.update.strideMatch, s_armed, s_detect1)
        } .otherwise {
          nextState := s_trained
        }
      }

      // In the armed state:
      // - A counter is used to generate a programmable number of prefetch requests ahead of the detected stream.
      //   One prefetch request will be generated per cycle (as long as request queue is ready).
      // - Remain in the Armed state until all requests have been generated, or we encounter a condition which requires
      //   us to pause:
      //   + Prefetch addresses will cross a 4K page boundary, and cross-page prefetching is disabled.  In this case move
      //     to the Paused state.
      // TODO: If we get another lookup address match with a mismatching stride, we could transition back to detect2 state
      //   before all prefetches are generated, but for now we can just let them go out
      is (s_armed) {
        when (io.prefReq.ready) {
          when (io.update.crossPagePause) {
            nextState := s_paused
          } .otherwise {
            prefetchFire := true.B
            nextState := Mux(genCount === UInt(0), s_trained, s_armed)
          }
        } .otherwise {
          nextState := s_armed
        }
      }

      // In the paused state:
      // - Stop generating prefetches and wait until the lookup address crosses into the next/previous page.  If this happens, either
      //   go to Armed if the stride matches, or go to detect1 if not 
      is (s_paused) {
        when (lookupAddrMatch) {
          when (!io.update.strideMatch) {
            nextState := s_detect1
          } .otherwise {
            nextState := Mux(io.update.addrPpnP1Match, s_armed, s_paused)
          }
        } .otherwise {
          nextState := s_paused
        }
      }
    }
  }
  state := nextState    // state update

  val initGenCount      = (state =/= s_armed) && (nextState === s_armed)
  val initialPrefetch   = state.isOneOf(s_detect2, s_paused)
  val r_initialPrefetch = RegNext(initialPrefetch)

  // ------------------------------------------------------
  // Prefetch request generation
  // ------------------------------------------------------
  // - Note: page crossing is determined outside. When page crossing is disabled, prefetch request will not be
  //   placed into request queue, and update.crossPagePause input will be asserted.
  io.prefReq.valid      := (state === s_armed)
  io.prefReq.bits.addr  := Mux(r_initialPrefetch, prevAddr, sentAddr)

  // ------------------------------------------------------
  // Updates to state registers
  // ------------------------------------------------------
  when (!io.update.invalidate && initGenCount) {
    genCount := Mux(initialPrefetch, io.dist-UInt(1), aggr)
  } .elsewhen ((state === s_armed) && io.prefReq.ready) {
    genCount := genCount - UInt(1)
  }

  when (initEntry) {
    aggr := UInt(0, c.distBits)
  } .elsewhen (io.update.increaseAggr) {
    aggr := Mux(aggr.andR, aggr, aggr + UInt(1, c.distBits))
  }

  // ------------------------------------------------------
  // Age timer (if supported)
  // ------------------------------------------------------
  // - When entry is allocated reset age to 0
  // - When entry is generating prefetch reset age to 0
  // - Otherwise if entry is valid, increment age
  if (c.ageOut) {
    when (initEntry || (state === s_armed)) {
      age := UInt(0)
    } .elsewhen (entValid && io.ageOutEn) {
      age := age + UInt(1)
    }

    agingOut := age === (UIntToOH(c.ageBits) - 1)
  } else {
    agingOut := false.B
  }
}

class SPFModule(c: SPFParamsBase, outer: TLSPF) (implicit p: Parameters) extends LazyModuleImp(outer)
  with HasSPFParameters {

  val (tl_in, edge_in)    = outer.node.in(0)          // TL node/edge coming to monitor for address strides
  val (tl_out, edge_out)  = outer.masterNode.out(0)   // TL node/edge for generating prefetch requests

  val ctrl            = Reg(init = SPFControl.init(c))                    // Memory-mapped control register
  val reqQueue        = Module(new SPFReqArray(c.prefQueueSize))          // Queue for prefetch requests
  val spfEntries      = Seq.fill(c.nStreams) { Module(new SPFEntry(c)) }  // Prefetcher entries
  val replIdx         = Wire(UInt(width = log2Up(c.nStreams)))            // Replacement index from PseudoLRU
  val increaseAggrVec = Wire(init = Fill(c.nStreams, false.B))

  // Grab lookup information from monitored TL channel
  val lookupValid = tl_in.a.fire() && (tl_in.a.bits.opcode === AcquireBlock) && ctrl.en
  val lookupAddr  = (tl_in.a.bits.address >> lgCacheBlockBytes)   // Don't care about block offset bits

  protected val regmapBase = Seq(
    SPFCRs.ctrl -> RegFieldGroup("ctrl", Some("SPF Control"), Seq(
      RegField(1,                 ctrl.en,           RegFieldDesc("en",          "Prefetcher global enable",             reset=Some(0))),
      RegField(1,                 ctrl.crossPageEn,  RegFieldDesc("crossPageEn", "Enable prefetching cross-page",        reset=Some(0))),
      RegField(1,                 ctrl.ageOutEn,     RegFieldDesc("ageOutEn",    "Enabling aging out inactive entries",  reset=Some(0))),
      RegField(c.distBits,        ctrl.dist,         RegFieldDesc("dist",        "Prefetch distance",                    reset=Some(c.distDefault))),
      RegField(c.accessTimeBits,  ctrl.approxL2Lat,  RegFieldDesc("approxL2Lat", "Approximate L2 Latency",               reset=Some(0)))
    ))
  )

  // Detect when prefetcher is being disabled, so we can clear valids and handle cleanup
  val disabling = RegNext(ctrl.en) && !ctrl.en

  assert(!ctrl.ageOutEn || c.ageOut, "SPF control register enabled timeout, but hardware not included")

  // ------------------------------------------------------
  // Helper functions
  // ------------------------------------------------------
  // Get PPN from address
  def getPpn(addr: UInt) = {
    addr(blockIdxBits-1,pgBlockIdxBits)
  }

  // Get Page offset bits while masking block offset bits
  def getPageBlock(addr: UInt): UInt = {
    addr(pgBlockIdxBits-1,0)
  }
  
  // Get PPN +/- 1 (depending on sign input)
  def getPpnInc(addr: UInt, sign: UInt) = {
    getPpn(addr) + Cat(Fill(blockIdxBits-pgBlockIdxBits-1,sign), UInt(1,1))
  }

  // Generate address of next cache block by: incrementing page offset. Note that addresses stored in prefetch entries
  // exclude block offset bits
  def genNextAddr(addr: UInt, stride: UInt, sign: UInt): (UInt, UInt) = {
    // sign-extend and compute address + stride
    val sum = Cat(UInt(0,1),addr(pgBlockIdxBits-1,0)) + Cat(Fill(pgBlockIdxBits+1-c.windowBits,sign),stride)
    val (crossPage, pgOffset) = Split(sum, pgBlockIdxBits)

    // Either add or subtract 1 based on sign
    val ppnP1 = getPpn(addr) + Cat(Fill(blockIdxBits-pgBlockIdxBits-1, sign), UInt(1, 1))

    val nextAddr = Cat( Mux(crossPage(0), ppnP1, getPpn(addr)), pgOffset)
    (crossPage, nextAddr)
  }

  // ------------------------------------------------------
  // Lookup address match detection
  // ------------------------------------------------------
  // Look at address match indications from each entry and select lowest-indexed matching entry
  val lookupMatchVec  = spfEntries.map(e => e.io.state.valid && e.io.state.bits.addrMatch).asUInt
  val lookupMatchIdx  = PriorityEncoder(lookupMatchVec)
  val lookupMiss      = !(lookupMatchVec.orR)
  val matchState      = PriorityMux(lookupMatchVec, spfEntries.map(_.io.state.bits))

  // If we get an address match on a paused entry, make sure that lookup address is in the next page.
  val addrPpnP1Match  = (getPpnInc(matchState.prevAddr,matchState.strideSign) === getPpn(lookupAddr))

  // On a multi-match (two streams are close in the address-space), just invalidate the highest-indexed one.
  val multiMatch          = PopCount(lookupMatchVec) > UInt(1)
  val multiMatchInvalVec  = Reverse(PriorityEncoderOH(Reverse(lookupMatchVec))) & Fill(c.nStreams, multiMatch)

  // Compute current stride from (current lookup address) - (previously seen address from matching entry)
  // Do explicit zero-extend and subtraction so that we can extract the carry-out bit to determine the sign of the stride
  val (strideUpper, curStride) = Split(Cat(UInt(0,1),lookupAddr) + ~Cat(UInt(0,1),matchState.prevAddr) + UInt(1), c.windowBits)
  val curStrideSign   = strideUpper(blockIdxBits-c.windowBits)            // MSB from subtraction (sign)
  val strideMismatch  = ((curStride =/= matchState.stride) || (curStrideSign =/= matchState.strideSign)) && matchState.strideValid
  
  // ------------------------------------------------------
  // Selection of armed entry and prefetch address generation
  // ------------------------------------------------------
  val armedVec         = spfEntries.map(_.io.prefReq.valid).asUInt
  val pickedEntryIdx   = PriorityEncoder(armedVec)
  val pickedBaseAddr   = PriorityMux(armedVec, spfEntries.map(_.io.prefReq.bits.addr))
  val pickedState      = PriorityMux(armedVec, spfEntries.map(_.io.state.bits))

  // Generate incremented/decremented next prefetch address
  val (crossPage, baseAddrInc) = genNextAddr(pickedBaseAddr, pickedState.stride, pickedState.strideSign)
  val crossPagePause = crossPage(0) && !ctrl.crossPageEn

  // ------------------------------------------------------
  // Generation of prefetcher entries
  // ------------------------------------------------------
  val valids  = spfEntries.map(_.io.state.valid).asUInt
  spfEntries.zipWithIndex.foreach { case (ent, i) =>
    ent.io.ageOutEn              := ctrl.ageOutEn
    ent.io.dist                  := ctrl.dist

    ent.io.update.allocate       := lookupValid && lookupMiss && (replIdx === UInt(i))
    ent.io.update.invalidate     := disabling || multiMatchInvalVec(i)
    ent.io.update.strideMatch    := !strideMismatch
    ent.io.update.addrPpnP1Match := addrPpnP1Match
    ent.io.update.curStride      := curStride
    ent.io.update.curStrideSign  := curStrideSign
    ent.io.update.baseAddrInc    := baseAddrInc
    ent.io.update.crossPagePause := crossPagePause && (pickedEntryIdx === UInt(i))
    ent.io.update.increaseAggr   := increaseAggrVec(i)

    ent.io.lookupReq.valid       := lookupValid
    ent.io.lookupReq.bits.addr   := lookupAddr

    ent.io.prefReq.ready         := reqQueue.io.enq.ready && (pickedEntryIdx === UInt(i))
  }

  // Pseudo-LRU replacement for choosing a stream to replace, unless we are configured for only 1 stream
  if (c.nStreams > 1) {
    val plru = new PseudoLRU(c.nStreams)
    replIdx := Mux(!valids(c.nStreams-1, 0).andR, PriorityEncoder(~valids(c.nStreams-1, 0)), plru.replace)

    when (lookupValid) {
      plru.access(Mux(lookupMiss, replIdx, lookupMatchIdx))
    }
  } else {
    replIdx := UInt(0, log2Up(c.nStreams))
  }

  // Memory access timer (if supported)
  // - Count number of cycles between lookup request on channel A and response on channel D to determine
  //   whether the L2 possibly missed (this will be an approximation as we don't have a "hit" indication in response)
  // - If response takes longer than a certain number of cycles, assume L2 missed and take this as a hint that we need
  //   to prefetch more aggressively (issue more incremental prefetches)
  if (c.accessTime) {
    val nTimers = (1 << tl_in.a.bits.source.getWidth) - 1
    val timerValids = RegInit(Vec.fill(nTimers) {false.B})
    val timerCounts = Reg(Vec(nTimers, UInt(width = c.accessTimeBits)))
    val timerSPFIdx = Reg(Vec(nTimers, UInt(width = log2Up(c.nStreams))))

    val aSource = tl_in.a.bits.source
    val dSource = tl_in.d.bits.source
    val SPFIdx  = timerSPFIdx(dSource)

    for (i <- 0 until nTimers) {
      when (lookupValid && !lookupMiss && matchState.trained && (i === aSource)) {
        // Allocate new timer using source field from channel A request
        timerValids(i) := true.   B
        timerCounts(i) := UInt(0, c.accessTimeBits)
        timerSPFIdx(i) := lookupMatchIdx
      } .elsewhen (tl_in.d.fire() && (i === dSource)) {
        // Check to see if timer is allocated for indicated source on channel D response
        // If so, grab index of SPF entry and increase aggressiveness if access took long enough
        timerValids(i) := false.B
      } .elsewhen (timerValids(i) && !timerCounts(i).andR) {
        timerCounts(i) := timerCounts(i) + UInt(1, c.accessTimeBits)
      }
    }

    when (tl_in.d.fire() && timerValids(dSource) && valids(SPFIdx) && (timerCounts(dSource) > ctrl.approxL2Lat)) {
        increaseAggrVec := UIntToOH(SPFIdx, width=c.nStreams)
    }

  } else {
    increaseAggrVec := Fill(c.nStreams, false.B)
  }

  // ------------------------------------------------------
  // Prefetch request generation portion
  // ------------------------------------------------------
  // Note: We suppress generated prefetch request if we detect that new address crosses a page boundary, and page-crossing
  //       is disabled.
  reqQueue.io.enq.valid     := armedVec.orR && !crossPagePause
  reqQueue.io.enq.bits.addr := baseAddrInc
  reqQueue.io.req.ready     := tl_out.a.fire()
  reqQueue.io.rsp.valid     := tl_out.d.fire()
  reqQueue.io.rsp.bits      := tl_out.d.bits.source

  tl_out.a.valid  := reqQueue.io.req.valid
  tl_out.a.bits   := edge_out.Hint(
                      fromSource = reqQueue.io.reqIdx,
                      toAddress = Cat(reqQueue.io.req.bits.addr, UInt(0, lgCacheBlockBytes)),
                      lgSize = lgCacheBlockBytes,
                      param = TLHints.PREFETCH_READ)._2

  // Tie-offs
  tl_out.b.ready  := Bool(true)
  tl_out.c.valid  := Bool(false)
  tl_out.d.ready  := Bool(true)
  tl_out.e.valid  := Bool(false)
}

abstract class TLSPFBase (c: SPFParamsBase, hartId: Int)(implicit p: Parameters) extends LazyModule {
  require(isPow2(c.size))
  val device = new SimpleDevice("spf", Seq("sifive,spf0"))

  // Node for register map
  val rnode = TLRegisterNode(address = Seq(AddressSet(c.address.getOrElse(0), c.size-1)), device = device, beatBytes = c.wordBytes)

  // Node for adapter (monitoring channel)
  val node = TLAdapterNode()

  // Master node for generating prefetch hints
  val masterNode = TLClientNode(Seq(TLClientPortParameters(
    Seq(TLClientParameters(
      name = s"Core ${hartId} SPF",
      sourceId = IdRange(0, c.prefQueueSize),
      requestFifo = true
    )))))
}

class TLSPF(c: SPFParams, hartId: Int)(implicit p: Parameters) extends TLSPFBase(c,hartId)(p) {
  lazy val module = new SPFModule(c, this) {
    rnode.regmap(regmapBase:_*)

    // For now, pass-through the channel we are monitoring to the output
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
    }
  }
}
