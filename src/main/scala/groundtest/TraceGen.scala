// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

// This file was originally written by Matthew Naylor, University of
// Cambridge, based on code already present in the groundtest repo.
//
// This software was partly developed by the University of Cambridge
// Computer Laboratory under DARPA/AFRL contract FA8750-10-C-0237
// ("CTSRD"), as part of the DARPA CRASH research programme.
// 
// This software was partly developed by the University of Cambridge
// Computer Laboratory under DARPA/AFRL contract FA8750-11-C-0249
// ("MRC2"), as part of the DARPA MRC research programme.
// 
// This software was partly developed by the University of Cambridge
// Computer Laboratory as part of the Rigorous Engineering of
// Mainstream Systems (REMS) project, funded by EPSRC grant
// EP/K008528/1.

package freechips.rocketchip.groundtest
 
import chisel3._
import chisel3.util.{log2Up, MuxLookup, Cat, log2Ceil, Enum}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy.{ClockCrossingType}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.{TileCrossingParamsLike, CanAttachTile}
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSinkParameters}

// =======
// Outline
// =======

// Generate memory traces that result from random sequences of memory
// operations.  These traces can then be validated by an external
// tool.  A trace is a simply sequence of memory requests and
// responses.

// ==========================
// Trace-generator parameters
// ==========================

// Compile-time parameters:
//
//   * The id of the generator (there may be more than one in a
//     multi-core system).
//
//   * The total number of generators present in the system.
//
//   * The desired number of requests to be sent by each generator.
//
//   * A bag of physical addresses, shared by all cores, from which an
//     address can be drawn when generating a fresh request.
//
//   * A number of random 'extra addresses', local to each core, from
//     which an address can be drawn when generating a fresh request.
//     (This is a way to generate a wider range of addresses without having
//     to repeatedly recompile with a different address bag.)

case class TraceGenParams(
    wordBits: Int, // p(XLen) 
    addrBits: Int, // p(PAddrBits)
    addrBag: List[BigInt], // p(AddressBag)
    maxRequests: Int,
    memStart: BigInt, //p(ExtMem).base
    numGens: Int,
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    hartId: Int = 0
) extends InstantiableTileParams[TraceGenTile] with GroundTestTileParams
{
  def instantiate(crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): TraceGenTile = {
    new TraceGenTile(this, crossing, lookup)
  }
  val beuAddr = None
  val blockerCtrlAddr = None
  val name = None
  val clockSinkParams = ClockSinkParameters()
}

trait HasTraceGenParams {
  implicit val p: Parameters
  val params: TraceGenParams
  val pAddrBits           = params.addrBits
  val numGens             = params.numGens
  val numReqsPerGen       = params.maxRequests
  val memStart            = params.memStart
  val memRespTimeout      = 8192
  val numBitsInWord       = params.wordBits
  val numBytesInWord      = numBitsInWord / 8
  val numBitsInWordOffset = log2Up(numBytesInWord)
  val addressBag          = params.addrBag
  val addressBagLen       = addressBag.length
  val logAddressBagLen    = log2Up(addressBagLen)
  val genExtraAddrs       = false
  val logNumExtraAddrs    = 1
  val numExtraAddrs       = 1 << logNumExtraAddrs
  val maxTags             = 8

  require(numBytesInWord * 8 == numBitsInWord)
  require((1 << logAddressBagLen) == addressBagLen)
}

case class TraceGenTileAttachParams(
  tileParams: TraceGenParams,
  crossingParams: TileCrossingParamsLike
) extends CanAttachTile {
  type TileType = TraceGenTile
  val lookup: LookupByHartIdImpl = HartsWontDeduplicate(tileParams)
}


// ============
// Trace format
// ============

// Let <id>   denote a generator id;
//     <addr> denote an address (in hex);
//     <data> denote a value that is stored at an address;
//     <tag>  denote a unique request/response id;
// and <time> denote an integer representing a cycle-count.

// Each line in the trace takes one of the following formats.
//
//   <id>: load-req                <addr> #<tag> @<time>
//   <id>: load-reserve-req        <addr> #<tag> @<time>
//   <id>: store-req        <data> <addr> #<tag> @<time>
//   <id>: store-cond-req   <data> <addr> #<tag> @<time>
//   <id>: swap-req         <data> <addr> #<tag> @<time>
//   <id>: resp             <data>        #<tag> @<time>
//   <id>: fence-req                             @<time>
//   <id>: fence-resp                            @<time>

// NOTE: The (address, value) pair of every generated store is unique,
// i.e. the same value is never written to the same address twice.
// This aids trace validation.

// ============
// Random seeds
// ============

// The generator employs "unitialised registers" to seed its PRNGs;
// these are randomly initialised by the C++ backend.  This means that
// the "-s" command-line argument to the Rocket emulator can be used
// to generate new traces, or to replay specific ones.

// ===========
// Tag manager
// ===========

//  This is used to obtain unique tags for memory requests: each
//  request must carry a unique tag since responses can come back
//  out-of-order.
//
//  The tag manager can be viewed as a set of tags.  The user can take
//  a tag out of the set (if there is one available) and later put it
//  back.

class TagMan(val logNumTags : Int) extends Module {
  val io = IO(new Bundle {
    // Is there a tag available?
    val available = Output(Bool())
    // If so, which one?
    val tagOut    = Output(UInt(logNumTags.W))
    // User pulses this to take the currently available tag
    val take      = Input(Bool())
    // User pulses this to put a tag back
    val put       = Input(Bool())
    // And the tag put back is
    val tagIn     = Input(UInt((logNumTags.W)))
  })

  // Total number of tags available
  val numTags = 1 << logNumTags

  // For each tag, record whether or not it is in use
  val inUse = List.fill(numTags)(RegInit(false.B))

  // Mapping from each tag to its 'inUse' bit
  val inUseMap = (0 to numTags-1).map(i => i.U).zip(inUse)

  // Next tag to offer
  val nextTag = RegInit(0.U(logNumTags.W))
  io.tagOut := nextTag

  // Is the next tag available?
  io.available := ~MuxLookup(nextTag, true.B, inUseMap)

  // When user takes a tag
  when (io.take) {
    for ((i, b) <- inUseMap) {
      when (i === nextTag) { b := true.B }
    }
    nextTag := nextTag + 1.U
  }

  // When user puts a tag back
  when (io.put) {
    for ((i, b) <- inUseMap) {
      when (i === io.tagIn) { b := false.B }
    }
  }
}

// ===============
// Trace generator
// ===============

class TraceGenerator(val params: TraceGenParams)(implicit val p: Parameters) extends Module
    with HasTraceGenParams {
  val io = IO(new Bundle {
    val finished = Output(Bool())
    val timeout = Output(Bool())
    val mem = new HellaCacheIO
    val hartid = Input(UInt(log2Up(numGens).W))
    val fence_rdy = Input(Bool())
  })

  val totalNumAddrs = addressBag.size + numExtraAddrs
  val initCount = RegInit(0.U(log2Up(totalNumAddrs).W))
  val initDone  = RegInit(false.B)

  val reqTimer = Module(new Timer(8192, maxTags))
  reqTimer.io.start.valid := io.mem.req.fire
  reqTimer.io.start.bits := io.mem.req.bits.tag
  reqTimer.io.stop.valid := io.mem.resp.valid
  reqTimer.io.stop.bits := io.mem.resp.bits.tag

  // Random addresses
  // ----------------

  // Address bag, shared by all cores, taken from module parameters.
  // In addition, there is a per-core random selection of extra addresses.

  val bagOfAddrs = addressBag.map(x => (memStart + x).U(pAddrBits.W))

  val extraAddrs = Seq.fill(numExtraAddrs) {
    (memStart + SeededRandom.fromSeed.nextInt(1 << 16) * numBytesInWord).U(pAddrBits.W)
  }

  // A random index into the address bag.

  val randAddrBagIndex = LCG(logAddressBagLen)

  // A random address from the address bag.

  val addrBagIndices = (0 to addressBagLen-1).
                    map(i => i.U(logAddressBagLen.W))

  val randAddrFromBag = MuxLookup(randAddrBagIndex, 0.U,
                          addrBagIndices.zip(bagOfAddrs))

  // Random address from the address bag or the extra addresses.

  val extraAddrIndices = (0 to numExtraAddrs-1)
                          .map(i => i.U(logNumExtraAddrs.W))

  val randAddr =
        if (! genExtraAddrs) {
          randAddrFromBag
        }
        else {
          // A random index into the extra addresses.

          val randExtraAddrIndex = LCG(logNumExtraAddrs)

          // A random address from the extra addresses.
          val randAddrFromExtra = Cat(0.U,
                MuxLookup(randExtraAddrIndex, 0.U,
                  extraAddrIndices.zip(extraAddrs)), 0.U(3.W))

          Frequency(List(
            (1, randAddrFromBag),
            (1, randAddrFromExtra)))
        }

  val allAddrs = extraAddrs ++ bagOfAddrs
  val allAddrIndices = (0 until totalNumAddrs)
    .map(i => i.U(log2Ceil(totalNumAddrs).W))
  val initAddr = MuxLookup(initCount, 0.U,
    allAddrIndices.zip(allAddrs))

  // Random opcodes
  // --------------

  // Generate random opcodes for memory operations according to the
  // given frequency distribution.

  // Opcodes
  val (opNop   :: opLoad :: opStore ::
       opFence :: opLRSC :: opSwap  ::
       opDelay :: Nil) = Enum(7)

  // Distribution specified as a list of (frequency,value) pairs.
  // NOTE: frequencies must sum to a power of two.

  val randOp = Frequency(List(
    (10, opLoad),
    (10, opStore),
    (4,  opFence),
    (3,  opLRSC),
    (3,  opSwap),
    (2,  opDelay)))

  // Request/response tags
  // ---------------------

  // Responses may come back out-of-order.  Each request and response
  // therefore contains a unique 7-bit identifier, referred to as a
  // "tag", used to match each response with its corresponding request.

  // Create a tag manager giving out unique 3-bit tags
  val tagMan = Module(new TagMan(log2Ceil(maxTags)))

  // Default inputs
  tagMan.io.take  := false.B;
  tagMan.io.put   := false.B;
  tagMan.io.tagIn := 0.U;

  // Cycle counter
  // -------------

  // 32-bit cycle count used to record send-times of requests and
  // receive-times of respones.

  val cycleCount = RegInit(0.U(32.W))
  cycleCount := cycleCount + 1.U;

  // Delay timer
  // -----------

  // Used to implement the delay operation and to insert random
  // delays between load-reserve and store-conditional commands.

  // A 16-bit timer is plenty
  val delayTimer = Module(new DynamicTimer(16))

  // Used to generate a random delay period
  val randDelayBase = LCG16()

  // Random delay period: usually small, occasionally big
  val randDelay = Frequency(List(
    (14, 0.U(13.W) ## randDelayBase(2, 0)),
    (2,  0.U(11.W) ## randDelayBase(5, 0))))

  // Default inputs
  delayTimer.io.start  := false.B
  delayTimer.io.period := randDelay
  delayTimer.io.stop   := false.B

  // Operation dispatch
  // ------------------

  // Hardware thread id
  val tid = io.hartid

  // Request & response count
  val reqCount  = RegInit(0.U(32.W))
  val respCount = RegInit(0.U(32.W))

  // Current operation being executed
  val currentOp = RegInit(opNop)

  // If larger than 0, a multi-cycle operation is in progress.
  // Value indicates stage of progress.
  val opInProgress = RegInit(0.U(2.W))

  // Indicate when a fresh request is to be sent
  val sendFreshReq = Wire(Bool())
  sendFreshReq := false.B

  // Used to generate unique data values
  val nextData = RegInit(1.U((numBitsInWord-tid.getWidth).W))

  // Registers for all the interesting parts of a request
  val reqValid = RegInit(false.B)
  val reqAddr  = RegInit(0.U(numBitsInWord.W))
  val reqData  = RegInit(0.U(numBitsInWord.W))
  val reqCmd   = RegInit(0.U(5.W))
  val reqTag   = RegInit(0.U(7.W))

   // Condition on being allowed to send a fresh request
  val canSendFreshReq = (!reqValid || io.mem.req.fire) &&
                          tagMan.io.available

  // Operation dispatch
  when (reqCount < numReqsPerGen.U) {

    // No-op
    when (currentOp === opNop) {
      // Move on to a new operation
      currentOp := Mux(initDone, randOp, opStore)
    }

    // Fence
    when (currentOp === opFence) {
      when (opInProgress === 0.U && !reqValid) {
        // Emit fence request
        printf("%d: fence-req @%d\n", tid, cycleCount)
        // Multi-cycle operation now in progress
        opInProgress := 1.U
      }
      // Wait until all requests have had a response
      .elsewhen (reqCount === respCount && io.fence_rdy) {
        // Emit fence response
        printf("%d: fence-resp @%d\n", tid, cycleCount)
        // Move on to a new operation
        currentOp := randOp
        // Operation finished
        opInProgress := 0.U
      }
    }

    // Delay
    when (currentOp === opDelay) {
      when (opInProgress === 0.U) {
        // Start timer
        delayTimer.io.start := true.B
        // Multi-cycle operation now in progress
        opInProgress := 1.U
      }
      .elsewhen (delayTimer.io.timeout) {
        // Move on to a new operation
        currentOp := randOp
        // Operation finished
        opInProgress := 0.U
      }
    }

    // Load, store, or atomic swap
    when (currentOp === opLoad  ||
          currentOp === opStore ||
          currentOp === opSwap) {
      when (canSendFreshReq) {
        // Set address
        reqAddr := Mux(initDone, randAddr, initAddr)
        // Set command
        when (currentOp === opLoad) {
          reqCmd := M_XRD
        } .elsewhen (currentOp === opStore) {
          reqCmd := M_XWR
        } .elsewhen (currentOp === opSwap) {
          reqCmd := M_XA_SWAP
        }
        // Send request
        sendFreshReq := true.B
        // Move on to a new operation
        when (!initDone && initCount =/= (totalNumAddrs - 1).U) {
          initCount := initCount + 1.U
          currentOp := opStore
        } .otherwise {
          currentOp := randOp
          initDone := true.B
        }
      }
    }

    // Load-reserve and store-conditional
    // First issue an LR, then delay, then issue an SC
    when (currentOp === opLRSC) {
      // LR request has not yet been sent
      when (opInProgress === 0.U) {
        when (canSendFreshReq) {
          // Set address and command
          reqAddr := randAddr
          reqCmd  := M_XLR
          // Send request
          sendFreshReq := true.B
          // Multi-cycle operation now in progress
          opInProgress := 1.U
        }
      }
      // LR request has been sent, start delay timer
      when (opInProgress === 1.U) {
        // Start timer
        delayTimer.io.start := true.B
        // Indicate that delay has started
        opInProgress := 2.U
      }
      // Delay in progress
      when (opInProgress === 2.U) {
        when (delayTimer.io.timeout) {
          // Delay finished
          opInProgress := 3.U
        }
      }
      // Delay finished, send SC request
      when (opInProgress === 3.U) {
        when (canSendFreshReq) {
          // Set command, but leave address
          // i.e. use same address as LR did
          reqCmd  := M_XSC
          // Send request
          sendFreshReq := true.B
          // Multi-cycle operation finished
          opInProgress := 0.U
          // Move on to a new operation
          currentOp := randOp
        }
      }
    }
  }

  // Sending of requests
  // -------------------

  when (sendFreshReq) {
    // Grab a unique tag for the request
    reqTag := tagMan.io.tagOut
    tagMan.io.take := true.B
    // Fill in unique data
    reqData := Cat(nextData, tid)
    nextData := nextData + 1.U
    // Request is good to go!
    reqValid := true.B
    // Increment request count
    reqCount := reqCount + 1.U
  }
  .elsewhen (io.mem.req.fire) {
    // Request has been sent and there is no new request ready
    reqValid := false.B
  }

  // Wire up interface to memory
  io.mem.req.valid     := reqValid
  io.mem.req.bits.addr := reqAddr
  io.mem.req.bits.data := reqData
  io.mem.req.bits.size := log2Ceil(numBytesInWord).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.cmd  := reqCmd
  io.mem.req.bits.tag  := reqTag
  io.mem.req.bits.no_alloc := false.B
  io.mem.req.bits.no_xcpt := false.B
  io.mem.req.bits.mask := ~(0.U((numBitsInWord / 8).W))
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := PRV.M.U
  io.mem.req.bits.dv := false.B
  io.mem.keep_clock_enabled := true.B

  // The below signals don't matter because this uses the SimpleHellaIF
  io.mem.s1_data.data := RegNext(io.mem.req.bits.data)
  io.mem.s1_data.mask := RegNext(io.mem.req.bits.mask)
  io.mem.s1_kill := false.B
  io.mem.s2_kill := false.B

  // On cycle when request is actually sent, print it
  when (io.mem.req.fire) {
    // Short-hand for address
    val addr = io.mem.req.bits.addr
    // Print thread id
    printf("%d:", tid)
    // Print command
    when (reqCmd === M_XRD) {
      printf(" load-req 0x%x", addr)
    }
    when (reqCmd === M_XLR) {
      printf(" load-reserve-req 0x%x", addr)
    }
    when (reqCmd === M_XWR) {
      printf(" store-req %d 0x%x", reqData, addr)
    }
    when (reqCmd === M_XSC) {
      printf(" store-cond-req %d 0x%x", reqData, addr)
    }
    when (reqCmd === M_XA_SWAP) {
      printf(" swap-req %d 0x%x", reqData, addr)
    }
    // Print tag
    printf(" #%d", reqTag)
    // Print time
    printf(" @%d\n", cycleCount)
  }

  // Handling of responses
  // ---------------------

  // When a response is received
  when (io.mem.resp.valid) {
    // Put tag back in tag set
    tagMan.io.tagIn := io.mem.resp.bits.tag
    tagMan.io.put   := true.B
    // Print response
    printf("%d: resp %d #%d @%d\n", tid,
      io.mem.resp.bits.data, io.mem.resp.bits.tag, cycleCount)
    // Increment response count
    respCount := respCount + 1.U
  }

  // Termination condition
  // ---------------------

  val done = reqCount  === numReqsPerGen.U &&
             respCount === numReqsPerGen.U

  val donePulse = done && !RegNext(done, false.B)

  // Emit that this thread has completed
  when (donePulse) {
    printf(s"FINISHED ${numGens}\n")
  }

  io.finished := done
  io.timeout := reqTimer.io.timeout.valid
}


// =======================
// Trace-generator wrapper
// =======================

class TraceGenTile private(
  val params: TraceGenParams,
  crossing: ClockCrossingType,
  lookup: LookupByHartIdImpl,
  q: Parameters
) extends GroundTestTile(params, crossing, lookup, q)
{
  def this(params: TraceGenParams, crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val masterNode: TLOutwardNode = TLIdentityNode() := visibilityNode := dcacheOpt.map(_.node).getOrElse(TLTempNode())

  override lazy val module = new TraceGenTileModuleImp(this)
}

class TraceGenTileModuleImp(outer: TraceGenTile) extends GroundTestTileModuleImp(outer) {

  val tracegen = Module(new TraceGenerator(outer.params))
  tracegen.io.hartid := outer.hartIdSinkNode.bundle

  outer.dcacheOpt foreach { dcache =>
    val dcacheIF = Module(new SimpleHellaCacheIF())
    dcacheIF.io.requestor <> tracegen.io.mem
    dcache.module.io.cpu <> dcacheIF.io.cache
    tracegen.io.fence_rdy := dcache.module.io.cpu.ordered
  }

  outer.reportCease(Some(tracegen.io.finished))
  outer.reportHalt(Some(tracegen.io.timeout))
  outer.reportWFI(None)
  status.timeout.valid := tracegen.io.timeout
  status.timeout.bits := 0.U
  status.error.valid := false.B

  assert(!tracegen.io.timeout, s"TraceGen tile ${outer.tileParams.hartId}: request timed out")
}
