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

package groundtest
 
import Chisel._
import uncore.tilelink._
import uncore.constants._
import uncore.devices.NTiles
import junctions._
import rocket._
import scala.util.Random
import cde.{Parameters, Field}

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

case object AddressBag extends Field[List[Int]]

trait HasTraceGenParams {
  implicit val p: Parameters
  val numGens             = p(NTiles)
  val numBitsInId         = log2Up(numGens)
  val numReqsPerGen       = p(MaxGenerateRequests)
  val memRespTimeout      = 1024
  val numBitsInWord       = p(WordBits)
  val numBytesInWord      = numBitsInWord / 8
  val numBitsInWordOffset = log2Up(numBytesInWord)
  val addressBag          = p(AddressBag)
  val addressBagLen       = addressBag.length
  val logAddressBagLen    = log2Up(addressBagLen)
  val genExtraAddrs       = false
  val logNumExtraAddrs    = 1
  val numExtraAddrs       = 1 << logNumExtraAddrs

  require(numBytesInWord * 8 == numBitsInWord)
  require((1 << logAddressBagLen) == addressBagLen)
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
  val io = new Bundle {
    // Is there a tag available?
    val available = Bool(OUTPUT)
    // If so, which one?
    val tagOut    = UInt(OUTPUT, logNumTags)
    // User pulses this to take the currently available tag
    val take      = Bool(INPUT)
    // User pulses this to put a tag back
    val put       = Bool(INPUT)
    // And the tag put back is
    val tagIn     = UInt(INPUT, logNumTags)
  }

  // Total number of tags available
  val numTags = 1 << logNumTags

  // For each tag, record whether or not it is in use
  val inUse = List.fill(numTags)(Reg(init = Bool(false)))

  // Mapping from each tag to its 'inUse' bit
  val inUseMap = (0 to numTags-1).map(i => BitPat(UInt(i))).zip(inUse)

  // Next tag to offer
  val nextTag = Reg(init = UInt(0, logNumTags))
  io.tagOut := nextTag

  // Is the next tag available?
  io.available := ~Lookup(nextTag, Bool(true), inUseMap)

  // When user takes a tag
  when (io.take) {
    for ((i, b) <- inUseMap) {
      when (i === nextTag) { b := Bool(true) }
    }
    nextTag := nextTag + UInt(1)
  }

  // When user puts a tag back
  when (io.put) {
    for ((i, b) <- inUseMap) {
      when (i === io.tagIn) { b := Bool(false) }
    }
  }
}

// ===============
// Trace generator
// ===============

class TraceGenerator(id: Int)
    (implicit p: Parameters) extends L1HellaCacheModule()(p)
                                with HasTraceGenParams {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
    val mem = new HellaCacheIO
  }

  // Random addresses
  // ----------------
  
  // Address bag, shared by all cores, taken from module parameters.
  // In addition, there is a per-core random selection of extra addresses.

  val addrHashMap = p(GlobalAddrMap)
  val baseAddr = addrHashMap("mem").start + 0x01000000

  val bagOfAddrs = addressBag.map(x => UInt(x, numBitsInWord))

  val extraAddrs = (0 to numExtraAddrs-1).
                   map(i => Reg(UInt(width = 16)))

  // A random index into the address bag.

  val randAddrBagIndex = Module(new LCG(logAddressBagLen)).io.out

  // A random address from the address bag.

  val addrBagIndices = (0 to addressBagLen-1).
                    map(i => UInt(i, logAddressBagLen))
  
  val randAddrFromBag = MuxLookup(randAddrBagIndex, UInt(0),
                          addrBagIndices.zip(bagOfAddrs))

  // Random address from the address bag or the extra addresses.

  val randAddr =
        if (! genExtraAddrs) {
          randAddrFromBag
        }
        else {
          // A random index into the extra addresses.

          val randExtraAddrIndex = Module(new LCG(logNumExtraAddrs)).io.out

          // A random address from the extra addresses.

          val extraAddrIndices = (0 to numExtraAddrs-1).
                                 map(i => UInt(i, logNumExtraAddrs))
  
          val randAddrFromExtra = Cat(UInt(0),
                MuxLookup(randExtraAddrIndex, UInt(0),
                  extraAddrIndices.zip(extraAddrs)), UInt(0, 3))

          Frequency(List(
            (1, randAddrFromBag),
            (1, randAddrFromExtra)))
        }

  // Random opcodes
  // --------------
 
  // Generate random opcodes for memory operations according to the
  // given frequency distribution.

  // Opcodes
  val (opNop   :: opLoad :: opStore ::
       opFence :: opLRSC :: opSwap  ::
       opDelay :: Nil) = Enum(Bits(), 7)

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
  val tagMan = Module(new TagMan(3))

  // Default inputs
  tagMan.io.take  := Bool(false);
  tagMan.io.put   := Bool(false);
  tagMan.io.tagIn := UInt(0);

  // Cycle counter
  // -------------

  // 32-bit cycle count used to record send-times of requests and
  // receive-times of respones.

  val cycleCount = Reg(init = UInt(0, 32))
  cycleCount := cycleCount + UInt(1);

  // Delay timer
  // -----------

  // Used to implement the delay operation and to insert random
  // delays between load-reserve and store-conditional commands.

  // A 16-bit timer is plenty
  val delayTimer = Module(new DynamicTimer(16))

  // Used to generate a random delay period
  val randDelayBase = Module(new LCG16()).io.out

  // Random delay period: usually small, occasionally big
  val randDelay = Frequency(List(
    (14, UInt(0, 13) ## randDelayBase(2, 0)),
    (2,  UInt(0, 11) ## randDelayBase(5, 0))))

  // Default inputs
  delayTimer.io.start  := Bool(false)
  delayTimer.io.period := randDelay
  delayTimer.io.stop   := Bool(false)

  // Operation dispatch
  // ------------------

  // Hardware thread id
  val tid = UInt(id, numBitsInId)

  // Request & response count
  val reqCount  = Reg(init = UInt(0, 32))
  val respCount = Reg(init = UInt(0, 32))

  // Current operation being executed
  val currentOp = Reg(init = opNop)

  // If larger than 0, a multi-cycle operation is in progress.
  // Value indicates stage of progress.
  val opInProgress = Reg(init = UInt(0, 2))

  // Indicate when a fresh request is to be sent
  val sendFreshReq = Wire(Bool())
  sendFreshReq := Bool(false)

  // Used to generate unique data values
  val nextData = Reg(init = UInt(1, numBitsInWord-numBitsInId))

  // Registers for all the interesting parts of a request
  val reqValid = Reg(init = Bool(false))
  val reqAddr  = Reg(init = UInt(0, numBitsInWord))
  val reqData  = Reg(init = UInt(0, numBitsInWord))
  val reqCmd   = Reg(init = UInt(0, 5))
  val reqTag   = Reg(init = UInt(0, 7))

   // Condition on being allowed to send a fresh request
  val canSendFreshReq = (!reqValid || io.mem.req.fire()) &&
                          tagMan.io.available

  // Operation dispatch
  when (reqCount < UInt(numReqsPerGen)) {

    // No-op
    when (currentOp === opNop) {
      // Move on to a new operation
      currentOp := randOp
    }

    // Fence
    when (currentOp === opFence) {
      when (opInProgress === UInt(0) && !reqValid) {
        // Emit fence request
        printf("%d: fence-req @%d\n", tid, cycleCount)
        // Multi-cycle operation now in progress
        opInProgress := UInt(1)
      }
      // Wait until all requests have had a response
      .elsewhen (reqCount === respCount) {
        // Emit fence response
        printf("%d: fence-resp @%d\n", tid, cycleCount)
        // Move on to a new operation
        currentOp := randOp
        // Operation finished
        opInProgress := UInt(0)
      }
    }

    // Delay
    when (currentOp === opDelay) {
      when (opInProgress === UInt(0)) {
        // Start timer
        delayTimer.io.start := Bool(true)
        // Multi-cycle operation now in progress
        opInProgress := UInt(1)
      }
      .elsewhen (delayTimer.io.timeout) {
        // Move on to a new operation
        currentOp := randOp
        // Operation finished
        opInProgress := UInt(0)
      }
    }
  
    // Load, store, or atomic swap
    when (currentOp === opLoad  ||
          currentOp === opStore ||
          currentOp === opSwap) {
      when (canSendFreshReq) {
        // Set address
        reqAddr := randAddr
        // Set command
        when (currentOp === opLoad) {
          reqCmd := M_XRD
        } .elsewhen (currentOp === opStore) {
          reqCmd := M_XWR
        } .elsewhen (currentOp === opSwap) {
          reqCmd := M_XA_SWAP
        }
        // Send request
        sendFreshReq := Bool(true)
        // Move on to a new operation
        currentOp := randOp
      }
    }
  
    // Load-reserve and store-conditional
    // First issue an LR, then delay, then issue an SC
    when (currentOp === opLRSC) {
      // LR request has not yet been sent
      when (opInProgress === UInt(0)) {
        when (canSendFreshReq) {
          // Set address and command
          reqAddr := randAddr
          reqCmd  := M_XLR
          // Send request
          sendFreshReq := Bool(true)
          // Multi-cycle operation now in progress
          opInProgress := UInt(1)
        }
      }
      // LR request has been sent, start delay timer
      when (opInProgress === UInt(1)) {
        // Start timer
        delayTimer.io.start := Bool(true)
        // Indicate that delay has started
        opInProgress := UInt(2)
      }
      // Delay in progress
      when (opInProgress === UInt(2)) {
        when (delayTimer.io.timeout) {
          // Delay finished
          opInProgress := UInt(3)
        }
      }
      // Delay finished, send SC request
      //when (opInProgress === UInt(3)) {
      when (opInProgress === UInt(3) && !reqValid && reqCount === respCount) {
        when (canSendFreshReq) {
          // Set command, but leave address
          // i.e. use same address as LR did
          reqCmd  := M_XSC
          // Send request
          sendFreshReq := Bool(true)
          // Multi-cycle operation finished
          opInProgress := UInt(0)
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
    reqTag := Cat(UInt(0), tagMan.io.tagOut)
    tagMan.io.take := Bool(true)
    // Fill in unique data
    reqData := Cat(nextData, tid)
    nextData := nextData + UInt(1)
    // Request is good to go!
    reqValid := Bool(true)
    // Increment request count
    reqCount := reqCount + UInt(1)
  }
  .elsewhen (io.mem.req.fire()) {
    // Request has been sent and there is no new request ready
    reqValid := Bool(false)
  }

  // Wire up interface to memory
  io.mem.req.valid     := reqValid
  io.mem.req.bits.addr := reqAddr + UInt(baseAddr)
  io.mem.req.bits.data := reqData
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.cmd  := reqCmd
  io.mem.req.bits.tag  := reqTag

  // On cycle when request is actually sent, print it
  when (io.mem.req.fire()) {
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
    tagMan.io.put   := Bool(true)
    // Print response
    printf("%d: resp %d #%d @%d\n", tid,
      io.mem.resp.bits.data, io.mem.resp.bits.tag, cycleCount)
    // Increment response count
    respCount := respCount + UInt(1)
  }

  // Response timeouts
  // ---------------------------

  // Raise an error if a response takes too long to come back
  val timeout = Timer(memRespTimeout, sendFreshReq, io.mem.resp.valid)
  assert(!timeout, s"Core ${id}: response timeout")

  // Termination condition
  // ---------------------

  val done = reqCount  === UInt(numReqsPerGen) &&
             respCount === UInt(numReqsPerGen)

  val donePulse = done && !Reg(init = Bool(false), next = done)

  // Emit that this thread has completed
  when (donePulse) {
    printf(s"FINISHED ${numGens}\n")
  }

  io.finished := Bool(false)
}

// =======================
// Trace-generator wrapper
// =======================

class GroundTestTraceGenerator(id: Int)(implicit p: Parameters)
    extends GroundTest()(p) with HasTraceGenParams {

  val traceGen = Module(new TraceGenerator(id))
  io.cache.head <> traceGen.io.mem

  io.finished := traceGen.io.finished
}
