// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.AMBAProt
import scala.math.{min,max}

case class TLBroadcastControlParams(
  address:   AddressSet,
  beatBytes: Int)

/**
  * @param lineBytes cacheline size
  * @param numTrackers maximum transaction size to record on each port.
  * @param bufferless if this is true. ready signal in A channel will directly decoupled from manger to client.
  * @param control Add a control node for filter notification.
  */
case class TLBroadcastParams(
  lineBytes:     Int,
  numTrackers:   Int = 4,
  bufferless:    Boolean = false,
  control:       Option[TLBroadcastControlParams] = None,
  filterFactory: TLBroadcast.ProbeFilterFactory = BroadcastFilter.factory)
{
  require (lineBytes > 0 && isPow2(lineBytes))
  require (numTrackers > 0)
}

/** [[TLBroadcast]] is the last level coherence manger for TileLink. Simplest topology can be this:
  * {{{
  *   A           B
  *   │           │
  *   └─────┬─────┘
  *         │
  *     Broadcast
  *         │
  *   ┌─────┴─────┐
  *   │           │
  *   A'          B'
  *   └─────┬─────┘
  *       XBar
  *         │
  *         C
  * }}}
  *
  * It probes clients cachelines to maintain coherence.
  * It can store or not store directory, depending on the implementation of [[ProbeFilter]] in this Broadcast.
  *
  * @example Here is some example transactions on the broadcast.
  *          if A is Tip, B acquire this cacheline:
  *          - (t0) B -> BH: AcquireBlock
  *          - (t1) BH -> A: ProbeBlock
  *          - (t1) A -> BH: ProbeAckData/ProbeAck
  *          -   (t2) if ProbeAckData: BH -> C: PutFullData
  *          -   (t2) if ProbeAckData: C -> BH: AccessAck
  *          - (t3) BH -> C: Get
  *          - (t3) C -> BH: AccessAckData
  *          - (t0) BH -> B: GrantData
  *          - (t0) B -> BH: GrantAck
  *
  *          if A is Tip, A need to release TtoB:
  *          - (t0) A -> BH: ReleaseData/Release TtoB
  *          - (t1) BH -> C: Put (payload [[TLBroadcastConstants.TRANSFORM_B]] in the highest bits of source field.)
  *          - (t1) C -> BH: AccessAck (check the highest bit to be 1 or 0, here is 1 since manger is replying to [[TLBroadcastConstants.TRANSFORM_B]] sourceId.))
  *          - (t0) BH -> A: ReleaseAck
  *
  * Behavior of [[TLBroadcast]]:
  * receive A channel from client:
  * PutFullData    : to manger A channel -> PutFullData with payload [[TLBroadcastConstants.PASS]] in highest 2 bits of source.
  * PutPartialData : to manger A channel -> PutPartialData with payload [[TLBroadcastConstants.PASS]] in highest 2 bits of source.
  * ArithmeticLogic: to manger A channel -> ArithmeticLogic with payload [[TLBroadcastConstants.PASS]] in highest 2 bits of source.
  * LogicalData    : to manger A channel -> LogicalData with payload [[TLBroadcastConstants.PASS]] in highest 2 bits of source.
  * Get            : to manger A channel -> Get with payload [[TLBroadcastConstants.PASS]] in highest 2 bits of source.
  * Intent         : to manger A channel -> Intent with payload [[TLBroadcastConstants.PASS]] in highest 2 bits of source.
  * AcquireBlock   : to manger A channel -> Get with payload in highest 2 bits of source.
  *                                         payload depends on `a_param`:
  *                                         if `a_param` is `NtoB`, payload is [[TLBroadcastConstants.TRANSFORM_B]]
  *                                         if `a_param` is `NtoT` or `BtoT`, payload is [[TLBroadcastConstants.TRANSFORM_T]]
  * AcquirePerm    : to manger A channel -> Get with payload in highest 2 bits of source.
  *                                         payload depends on `a_param`:
  *                                         if `a_param` is `NtoB`, payload is [[TLBroadcastConstants.TRANSFORM_B]]
  *                                         if `a_param` is `NtoT` or `BtoT`, payload is [[TLBroadcastConstants.TRANSFORM_T]]
  *
  * receive C channel from client:
  * ProbeAck       : notify [[TLBroadcastTracker.io.probenack]](Probe with no data is finished.)
  * ProbeAckData   : to manger A channel -> PutFullData with payload [[TLBroadcastConstants.DROP]] in highest 2 bits of source.
  * Release        : to client D channel -> ReleaseAck
  *                  notify [[ProbeFilter.io.release]]
  * ReleaseData    : PutFullData with payload [[TLBroadcastConstants.TRANSFORM_B]] in highest 2 bits of source.
  *                  notify [[ProbeFilter.io.release]]
  *
  * receive E channel from client:
  * GrantAck       : accept only
  *                  notify [[TLBroadcastTracker.io.e_last]]
  *
  * receive D channel from manger:
  * if payload in highest 2 bits of source is [[TLBroadcastConstants.PASS]]:
  *   AccessAck      : to manger D channel -> AccessAck
  *   AccessAckData  : to manger D channel -> AccessAckData
  *   HintAck        : to manger D channel -> HintAck
  * if payload in highest 2 bits of source is [[TLBroadcastConstants.DROP]]:
  *   AccessAck      : notify [[TLBroadcastTracker.io.probedack]](replying ProbeAckData is finished.)
  * if payload in highest 2 bits of source is [[TLBroadcastConstants.TRANSFORM_B]]:
  *   AccessAck      : to manger D channel -> TLMessages.ReleaseAck with `d_param` [[TLPermissions.toB]](replying to AcquirePerm)
  *   AccessAckData  : to manger D channel -> TLMessages.GrantData with `d_param` [[TLPermissions.toB]](replying to AcquireBlock)
  * if payload in highest 2 bits of source is [[TLBroadcastConstants.TRANSFORM_T]]:
  *   AccessAck      : to manger D channel -> TLMessages.ReleaseAck with `d_param` [[TLPermissions.toT]](replying to AcquirePerm)
  *   AccessAckData  : to manger D channel -> TLMessages.GrantData with `d_param` [[TLPermissions.toT]](replying to AcquireBlock)
  *
  * send to client in B channel:
  * Probe cachelines in `probe_todo` which is updated by [[ProbeFilter.io.response]].
  *
  * send to client in D channel:
  * Convert D transaction from D channel of manger.
  *
  */
class TLBroadcast(params: TLBroadcastParams)(implicit p: Parameters) extends LazyModule
{
  // Backwards compatibility
  def this(lineBytes: Int, numTrackers: Int = 4, bufferless: Boolean = false, filterFactory: TLBroadcast.ProbeFilterFactory = BroadcastFilter.factory)(implicit p: Parameters) =
    this(TLBroadcastParams(lineBytes, numTrackers, bufferless, filterFactory=filterFactory))

  val node = TLAdapterNode(
    clientFn  = { cp =>
      cp.v1copy(clients = Seq(TLMasterParameters.v1(
        name     = "TLBroadcast",
        /* Add 2 bits encoded in [[TLBroadcastConstants]] to record transaction type for each sourceId in clients. */
        sourceId = IdRange(0, 1 << log2Ceil(cp.endSourceId*4)))))
    },
    managerFn = { mp =>
      mp.v1copy(
        endSinkId  = params.numTrackers,
        managers   = mp.managers.map { m =>
          /** This is the last level coherence manager, so it cannot be probed by mangers. */
          require (!m.supportsAcquireB)
          /* We only manage addresses which are uncached. */
          if (m.regionType == RegionType.UNCACHED) {
            /* The device had better support line transfers.
             * @todo why?
             */
            val lowerBound = max(m.supportsPutFull.min, m.supportsGet.min)
            require (!m.supportsPutFull || m.supportsPutFull.contains(params.lineBytes), s"${m.name} only supports PutFull(${m.supportsPutFull}), which does not include ${params.lineBytes}")
            require (!m.supportsGet     || m.supportsGet    .contains(params.lineBytes), s"${m.name} only supports Get(${m.supportsGet}), which does not include ${params.lineBytes}")
            m.v1copy(
              regionType         = RegionType.TRACKED,
              supportsAcquireB   = TransferSizes(lowerBound, params.lineBytes),
              /* dirty data write back to ddr in a transaction. */
              supportsAcquireT   = if (m.supportsPutFull) TransferSizes(lowerBound, params.lineBytes) else TransferSizes.none,
              alwaysGrantsT      = false,
              /* truncate supported accesses to lineBytes (we only ever probe for one line). */
              supportsPutFull    = TransferSizes(m.supportsPutFull   .min, min(m.supportsPutFull   .max, params.lineBytes)),
              supportsPutPartial = TransferSizes(m.supportsPutPartial.min, min(m.supportsPutPartial.max, params.lineBytes)),
              supportsGet        = TransferSizes(m.supportsGet       .min, min(m.supportsGet       .max, params.lineBytes)),
              supportsHint       = TransferSizes(m.supportsHint      .min, min(m.supportsHint      .max, params.lineBytes)),
              supportsArithmetic = TransferSizes(m.supportsArithmetic.min, min(m.supportsArithmetic.max, params.lineBytes)),
              supportsLogical    = TransferSizes(m.supportsLogical   .min, min(m.supportsLogical   .max, params.lineBytes)),
              /* trackers do not respond in FIFO order! */
              fifoId             = None
            )
          } else {
            m
          }
        }
      )
    }
  )

  /** Interrupt Node for filter. Each Filter has a interrupt. */
  val intNode = if (params.control.isEmpty) None else Some(IntSourceNode(IntSourcePortSimple(num = 1)))
  /** Control Node for filter.
    * [[ProbeFilter.useRegFields]] can be defined to add register to control.
    */
  val controlNode = params.control.map(x => TLRegisterNode(
    address   = Seq(x.address),
    beatBytes = x.beatBytes,
    device    = new SimpleDevice("cache-controller", Seq("sifive,broadcast0"))))

  lazy val module = new LazyModuleImp(this) {
    /* Number of inward node will decide the number bank. */
    val (ints, fields) = node.in.zip(node.out).zipWithIndex.map { case (((in, edgeIn), (out, edgeOut)), bankIndex) =>
      val clients = edgeIn.client.clients
      val managers = edgeOut.manager.managers
      val lineShift = log2Ceil(params.lineBytes)

      import TLBroadcastConstants._

      /** Since [[TLBroadcast]] doesn't implements PutPartialData,
        * cacheline size must not be less than manger beatBytes.
        */
      require (params.lineBytes >= edgeOut.manager.beatBytes)
      /** This identify all the caches for the probe walker.  */
      val caches: Seq[IdRange] = clients.filter(_.supports.probe).map(_.sourceId)
      /** Use start bit in IdRange to locate which client is target. */
      val cache_targets: Seq[UInt] = caches.map(c => c.start.U)

      /** Create the probe filter, to filter B channel and D channel transactions. */
      val flatAddresses: Seq[AddressSet] = AddressSet.unify(edgeOut.manager.managers.flatMap(_.address))
      val addressMask: BigInt = AddressDecoder(flatAddresses.map(Seq(_)), flatAddresses.map(_.mask).reduce(_|_))
      /** create [[ProbeFilter]] instance. */
      val filter = Module(params.filterFactory(ProbeFilterParams(
        mshrs  = params.numTrackers,
        caches = caches.size,
        maxAddress  = edgeIn.manager.maxAddress,
        addressMask = addressMask & ~(params.lineBytes-1))))

      /** Create the request tracker queues. */
      val trackers = Seq.tabulate(params.numTrackers) { id =>
        Module(new TLBroadcastTracker(id, params.lineBytes, caches.size, params.bufferless, edgeIn, edgeOut)).io
      }

      /* Client E Channel:
       * Always accept E.
       */
      in.e.ready := true.B
      /* Notify tracker received E of a transaction.
       * In to D channel handling logic of client, MSHR(currently tracker ID) is encoded in the sink field,
       * client will reply it in the sink.
       * this decode sink from E channel, then notify correspond tracker.
       */
      (trackers zip UIntToOH(in.e.bits.sink).asBools) foreach { case (tracker, select) =>
        tracker.e_last := select && in.e.fire()
      }

      /* Client D Channel:
       * Forward or transform manger D channel based on the payload of the source.
       */
      /** Highest sourceId index, using endSourceId to detect. */
      val d_high = log2Ceil(edgeIn.client.endSourceId)
      /** payload extracted from source. */
      val d_what = out.d.bits.source(d_high+1, d_high)
      /** indicate this transaction should be dropped:
        * Probe client -> client ProbeAckData -> Put manger(DROP) -> manger AccessAck(DROP)
        */
      val d_drop = d_what === DROP
      /** indicate this transaction has an available data field. */
      val d_hasData = edgeOut.hasData(out.d.bits)
      /* D channel to response to clients. */
      val d_normal = Wire(in.d)
      /** [[d_first]] is the update enable signal of [[d_trackerOH]].
        * [[d_last]] tell tracker and filter the end of transaction in D channel.
        */
      val (d_first, d_last, _) = edgeIn.firstlast(d_normal)
      /** select tracker with:
        * 1. tracker need d reply
        * 2. tracker source is same with the client source.
        *
        * This is used for:
        * 1. encode tracker id into sink of D, make client reply in E to this tracker.
        * 2. filter will need this D.
        */
      val d_trackerOH = VecInit(trackers.map { t => t.need_d && t.source === d_normal.bits.source }).asUInt holdUnless d_first

      /** if d is valid, and not drop. */
      assert (!out.d.valid || !d_drop || out.d.bits.opcode === TLMessages.AccessAck)

      /** [[d_allow]] is the signal allowing accept transaction from manger. */
      val d_allow = Wire(Bool())
      /* if [[d_drop]], accept it and don't pass it to client.
       * if not [[d_drop]], wait for client ready.
       */
      out.d.ready := (d_normal.ready && d_allow) || d_drop
      /* only pass to client when manger is valid and not [[d_drop]]. */
      d_normal.valid := out.d.valid && d_allow && !d_drop
      /* size of source in client is less 2 bits than manger.
       * truncates the higher 2 bits in source.
       */
      d_normal.bits := out.d.bits

      /* modify opcode and param with d_what:
       * if [[d_what(1)]] is high, this transaction is a TL-C transaction:
       * if [[d_hasData]], transaction is: client -> BH: Acquire.
       * if not [[d_hasData]], transaction is: client -> BH: ReleaseData.
       *
       * [[d_what(0)]] will decide which target permission is.
       */
      when (d_what(1)) {
        d_normal.bits.opcode := Mux(d_hasData, TLMessages.GrantData, TLMessages.ReleaseAck)
        d_normal.bits.param  := Mux(d_hasData, Mux(d_what(0), TLPermissions.toT, TLPermissions.toB), 0.U)
      }
      /** Miss Stats Handler Register of D, directly encode from tracker id. */
      val d_mshr = OHToUInt(d_trackerOH)

      /* ask client to reply in E channel with the [[d_mshr]] as source. */
      d_normal.bits.sink := d_mshr
      assert (!d_normal.valid || (d_trackerOH.orR() || d_normal.bits.opcode === TLMessages.ReleaseAck))

      /* Need to response to client.
       * A tracker response is anything neither dropped nor a ReleaseAck.
       *
       * if [[d_hasData]], there are data must be reply to client.
       * if is not TL-C transaction, just pass from manger to client.
       */
      val d_response = d_hasData || !d_what(1)

      (trackers zip d_trackerOH.asBools) foreach { case (tracker, select) =>
        /* last bit in D channel reply to client:
         * 1. This tracker is selected
         * 2. Reply to client in D is fired.
         * 3. Need to response to client.
         *    if the transaction is Release -> ReleaseAck, tracker won't track this transaction, and [[d_response]] is false.
         * 4. this is the last beat of this transaction.
         */
        tracker.d_last := select && d_normal.fire() && d_response && d_last
        /* C channel is ProbeAckData or Release. */
        tracker.probedack := select && out.d.fire() && d_drop
      }

      d_allow := filter.io.update.ready || !d_response || !d_last
      filter.io.update.valid := out.d.valid && d_normal.ready && !d_drop && d_response && d_last
      filter.io.update.bits.mshr := d_mshr
      filter.io.update.bits.gaveT := d_what === TRANSFORM_T
      filter.io.update.bits.cacheOH := Mux1H(d_trackerOH, trackers.map(_.cacheOH))

      /* C Channel */
      val c_probeack     = in.c.bits.opcode === TLMessages.ProbeAck
      val c_probeackdata = in.c.bits.opcode === TLMessages.ProbeAckData
      val c_releasedata  = in.c.bits.opcode === TLMessages.ReleaseData
      val c_release      = in.c.bits.opcode === TLMessages.Release
      val c_trackerOH    = trackers.map { t => t.line === (in.c.bits.address >> lineShift) }
      val c_trackerSrc   = Mux1H(c_trackerOH, trackers.map { _.source })

      // Record if this inner cache no longer has the block
      val whoC = if (caches.size == 0) 0.U else Cat(caches.map(_.contains(in.c.bits.source)).reverse)
      val CisN = in.c.bits.param === TLPermissions.TtoN ||
                 in.c.bits.param === TLPermissions.BtoN ||
                 in.c.bits.param === TLPermissions.NtoN
      val clearOH = Mux(in.c.fire() && (c_probeack || c_probeackdata) && CisN, whoC, 0.U)

      // Decrement the tracker's outstanding probe counter
      (trackers zip c_trackerOH) foreach { case (tracker, select) =>
        tracker.clearOH := Mux(select, clearOH, 0.U)
        tracker.probenack := in.c.fire() && c_probeack && select
        tracker.probesack := in.c.fire() && select && (c_probeack || c_probeackdata) && (
          in.c.bits.param === TLPermissions.TtoB ||
          in.c.bits.param === TLPermissions.BtoB)
      }

      val releaseack = Wire(in.d)
      val putfull = Wire(out.a)

      in.c.ready := c_probeack || Mux(c_release, releaseack.ready, putfull.ready)

      val c_first = edgeIn.first(in.c)
      filter.io.release.valid := in.c.valid && c_first && (c_releasedata || c_release)
      filter.io.release.bits.address := in.c.bits.address
      filter.io.release.bits.keepB   := in.c.bits.param === TLPermissions.TtoB
      filter.io.release.bits.cacheOH := whoC

      releaseack.valid := in.c.valid && (filter.io.release.ready || !c_first) && c_release
      releaseack.bits  := edgeIn.ReleaseAck(in.c.bits)

      val put_what = Mux(c_releasedata, TRANSFORM_B, DROP)
      val put_who  = Mux(c_releasedata, in.c.bits.source, c_trackerSrc)
      putfull.valid := in.c.valid && (c_probeackdata || (c_releasedata && (filter.io.release.ready || !c_first)))
      putfull.bits := edgeOut.Put(Cat(put_what, put_who), in.c.bits.address, in.c.bits.size, in.c.bits.data)._2
      putfull.bits.user.lift(AMBAProt).foreach { x =>
        x.fetch       := false.B
        x.secure      := true.B
        x.privileged  := true.B
        x.bufferable  := true.B
        x.modifiable  := true.B
        x.readalloc   := true.B
        x.writealloc  := true.B
      }

      // Combine ReleaseAck or the modified D
      TLArbiter.lowest(edgeOut, in.d, releaseack, d_normal)
      // Combine the PutFull with the trackers
      TLArbiter.lowestFromSeq(edgeOut, out.a, putfull +: trackers.map(_.out_a))

      /** B Channel
        * The Probe FSM walks all caches and probes them
        */
      val probe_todo = RegInit(0.U(max(1, caches.size).W))
      val probe_line = Reg(UInt())
      val probe_perms = Reg(UInt(2.W))
      val probe_next = probe_todo & ~(leftOR(probe_todo) << 1)
      val probe_busy = probe_todo.orR()
      val probe_target = if (caches.size == 0) 0.U else Mux1H(probe_next, cache_targets)

      // Probe whatever the FSM wants to do next
      in.b.valid := probe_busy
      if (caches.size != 0) {
        in.b.bits := edgeIn.Probe(probe_line << lineShift, probe_target, lineShift.U, probe_perms)._2
      }
      when (in.b.fire()) { probe_todo := probe_todo & ~probe_next }

      /* A Channel. */
      // Which cache does a request come from?
      val a_cache = if (caches.size == 0) 0.U else VecInit(caches.map(_.contains(in.a.bits.source))).asUInt
      val a_first = edgeIn.first(in.a)

      // To accept a request from A, the probe FSM must be idle and there must be a matching tracker
      val freeTrackers = VecInit(trackers.map { t => t.idle }).asUInt
      val freeTracker = freeTrackers.orR()
      val matchTrackers = VecInit(trackers.map { t => t.line === in.a.bits.address >> lineShift }).asUInt
      val matchTracker = matchTrackers.orR()
      val allocTracker = freeTrackers & ~(leftOR(freeTrackers) << 1)
      val selectTracker = Mux(matchTracker, matchTrackers, allocTracker)
      val trackerReadys = VecInit(trackers.map(_.in_a.ready)).asUInt
      val trackerReady = (selectTracker & trackerReadys).orR()

      in.a.ready := (!a_first || filter.io.request.ready) && trackerReady
      (trackers zip selectTracker.asBools) foreach { case (t, select) =>
        t.in_a.valid := in.a.valid && select && (!a_first || filter.io.request.ready)
        t.in_a.bits := in.a.bits
        t.in_a_first := a_first
      }

      filter.io.request.valid := in.a.valid && a_first && trackerReady
      filter.io.request.bits.mshr    := OHToUInt(selectTracker)
      filter.io.request.bits.address := in.a.bits.address
      filter.io.request.bits.needT   := edgeIn.needT(in.a.bits)
      filter.io.request.bits.allocOH := a_cache // Note: this assumes a cache doing MMIO has allocated

      val leaveB = !filter.io.response.bits.needT && !filter.io.response.bits.gaveT
      val others = filter.io.response.bits.cacheOH & ~filter.io.response.bits.allocOH
      val todo = Mux(leaveB, 0.U, others)
      filter.io.response.ready := !probe_busy
      when (filter.io.response.fire()) {
        probe_todo  := todo
        probe_line  := filter.io.response.bits.address >> lineShift
        probe_perms := Mux(filter.io.response.bits.needT, TLPermissions.toN, TLPermissions.toB)
      }

      // Inform the tracker of the number of probe targets
      val responseCache = filter.io.response.bits.cacheOH | filter.io.response.bits.allocOH
      val responseCount = PopCount(todo)
      val responseMSHR = UIntToOH(filter.io.response.bits.mshr, params.numTrackers).asBools
      val sack = filter.io.response.fire() && leaveB && others =/= 0.U
      (trackers zip responseMSHR) foreach { case (tracker, select) =>
        tracker.probe.valid := filter.io.response.fire() && select
        tracker.probe.bits.count   := responseCount
        tracker.probe.bits.cacheOH := responseCache
        when (sack && select) { tracker.probesack := true.B }
      }

      // The outer TL connections may not be cached
      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B

      // Collect all the filters together
      (filter.io.int, controlNode match {
        case Some(x) => filter.useRegFields(bankIndex)
        case None    => { filter.tieRegFields(bankIndex); Nil }
      })
    }.unzip

    // Hook up the filter interfaces, if they are requested
    intNode.foreach { _.out(0)._1(0) := ints.reduce(_||_) }
    controlNode.foreach { _.regmap(fields.flatten:_*) }
  }
}

// maxAddress is the largest legal physical address that will be filtered
// addressMask is set for those bits which can actually toggle in an address; ie:
//   - offset bits are 0
//   - bank bits are 0
//   - bits unused by any actual slave are 0
case class ProbeFilterParams(mshrs: Int, caches: Int, maxAddress: BigInt, addressMask: BigInt)
{
  require (mshrs >= 0)
  require (caches >= 0)
  require (maxAddress > 0)
  require (addressMask != 0)

  val mshrBits = log2Ceil(mshrs)
  val addressBits = log2Ceil(maxAddress)

  require ((addressMask >> addressBits) == 0)

  private def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
  val addressMaskBitList: List[Boolean] = bigBits(addressMask)
  val addressMaskBits: Int = addressMaskBitList.filter(x => x).size

  def maskBits(address: UInt) = Cat((addressMaskBitList zip address.asBools).filter(_._1).map(_._2).reverse)
}

class ProbeFilterRequest(val params: ProbeFilterParams) extends Bundle {
  val mshr    = UInt(params.mshrBits.W)
  val address = UInt(params.addressBits.W)
  val allocOH = UInt(params.caches.W)
  val needT   = Bool()
}

class ProbeFilterResponse(params: ProbeFilterParams) extends ProbeFilterRequest(params) {
  val gaveT   = Bool()
  val cacheOH = UInt(params.caches.W)
}

class ProbeFilterUpdate(val params: ProbeFilterParams) extends Bundle {
  val mshr    = UInt(params.mshrBits.W)
  val gaveT   = Bool()
  val cacheOH = UInt(params.caches.W)
}

class ProbeFilterRelease(val params: ProbeFilterParams) extends Bundle {
  val address = UInt(params.addressBits.W)
  val keepB   = Bool()
  val cacheOH = UInt(params.caches.W)
}

class ProbeFilterIO(val params: ProbeFilterParams) extends Bundle {
  val int = Output(Bool())
  val request = Flipped(Decoupled(new ProbeFilterRequest(params)))
  val response = Decoupled(new ProbeFilterResponse(params))
  val update  = Flipped(Decoupled(new ProbeFilterUpdate(params)))
  val release = Flipped(Decoupled(new ProbeFilterRelease(params)))
}

abstract class ProbeFilter(val params: ProbeFilterParams) extends MultiIOModule {
  def useRegFields(bankIndex: Int): Seq[RegField.Map] = Nil
  def tieRegFields(bankIndex: Int): Unit = Unit
  val io = IO(new ProbeFilterIO(params))
}

class BroadcastFilter(params: ProbeFilterParams) extends ProbeFilter(params) {
  io.int := false.B

  io.request.ready := io.response.ready
  io.response.valid := io.request.valid

  io.response.bits.mshr    := io.request.bits.mshr
  io.response.bits.address := io.request.bits.address
  io.response.bits.needT   := io.request.bits.needT
  io.response.bits.allocOH := io.request.bits.allocOH
  io.response.bits.gaveT   := true.B
  if (params.caches > 0)
    io.response.bits.cacheOH := ~0.U(params.caches.W)

  io.update.ready := true.B
  io.release.ready := true.B
}

object BroadcastFilter {
  def factory: TLBroadcast.ProbeFilterFactory = params => new BroadcastFilter(params)
}

object TLBroadcast
{
  type ProbeFilterFactory = ProbeFilterParams => ProbeFilter
  def apply(lineBytes: Int, numTrackers: Int = 4, bufferless: Boolean = false, filterFactory: TLBroadcast.ProbeFilterFactory = BroadcastFilter.factory)(implicit p: Parameters): TLNode =
    apply(TLBroadcastParams(lineBytes, numTrackers, bufferless, filterFactory=filterFactory))
  def apply(params: TLBroadcastParams)(implicit p: Parameters): TLNode =
  {
    val broadcast = LazyModule(new TLBroadcast(params))
    broadcast.node
  }
}

class ProbeTrackInfo(val caches: Int) extends Bundle {
  /** how many clients left need to be probed. */
  val count   = UInt(log2Ceil(caches+1).W)
  /** probe which client now. */
  val cacheOH = UInt(caches.W)
}

/** Tracker to track trans */
class TLBroadcastTracker(id: Int, lineBytes: Int, caches: Int, bufferless: Boolean, edgeIn: TLEdgeIn, edgeOut: TLEdgeOut) extends Module
{
  val io = IO(new Bundle {
    /** first packet come from a. */
    val in_a_first: Bool = Input(Bool())
    /** incoming A channel from clients. */
    val in_a: DecoupledIO[TLBundleA] = Flipped(Decoupled(new TLBundleA(edgeIn.bundle)))
    /** output A channel to mangers. */
    val out_a = Decoupled(new TLBundleA(edgeOut.bundle))
    /** @todo */
    val probe = Input(Valid(new ProbeTrackInfo(caches)))
    /** nack: No Data ACK. */
    val probenack = Input(Bool())
    /** dack: ACK with Data. */
    val probedack = Input(Bool())
    /** sack: shared ack. */
    val probesack = Input(Bool())

    /** transaction in d has finished. */
    val d_last = Input(Bool())
    /** transaction in e has finished. */
    val e_last = Input(Bool())
    /** the source awaiting D response. */
    val source = Output(UInt())
    /** the line waiting for probes. */
    val line = Output(UInt())
    /** this module is idle. */
    val idle = Output(Bool())
    /** wait for transcation in d channel. */
    val need_d = Output(Bool())
    /** @todo */
    val cacheOH = Output(UInt(caches.W))
    /** @todo */
    val clearOH = Input(UInt(caches.W))
  })

  val lineShift = log2Ceil(lineBytes)
  import TLBroadcastConstants._

  /* Only one operation can be inflight per line
   *
   * since we need to force the order:
   * this probe -> request -> next probes
   * @todo what is request?
   */

  /** state we got transaction in e channel.
    * client -> manger receive state has ended.
    */
  val got_e   = RegInit(true.B)
  /** state we sent transaction in d channel.
    * client -> manger send state has ended
    */
  val sent_d  = RegInit(true.B)
  /** indicate a transaction is for branch operation. */
  val shared  = Reg(Bool())
  /* cache transaction. */
  val opcode  = Reg(io.in_a.bits.opcode)
  val param   = Reg(io.in_a.bits.param)
  val size    = Reg(io.in_a.bits.size)
  val source  = Reg(io.in_a.bits.source)
  val user    = Reg(io.in_a.bits.user)
  val echo    = Reg(io.in_a.bits.echo)
  /** @todo why this Init value? */
  val address = RegInit((id << lineShift).U(io.in_a.bits.address.getWidth.W))
  /** caches: how many cache from upper level.
    * 1: need to fetch from lower level
    * */
  val count   = Reg(UInt(log2Ceil(caches+1).W))
  val cacheOH = Reg(UInt(caches.W))
  val idle    = got_e && sent_d

  when (io.in_a.fire() && io.in_a_first) {
    assert (idle)
    sent_d  := false.B
    shared  := false.B
    /* If incoming transaction is AcquireBlock or AcquirePerm, we need to wait for e. */
    got_e   := io.in_a.bits.opcode =/= TLMessages.AcquireBlock && io.in_a.bits.opcode =/= TLMessages.AcquirePerm
    opcode  := io.in_a.bits.opcode
    param   := io.in_a.bits.param
    size    := io.in_a.bits.size
    source  := io.in_a.bits.source
    user   :<= io.in_a.bits.user
    echo   :<= io.in_a.bits.echo
    address := io.in_a.bits.address
    /* we need to fetch from lower level at least. */
    count   := 1.U
  }
  /* cache [[cacheOH]] by [[io.clearOH]] */
  cacheOH := cacheOH & ~io.clearOH

  /* if we are probing to other clients. */
  when (io.probe.valid) {
    count   := io.probe.bits.count
    cacheOH := io.probe.bits.cacheOH
  }

  when (io.d_last) {
    assert (!sent_d)
    sent_d := true.B
  }

  when (io.e_last) {
    assert (!got_e)
    got_e := true.B
  }

  /* send probe transactions. */
  when (io.probenack || io.probedack) {
    assert (count > 0.U)
    count := count - Mux(io.probenack && io.probedack, 2.U, 1.U)
  }

  /* we have branch in the upper level. */
  when (io.probesack) {
    shared := true.B
  }

  io.idle := idle
  io.need_d := !sent_d
  io.source := source
  io.line := address >> lineShift
  io.cacheOH := cacheOH

  /* only have mask and data.
   * only in idle state, tracker will accept new transaction set.
   */
  val i_data = Wire(Decoupled(new TLBroadcastData(edgeIn.bundle)))
  val o_data = Queue(i_data, if (bufferless) 1 else (lineBytes / edgeIn.manager.beatBytes), pipe=bufferless)

  io.in_a.ready := (idle || !io.in_a_first) && i_data.ready
  i_data.valid := (idle || !io.in_a_first) && io.in_a.valid
  i_data.bits.mask := io.in_a.bits.mask
  i_data.bits.data := io.in_a.bits.data

  /* finish this all probe and fetch data from next level. */
  val probe_done = count === 0.U
  /** from upper level this transaction is a Acquire. */
  val acquire = opcode === TLMessages.AcquireBlock || opcode === TLMessages.AcquirePerm

  /** if this transaction probe others to branch:
    * this is [[TRANSFORM_B]], otherwise [[TRANSFORM_T]]
    */
  val transform = Mux(shared, TRANSFORM_B, TRANSFORM_T)

  o_data.ready := io.out_a.ready && probe_done
  io.out_a.valid := o_data.valid && probe_done
  /** convert TL-C -> TL-UH. */
  io.out_a.bits.opcode  := Mux(acquire, TLMessages.Get, opcode)
  io.out_a.bits.param   := Mux(acquire, 0.U, param)
  io.out_a.bits.size    := size
  io.out_a.bits.source  := Cat(Mux(acquire, transform, PASS), source)
  io.out_a.bits.address := address
  io.out_a.bits.mask    := o_data.bits.mask
  io.out_a.bits.data    := o_data.bits.data
  io.out_a.bits.corrupt := false.B
  io.out_a.bits.user   :<= user
  io.out_a.bits.echo   :<= echo
}

object TLBroadcastConstants
{
  def TRANSFORM_T = 3.U
  def TRANSFORM_B = 2.U
  def DROP        = 1.U
  def PASS        = 0.U
}

class TLBroadcastData(params: TLBundleParameters) extends TLBundleBase(params)
{
  val mask = UInt((params.dataBits/8).W)
  val data = UInt(params.dataBits.W)
}
