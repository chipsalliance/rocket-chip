// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.AMBAProt
import scala.math.{min,max}

class TLBroadcast(lineBytes: Int, numTrackers: Int = 4, bufferless: Boolean = false, filterFactory: TLBroadcast.ProbeFilterFactory = BroadcastFilter.factory)(implicit p: Parameters) extends LazyModule
{
  require (lineBytes > 0 && isPow2(lineBytes))
  require (numTrackers > 0)

  val node = TLAdapterNode(
    clientFn  = { cp =>
      cp.v1copy(clients = Seq(TLMasterParameters.v1(
        name     = "TLBroadcast",
        sourceId = IdRange(0, 1 << log2Ceil(cp.endSourceId*4)))))
    },
    managerFn = { mp =>
      mp.v1copy(
        endSinkId  = numTrackers,
        managers   = mp.managers.map { m =>
          // We are the last level manager
          require (!m.supportsAcquireB)
          // We only manage addresses which are uncached
          if (m.regionType == RegionType.UNCACHED) {
            // The device had better support line transfers
            val lowerBound = max(m.supportsPutFull.min, m.supportsGet.min)
            require (!m.supportsPutFull || m.supportsPutFull.contains(lineBytes), s"${m.name} only supports PutFull(${m.supportsPutFull}), which does not include $lineBytes")
            require (!m.supportsGet     || m.supportsGet    .contains(lineBytes), s"${m.name} only supports Get(${m.supportsGet}), which does not include $lineBytes")
            m.v1copy(
              regionType         = RegionType.TRACKED,
              supportsAcquireB   = TransferSizes(lowerBound, lineBytes),
              supportsAcquireT   = if (m.supportsPutFull) TransferSizes(lowerBound, lineBytes) else TransferSizes.none,
              alwaysGrantsT      = false,
              // truncate supported accesses to lineBytes (we only ever probe for one line)
              supportsPutFull    = TransferSizes(m.supportsPutFull   .min, min(m.supportsPutFull   .max, lineBytes)),
              supportsPutPartial = TransferSizes(m.supportsPutPartial.min, min(m.supportsPutPartial.max, lineBytes)),
              supportsGet        = TransferSizes(m.supportsGet       .min, min(m.supportsGet       .max, lineBytes)),
              supportsHint       = TransferSizes(m.supportsHint      .min, min(m.supportsHint      .max, lineBytes)),
              supportsArithmetic = TransferSizes(m.supportsArithmetic.min, min(m.supportsArithmetic.max, lineBytes)),
              supportsLogical    = TransferSizes(m.supportsLogical   .min, min(m.supportsLogical   .max, lineBytes)),
              fifoId             = None // trackers do not respond in FIFO order!
            )
          } else {
            m
          }
        }
      )
    }
  )

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val clients = edgeIn.client.clients
      val managers = edgeOut.manager.managers
      val lineShift = log2Ceil(lineBytes)

      import TLBroadcastConstants._

      require (lineBytes >= edgeOut.manager.beatBytes)
      // For the probe walker, we need to identify all the caches
      val caches = clients.filter(_.supportsProbe).map(_.sourceId)
      val cache_targets = caches.map(c => c.start.U)

      // Create the probe filter
      val filter = Module(filterFactory(ProbeFilterParams(
        mshrs  = numTrackers,
        caches = caches.size,
        blockAddressBits = log2Ceil(edgeIn.manager.maxAddress) - lineShift)))

      // Create the request tracker queues
      val trackers = Seq.tabulate(numTrackers) { id =>
        Module(new TLBroadcastTracker(id, lineBytes, caches.size, bufferless, edgeIn, edgeOut)).io
      }

      // We always accept E
      in.e.ready := true.B
      (trackers zip UIntToOH(in.e.bits.sink).asBools) foreach { case (tracker, select) =>
        tracker.e_last := select && in.e.fire()
      }

      // Depending on the high source bits, we might transform D
      val d_high = log2Ceil(edgeIn.client.endSourceId)
      val d_what = out.d.bits.source(d_high+1, d_high)
      val d_drop = d_what === DROP
      val d_hasData = edgeOut.hasData(out.d.bits)
      val d_normal = Wire(in.d)
      val (d_first, d_last, _) = edgeIn.firstlast(d_normal)
      val d_trackerOH = VecInit(trackers.map { t => t.need_d && t.source === d_normal.bits.source }).asUInt holdUnless d_first

      assert (!out.d.valid || !d_drop || out.d.bits.opcode === TLMessages.AccessAck)

      val d_allow = Wire(Bool())
      out.d.ready := (d_normal.ready && d_allow) || d_drop
      d_normal.valid := out.d.valid && d_allow && !d_drop
      d_normal.bits := out.d.bits // truncates source
      when (d_what(1)) { // TRANSFORM_*
        d_normal.bits.opcode := Mux(d_hasData, TLMessages.GrantData, TLMessages.ReleaseAck)
        d_normal.bits.param  := Mux(d_hasData, Mux(d_what(0), TLPermissions.toT, TLPermissions.toB), 0.U)
      }
      val d_mshr = OHToUInt(d_trackerOH)
      d_normal.bits.sink := d_mshr
      assert (!d_normal.valid || (d_trackerOH.orR() || d_normal.bits.opcode === TLMessages.ReleaseAck))

      // A tracker response is anything neither dropped nor a ReleaseAck
      val d_response = d_hasData || !d_what(1)
      (trackers zip d_trackerOH.asBools) foreach { case (tracker, select) =>
        tracker.d_last := select && d_normal.fire() && d_response && d_last
        tracker.probedack := select && out.d.fire() && d_drop
      }

      d_allow := filter.io.update.ready || !d_response || !d_last
      filter.io.update.valid := out.d.valid && d_normal.ready && !d_drop && d_response && d_last
      filter.io.update.bits.mshr := d_mshr
      filter.io.update.bits.gaveT := d_what === TRANSFORM_T
      filter.io.update.bits.cacheOH := Mux1H(d_trackerOH, trackers.map(_.cacheOH))

      // Incoming C can be:
      // ProbeAck     => decrement tracker, drop 
      // ProbeAckData => decrement tracker, send out A as PutFull(DROP)
      // ReleaseData  =>                    send out A as PutFull(TRANSFORM)
      // Release      => send out D as ReleaseAck

      val c_probeack     = in.c.bits.opcode === TLMessages.ProbeAck
      val c_probeackdata = in.c.bits.opcode === TLMessages.ProbeAckData
      val c_releasedata  = in.c.bits.opcode === TLMessages.ReleaseData
      val c_release      = in.c.bits.opcode === TLMessages.Release
      val c_trackerOH    = trackers.map { t => t.line === (in.c.bits.address >> lineShift) }
      val c_trackerSrc   = Mux1H(c_trackerOH, trackers.map { _.source })

      // Record if this inner cache no longer has the block
      val whoC = Cat(caches.map(_.contains(in.c.bits.source)).reverse)
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

      // The Probe FSM walks all caches and probes them
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
      filter.io.request.bits.address := in.a.bits.address >> lineShift
      filter.io.request.bits.needT   := edgeIn.needT(in.a.bits)
      filter.io.request.bits.allocOH := a_cache // Note: this assumes a cache doing MMIO has allocated

      val leaveB = !filter.io.response.bits.needT && !filter.io.response.bits.gaveT
      val others = filter.io.response.bits.cacheOH & ~filter.io.response.bits.allocOH
      val todo = Mux(leaveB, 0.U, others)
      filter.io.response.ready := !probe_busy
      when (filter.io.response.fire()) {
        probe_todo  := todo
        probe_line  := filter.io.response.bits.address
        probe_perms := Mux(filter.io.response.bits.needT, TLPermissions.toN, TLPermissions.toB)
      }

      // Inform the tracker of the number of probe targets
      val responseCache = filter.io.response.bits.cacheOH | filter.io.response.bits.allocOH
      val responseCount = PopCount(todo)
      val responseMSHR = UIntToOH(filter.io.response.bits.mshr, numTrackers).asBools
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
    }
  }
}

// blockAddressBits is after removing the block offset (typically lowest 6 bits for 64 bytes)
case class ProbeFilterParams(mshrs: Int, caches: Int, blockAddressBits: Int)
{
  require (mshrs >= 0)
  require (caches >= 0)
  require (blockAddressBits > 0)

  val mshrBits = log2Ceil(mshrs)
}

class ProbeFilterRequest(val params: ProbeFilterParams) extends Bundle {
  val mshr    = UInt(params.mshrBits.W)
  val address = UInt(params.blockAddressBits.W)
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
  val keepB   = Bool()
  val cacheOH = UInt(params.caches.W)
}

class ProbeFilterIO(val params: ProbeFilterParams) extends Bundle {
  val request = Flipped(Decoupled(new ProbeFilterRequest(params)))
  val response = Decoupled(new ProbeFilterResponse(params))
  val update  = Flipped(Decoupled(new ProbeFilterUpdate(params)))
  val release = Flipped(Decoupled(new ProbeFilterRelease(params)))
}

abstract class ProbeFilter(val params: ProbeFilterParams) extends Module {
  val io = IO(new ProbeFilterIO(params))
}

class BroadcastFilter(params: ProbeFilterParams) extends ProbeFilter(params) {
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
  {
    val broadcast = LazyModule(new TLBroadcast(lineBytes, numTrackers, bufferless, filterFactory))
    broadcast.node
  }
}

class ProbeTrackInfo(val caches: Int) extends Bundle {
  val count   = UInt(log2Ceil(caches+1).W)
  val cacheOH = UInt(caches.W)
}

class TLBroadcastTracker(id: Int, lineBytes: Int, caches: Int, bufferless: Boolean, edgeIn: TLEdgeIn, edgeOut: TLEdgeOut) extends Module
{
  val io = IO(new Bundle {
    val in_a_first = Input(Bool())
    val in_a  = Flipped(Decoupled(new TLBundleA(edgeIn.bundle)))
    val out_a = Decoupled(new TLBundleA(edgeOut.bundle))
    val probe = Input(Valid(new ProbeTrackInfo(caches)))
    val probenack = Input(Bool())
    val probedack = Input(Bool())
    val probesack = Input(Bool())
    val d_last = Input(Bool())
    val e_last = Input(Bool())
    val source = Output(UInt()) // the source awaiting D response
    val line = Output(UInt())   // the line waiting for probes
    val idle = Output(Bool())
    val need_d = Output(Bool())
    val cacheOH = Output(UInt(caches.W))
    val clearOH = Input(UInt(caches.W))
  })

  val lineShift = log2Ceil(lineBytes)
  import TLBroadcastConstants._

  // Only one operation can be inflight per line, because we need to be sure
  // we send the request after all the probes we sent and before all the next probes
  val got_e   = RegInit(true.B)
  val sent_d  = RegInit(true.B)
  val shared  = Reg(Bool())
  val opcode  = Reg(io.in_a.bits.opcode)
  val param   = Reg(io.in_a.bits.param)
  val size    = Reg(io.in_a.bits.size)
  val source  = Reg(io.in_a.bits.source)
  val user    = Reg(io.in_a.bits.user)
  val echo    = Reg(io.in_a.bits.echo)
  val address = RegInit((id << lineShift).U(io.in_a.bits.address.getWidth.W))
  val count   = Reg(UInt(log2Ceil(caches+1).W))
  val cacheOH = Reg(UInt(caches.W))
  val idle    = got_e && sent_d

  when (io.in_a.fire() && io.in_a_first) {
    assert (idle)
    sent_d  := false.B
    shared  := false.B
    got_e   := io.in_a.bits.opcode =/= TLMessages.AcquireBlock && io.in_a.bits.opcode =/= TLMessages.AcquirePerm
    opcode  := io.in_a.bits.opcode
    param   := io.in_a.bits.param
    size    := io.in_a.bits.size
    source  := io.in_a.bits.source
    user   :<= io.in_a.bits.user
    echo   :<= io.in_a.bits.echo
    address := io.in_a.bits.address
    count   := 1.U
  }

  cacheOH := cacheOH & ~io.clearOH
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

  when (io.probenack || io.probedack) {
    assert (count > 0.U)
    count := count - Mux(io.probenack && io.probedack, 2.U, 1.U)
  }

  when (io.probesack) {
    shared := true.B
  }

  io.idle := idle
  io.need_d := !sent_d
  io.source := source
  io.line := address >> lineShift
  io.cacheOH := cacheOH

  val i_data = Wire(Decoupled(new TLBroadcastData(edgeIn.bundle)))
  val o_data = Queue(i_data, if (bufferless) 1 else (lineBytes / edgeIn.manager.beatBytes), pipe=bufferless)

  io.in_a.ready := (idle || !io.in_a_first) && i_data.ready
  i_data.valid := (idle || !io.in_a_first) && io.in_a.valid
  i_data.bits.mask := io.in_a.bits.mask
  i_data.bits.data := io.in_a.bits.data

  val probe_done = count === 0.U
  val acquire = opcode === TLMessages.AcquireBlock || opcode === TLMessages.AcquirePerm

  val transform = Mux(shared, TRANSFORM_B, TRANSFORM_T)

  o_data.ready := io.out_a.ready && probe_done
  io.out_a.valid := o_data.valid && probe_done
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
