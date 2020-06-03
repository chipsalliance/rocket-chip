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

class TLBroadcast(lineBytes: Int, numTrackers: Int = 4, bufferless: Boolean = false)(implicit p: Parameters) extends LazyModule
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

      // Create the request tracker queues
      val trackers = Seq.tabulate(numTrackers) { id =>
        Module(new TLBroadcastTracker(id, lineBytes, log2Up(caches.size+1), bufferless, edgeIn, edgeOut)).io
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
      val d_trackerOH = VecInit(trackers.map { t => t.need_d && t.source === d_normal.bits.source }).asUInt

      assert (!out.d.valid || !d_drop || out.d.bits.opcode === TLMessages.AccessAck)

      out.d.ready := d_normal.ready || d_drop
      d_normal.valid := out.d.valid && !d_drop
      d_normal.bits := out.d.bits // truncates source
      when (d_what(1)) { // TRANSFORM_*
        d_normal.bits.opcode := Mux(d_hasData, TLMessages.GrantData, TLMessages.ReleaseAck)
        d_normal.bits.param  := Mux(d_hasData, Mux(d_what(0), TLPermissions.toT, TLPermissions.toB), 0.U)
      }
      d_normal.bits.sink := OHToUInt(d_trackerOH)
      assert (!d_normal.valid || (d_trackerOH.orR() || d_normal.bits.opcode === TLMessages.ReleaseAck))

      // A tracker response is anything neither dropped nor a ReleaseAck
      val d_response = d_hasData || !d_what(1)
      val d_last = edgeIn.last(d_normal)
      (trackers zip d_trackerOH.asBools) foreach { case (tracker, select) =>
        tracker.d_last := select && d_normal.fire() && d_response && d_last
        tracker.probedack := select && out.d.fire() && d_drop
      }

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

      // Decrement the tracker's outstanding probe counter
      (trackers zip c_trackerOH) foreach { case (tracker, select) =>
        tracker.probenack := in.c.fire() && c_probeack && select
      }

      (trackers zip c_trackerOH) foreach { case (tracker, select) =>
        tracker.probesack := in.c.fire() && select && (c_probeack || c_probeackdata) && (
          in.c.bits.param === TLPermissions.TtoB ||
          in.c.bits.param === TLPermissions.BtoB)
      }

      val releaseack = Wire(in.d)
      val putfull = Wire(out.a)

      in.c.ready := c_probeack || Mux(c_release, releaseack.ready, putfull.ready)

      releaseack.valid := in.c.valid && c_release
      releaseack.bits  := edgeIn.ReleaseAck(in.c.bits)

      val put_what = Mux(c_releasedata, TRANSFORM_B, DROP)
      val put_who  = Mux(c_releasedata, in.c.bits.source, c_trackerSrc)
      putfull.valid := in.c.valid && (c_probeackdata || c_releasedata)
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
      val a_cache = if (caches.size == 0) 1.U else VecInit(caches.map(_.contains(in.a.bits.source))).asUInt
      val a_first = edgeIn.first(in.a)

      // To accept a request from A, the probe FSM must be idle and there must be a matching tracker
      val freeTrackers = VecInit(trackers.map { t => t.idle }).asUInt
      val freeTracker = freeTrackers.orR()
      val matchTrackers = VecInit(trackers.map { t => t.line === in.a.bits.address >> lineShift }).asUInt
      val matchTracker = matchTrackers.orR()
      val allocTracker = freeTrackers & ~(leftOR(freeTrackers) << 1)
      val selectTracker = Mux(matchTracker, matchTrackers, allocTracker)

      val trackerReady = VecInit(trackers.map(_.in_a.ready)).asUInt
      in.a.ready := (!a_first || !probe_busy) && (selectTracker & trackerReady).orR()
      (trackers zip selectTracker.asBools) foreach { case (t, select) =>
        t.in_a.valid := in.a.valid && select && (!a_first || !probe_busy)
        t.in_a.bits := in.a.bits
        t.in_a_first := a_first
        t.probe := (if (caches.size == 0) 0.U else Mux(a_cache.orR(), (caches.size-1).U, caches.size.U))
      }

      val acq_perms = MuxLookup(in.a.bits.param, Wire(UInt(2.W)), Array(
        TLPermissions.NtoB -> TLPermissions.toB,
        TLPermissions.NtoT -> TLPermissions.toN,
        TLPermissions.BtoT -> TLPermissions.toN))

      when (in.a.fire() && a_first) {
        probe_todo  := ~a_cache // probe all but the cache who poked us
        probe_line  := in.a.bits.address >> lineShift
        probe_perms := MuxLookup(in.a.bits.opcode, Wire(UInt(2.W)), Array(
          TLMessages.PutFullData    -> TLPermissions.toN,
          TLMessages.PutPartialData -> TLPermissions.toN,
          TLMessages.ArithmeticData -> TLPermissions.toN,
          TLMessages.LogicalData    -> TLPermissions.toN,
          TLMessages.Get            -> TLPermissions.toB,
          TLMessages.Hint           -> MuxLookup(in.a.bits.param, Wire(UInt(2.W)), Array(
            TLHints.PREFETCH_READ   -> TLPermissions.toB,
            TLHints.PREFETCH_WRITE  -> TLPermissions.toN)),
          TLMessages.AcquireBlock   -> acq_perms,
          TLMessages.AcquirePerm    -> acq_perms))
      }

      // The outer TL connections may not be cached
      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B
    }
  }
}

object TLBroadcast
{
  def apply(lineBytes: Int, numTrackers: Int = 4, bufferless: Boolean = false)(implicit p: Parameters): TLNode =
  {
    val broadcast = LazyModule(new TLBroadcast(lineBytes, numTrackers, bufferless))
    broadcast.node
  }
}

class TLBroadcastTracker(id: Int, lineBytes: Int, probeCountBits: Int, bufferless: Boolean, edgeIn: TLEdgeIn, edgeOut: TLEdgeOut) extends Module
{
  val io = IO(new Bundle {
    val in_a_first = Input(Bool())
    val in_a  = Flipped(Decoupled(new TLBundleA(edgeIn.bundle)))
    val out_a = Decoupled(new TLBundleA(edgeOut.bundle))
    val probe = Input(UInt(probeCountBits.W))
    val probenack = Input(Bool())
    val probedack = Input(Bool())
    val probesack = Input(Bool())
    val d_last = Input(Bool())
    val e_last = Input(Bool())
    val source = Output(UInt()) // the source awaiting D response
    val line = Output(UInt())   // the line waiting for probes
    val idle = Output(Bool())
    val need_d = Output(Bool())
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
  val count   = Reg(UInt(probeCountBits.W))
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
    count   := io.probe
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
