// See LICENSE for license details.

package uncore.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util.IrrevocableIO
import diplomacy._
import scala.math.{min,max}
import uncore.tilelink2.{leftOR, rightOR, UIntToOH1}

// lite: masters all use only one ID => reads will not be interleaved
class AXI4Fragmenter(lite: Boolean = false, maxInFlight: Int = 32, combinational: Boolean = true) extends LazyModule
{
  val maxBeats = 1 << AXI4Parameters.lenBits
  def expandTransfer(x: TransferSizes, beatBytes: Int, alignment: BigInt) =
    if (!x) x else TransferSizes(x.min, alignment.min(maxBeats*beatBytes).intValue)
  def mapSlave(s: AXI4SlaveParameters, beatBytes: Int) = s.copy(
    supportsWrite = expandTransfer(s.supportsWrite, beatBytes, s.minAlignment),
    supportsRead  = expandTransfer(s.supportsRead,  beatBytes, s.minAlignment),
    interleavedId = if (lite) Some(0) else s.interleavedId) // see AXI4FragmenterSideband for !lite case
  def mapMaster(m: AXI4MasterParameters) = m.copy(aligned = true)

  val node = AXI4AdapterNode(
    masterFn = { case Seq(mp) => mp.copy(masters = mp.masters.map(m => mapMaster(m))) },
    slaveFn  = { case Seq(sp) => sp.copy(slaves  = sp.slaves .map(s => mapSlave(s, sp.beatBytes))) })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    val edgeOut   = node.edgesOut(0)
    val edgeIn    = node.edgesIn(0)
    val slave     = edgeOut.slave
    val slaves    = slave.slaves
    val beatBytes = slave.beatBytes
    val lgBytes   = log2Ceil(beatBytes)
    val master    = edgeIn.master
    val masters   = master.masters

    // If the user claimed this was a lite interface, then there must be only one Id
    require (!lite || master.endId == 1)

    // We don't support fragmenting to sub-beat accesses
    slaves.foreach { s =>
      require (!s.supportsRead  || s.supportsRead.contains(beatBytes))
      require (!s.supportsWrite || s.supportsWrite.contains(beatBytes))
    }

    /* We need to decompose a request into 
     *   FIXED => each beat is a new request
     *   WRAP/INCR => take xfr up to next power of two, capped by max size of target
     *
     * On AR and AW, we fragment one request into many
     * On W we set 'last' on beats which are fragment boundaries
     * On R we clear 'last' on the fragments being reassembled
     * On B we clear 'valid' on the responses for the injected fragments
     *
     * AR=>R and AW+W=>B are completely independent state machines.
     */

    /* Returns the number of beats to execute and the new address */
    def fragment(a: IrrevocableIO[AXI4BundleA], supportedSizes1: Seq[Int]): (IrrevocableIO[AXI4BundleA], Bool, UInt) = {
      val out = Wire(a)

      val busy   = RegInit(Bool(false))
      val r_addr = Reg(UInt(width = a.bits.params.addrBits))
      val r_len  = Reg(UInt(width = AXI4Parameters.lenBits))

      val len  = Mux(busy, r_len,  a.bits.len)
      val addr = Mux(busy, r_addr, a.bits.addr)

      val lo = if (lgBytes == 0) UInt(0) else addr(lgBytes-1, 0)
      val hi = addr >> lgBytes
      val alignment = hi(AXI4Parameters.lenBits-1,0)

      val allSame = supportedSizes1.filter(_ >= 0).distinct.size <= 1
      val dynamic1 = Mux1H(slave.findFast(addr), supportedSizes1.map(s => UInt(max(0, s))))
      val fixed1 = UInt(supportedSizes1.filter(_ >= 0).headOption.getOrElse(0))

      /* We need to compute the largest transfer allowed by the AXI len.
       * len+1 is the number of beats to execute.
       * We want the MSB(len+1)-1; one less than the largest power of two we could execute.
       * There are two cases; either len is 2^n-1 in which case we leave it unchanged, ELSE
       *   fill the bits from highest to lowest, and shift right by one bit.
       */
      val fillLow  = rightOR(len) >> 1   // set   all bits in positions <  a set     bit
      val wipeHigh = ~leftOR(~len)       // clear all bits in position  >= a cleared bit
      val remain1  = fillLow | wipeHigh  // MSB(a.len+1)-1
      val align1   = ~leftOR(alignment)  // transfer size limited by address alignment
      val support1 = if (allSame) fixed1 else dynamic1 // maximum supported size-1 based on target address
      val maxSupported1 = remain1 & align1 & support1 // Take the minimum of all the limits

      // Things that cause us to degenerate to a single beat
      val fixed = a.bits.burst === AXI4Parameters.BURST_FIXED
      val narrow = a.bits.size =/= UInt(lgBytes)
      val bad = fixed || narrow

      // The number of beats-1 to execute
      val beats1 = Mux(bad, UInt(0), maxSupported1)
      val beats = ~(~(beats1 << 1 | UInt(1)) | beats1) // beats1 + 1

      val inc_addr = addr + (beats << a.bits.size) // address after adding transfer
      val wrapMask = a.bits.bytes1() // only these bits may change, if wrapping
      val mux_addr = Wire(init = inc_addr)
      when (a.bits.burst === AXI4Parameters.BURST_WRAP) {
        mux_addr := (inc_addr & wrapMask) | ~(~a.bits.addr | wrapMask)
      }
      when (a.bits.burst === AXI4Parameters.BURST_FIXED) {
        mux_addr := a.bits.addr
      }

      val last = beats1 === len
      a.ready := out.ready && last
      out.valid := a.valid

      out.bits := a.bits
      out.bits.len := beats1

      // We forcibly align every access. If the first beat was misaligned, the strb bits
      // for the lower addresses must not have been set. Therefore, rounding the address
      // down is harmless. We can do this after the address update algorithm, because the
      // incremented values will be rounded down the same way. Furthermore, a subword
      // offset cannot cause a premature wrap-around.
      out.bits.addr := ~(~addr | UIntToOH1(a.bits.size, lgBytes))

      when (out.fire()) {
        busy := !last
        r_addr := mux_addr
        r_len  := len - beats
      }

      (out, last, beats)
    }

    val in = io.in(0)
    val out = io.out(0)

    // The size to which we will fragment the access
    val readSizes1  = slaves.map(s => s.supportsRead .max/beatBytes-1)
    val writeSizes1 = slaves.map(s => s.supportsWrite.max/beatBytes-1)

    // Indirection variables for inputs and outputs; makes transformation application easier
    val (in_ar, ar_last, _)       = fragment(in.ar, readSizes1)
    val (in_aw, aw_last, w_beats) = fragment(in.aw, writeSizes1)
    val in_w = in.w
    val in_r = in.r
    val in_b = in.b
    val out_ar = Wire(out.ar)
    val out_aw = out.aw
    val out_w = out.w
    val out_r = Wire(out.r)
    val out_b = Wire(out.b)

    val depth = if (combinational) 1 else 2
    // In case a slave ties arready := rready, we need a queue to break the combinational loop
    // between the two branches (in_ar => {out_ar => out_r, sideband} => in_r).
    if (in.ar.bits.getWidth < in.r.bits.getWidth) {
      out.ar <> Queue(out_ar, depth, flow=combinational)
      out_r <> out.r
    } else {
      out.ar <> out_ar
      out_r <> Queue(out.r, depth, flow=combinational)
    }
    // In case a slave ties awready := bready or wready := bready, we need this queue
    out_b <> Queue(out.b, depth, flow=combinational)

    // Sideband to track which transfers were the last fragment
    def sideband() = if (lite) {
      Module(new Queue(Bool(), maxInFlight, flow=combinational)).io
    } else {
      Module(new AXI4FragmenterSideband(maxInFlight, flow=combinational)).io
    }
    val sideband_ar_r = sideband()
    val sideband_aw_b = sideband()

    // AR flow control
    out_ar.valid := in_ar.valid && sideband_ar_r.enq.ready
    in_ar.ready := sideband_ar_r.enq.ready && out_ar.ready
    sideband_ar_r.enq.valid := in_ar.valid && out_ar.ready
    out_ar.bits := in_ar.bits
    sideband_ar_r.enq.bits := ar_last

    // When does W channel start counting a new transfer
    val wbeats_latched = RegInit(Bool(false))
    val wbeats_ready = Wire(Bool())
    val wbeats_valid = Wire(Bool())
    when (wbeats_valid && wbeats_ready) { wbeats_latched := Bool(true) }
    when (out_aw.fire()) { wbeats_latched := Bool(false) }

    // AW flow control
    out_aw.valid := in_aw.valid && sideband_aw_b.enq.ready && (wbeats_ready || wbeats_latched)
    in_aw.ready := sideband_aw_b.enq.ready && out_aw.ready && (wbeats_ready || wbeats_latched)
    sideband_aw_b.enq.valid := in_aw.valid && out_aw.ready && (wbeats_ready || wbeats_latched)
    wbeats_valid := in_aw.valid && !wbeats_latched
    out_aw.bits := in_aw.bits
    sideband_aw_b.enq.bits := aw_last

    // We need to inject 'last' into the W channel fragments, count!
    val w_counter = RegInit(UInt(0, width = AXI4Parameters.lenBits+1))
    val w_idle = w_counter === UInt(0)
    val w_todo = Mux(w_idle, Mux(wbeats_valid, w_beats, UInt(0)), w_counter)
    val w_last = w_todo === UInt(1)
    w_counter := w_todo - out_w.fire()
    assert (!out_w.fire() || w_todo =/= UInt(0)) // underflow impossible

    // W flow control
    wbeats_ready := w_idle
    out_w.valid := in_w.valid && (!wbeats_ready || wbeats_valid)
    in_w.ready := out_w.ready && (!wbeats_ready || wbeats_valid)
    out_w.bits := in_w.bits
    out_w.bits.last := w_last
    // We should also recreate the last last
    assert (!out_w.valid || !in_w.bits.last || w_last)

    // R flow control
    val r_last = out_r.bits.last
    in_r.valid := out_r.valid && (!r_last || sideband_ar_r.deq.valid)
    out_r.ready := in_r.ready && (!r_last || sideband_ar_r.deq.valid)
    sideband_ar_r.deq.ready := r_last && out_r.valid && in_r.ready
    in_r.bits := out_r.bits
    in_r.bits.last := r_last && sideband_ar_r.deq.bits

    // B flow control
    val b_last = sideband_aw_b.deq.bits
    in_b.valid := out_b.valid && sideband_aw_b.deq.valid && b_last
    out_b.ready := sideband_aw_b.deq.valid && (!b_last || in_b.ready)
    sideband_aw_b.deq.ready := out_b.valid && (!b_last || in_b.ready)
    in_b.bits := out_b.bits

    // Merge errors from dropped B responses
    val r_resp = RegInit(UInt(0, width = AXI4Parameters.respBits))
    val resp = out_b.bits.resp | r_resp
    when (out_b.fire()) { r_resp := Mux(b_last, UInt(0), resp) }
    in_b.bits.resp := resp
  }
}

/* We want to put barriers between the fragments of a fragmented transfer and all other transfers.
 * This lets us use very little state to reassemble the fragments (else we need one FIFO per ID).
 * Furthermore, because all the fragments share the same AXI ID, they come back contiguously.
 * This guarantees that no other R responses might get mixed between fragments, ensuring that the
 * interleavedId for the slaves remains unaffected by the fragmentation transformation.
 * Of course, if you need to fragment, this means there is a potentially hefty serialization cost.
 * However, this design allows full concurrency in the common no-fragmentation-needed scenario.
 */
class AXI4FragmenterSideband(maxInFlight: Int, flow: Boolean = false) extends Module
{
  val io = new QueueIO(Bool(), maxInFlight)
  io.count := UInt(0)

  val PASS = UInt(2, width = 2) // allow 'last=1' bits to enque, on 'last=0' if count>0 block else accept+FIND
  val FIND = UInt(0, width = 2) // allow 'last=0' bits to enque, accept 'last=1' and switch to WAIT
  val WAIT = UInt(1, width = 2) // block all access till count=0

  val state = RegInit(PASS)
  val count = RegInit(UInt(0, width = log2Up(maxInFlight)))
  val full  = count === UInt(maxInFlight-1)
  val empty = count === UInt(0)
  val last  = count === UInt(1)

  io.deq.bits := state(1) || (last && state(0)) // PASS || (last && WAIT)
  io.deq.valid := !empty

  io.enq.ready := !full && (empty || (state === FIND) || (state === PASS && io.enq.bits))

  // WAIT => count > 0
  assert (state =/= WAIT || count =/= UInt(0))

  if (flow) {
    when (io.enq.valid) {
      io.deq.valid := Bool(true)
      when (empty) { io.deq.bits := io.enq.bits }
    }
  }

  count := count + io.enq.fire() - io.deq.fire()
  switch (state) {
    is(PASS) { when (io.enq.valid && !io.enq.bits && empty) { state := FIND } }
    is(FIND) { when (io.enq.valid &&  io.enq.bits && !full) { state := Mux(empty, PASS, WAIT) } }
    is(WAIT) { when (last && io.deq.ready)                  { state := PASS } }
  }
}

object AXI4Fragmenter
{
  // applied to the AXI4 source node; y.node := AXI4Fragmenter()(x.node)
  def apply(lite: Boolean = false, maxInFlight: Int = 32, combinational: Boolean = true)(x: AXI4OutwardNode)(implicit sourceInfo: SourceInfo): AXI4OutwardNode = {
    val fragmenter = LazyModule(new AXI4Fragmenter(lite, maxInFlight, combinational))
    fragmenter.node := x
    fragmenter.node
  }
}
