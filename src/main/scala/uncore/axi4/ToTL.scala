// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import uncore.tilelink2._

case class AXI4ToTLNode() extends MixedAdapterNode(AXI4Imp, TLImp)(
  dFn = { case AXI4MasterPortParameters(masters) =>
    TLClientPortParameters(clients = masters.map { m =>
      TLClientParameters(
        sourceId = IdRange(m.id.start << 1, m.id.end << 1), // R+W ids are distinct
        nodePath = m.nodePath)
    })
  },
  uFn = { mp => AXI4SlavePortParameters(
    slaves = mp.managers.map { m =>
      AXI4SlaveParameters(
        address       = m.address,
        resources     = m.resources,
        regionType    = m.regionType,
        executable    = m.executable,
        nodePath      = m.nodePath,
        supportsWrite = m.supportsPutPartial,
        supportsRead  = m.supportsGet,
        interleavedId = Some(0))}, // TL2 never interleaves D beats
    beatBytes = mp.beatBytes,
    minLatency = mp.minLatency)
  })

class AXI4ToTL()(implicit p: Parameters) extends LazyModule
{
  val node = AXI4ToTLNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val numIds = edgeIn.master.endId
      val beatBytes = edgeOut.manager.beatBytes
      val countBits = AXI4Parameters.lenBits + (1 << AXI4Parameters.sizeBits) - 1

      require (edgeIn.master.masters(0).aligned)

      val r_out = Wire(out.a)
      val r_inflight = RegInit(UInt(0, width = numIds))
      val r_block = r_inflight(in.ar.bits.id)
      val r_size1 = in.ar.bits.bytes1()
      val r_size = OH1ToUInt(r_size1)
      val r_addr = in.ar.bits.addr
      val r_ok = edgeOut.manager.supportsGetSafe(r_addr, r_size)
      val r_err_in = Wire(Decoupled(new AXI4BundleRError(in.ar.bits.params)))
      val r_err_out = Queue(r_err_in, 2)
      val r_count = RegInit(UInt(0, width = in.ar.bits.params.lenBits))
      val r_last = r_count === in.ar.bits.len

      assert (!in.ar.valid || r_size1 === UIntToOH1(r_size, countBits)) // because aligned
      in.ar.ready := Mux(r_ok, r_out.ready, r_err_in.ready && r_last) && !r_block
      r_out.valid := in.ar.valid && !r_block && r_ok
      r_out.bits := edgeOut.Get(in.ar.bits.id << 1 | UInt(1), r_addr, r_size)._2
      r_err_in.valid := in.ar.valid && !r_block && !r_ok
      r_err_in.bits.last := r_last
      r_err_in.bits.id := in.ar.bits.id

      when (r_err_in.fire()) { r_count := Mux(r_last, UInt(0), r_count + UInt(1)) }

      val w_out = Wire(out.a)
      val w_inflight = RegInit(UInt(0, width = numIds))
      val w_block = w_inflight(in.aw.bits.id)
      val w_size1 = in.aw.bits.bytes1()
      val w_size = OH1ToUInt(w_size1)
      val w_addr = in.aw.bits.addr
      val w_ok = edgeOut.manager.supportsPutPartialSafe(w_addr, w_size)
      val w_err_in = Wire(Decoupled(in.aw.bits.id))
      val w_err_out = Queue(w_err_in, 2)

      assert (!in.aw.valid || w_size1 === UIntToOH1(w_size, countBits)) // because aligned
      assert (!in.aw.valid || in.aw.bits.len === UInt(0) || in.aw.bits.size === UInt(log2Ceil(beatBytes))) // because aligned
      in.aw.ready := Mux(w_ok, w_out.ready, w_err_in.ready) && in.w.valid && in.w.bits.last && !w_block
      in.w.ready  := Mux(w_ok, w_out.ready, w_err_in.ready || !in.w.bits.last) && in.aw.valid && !w_block
      w_out.valid := in.aw.valid && in.w.valid && !w_block && w_ok
      w_out.bits := edgeOut.Put(in.aw.bits.id << 1, w_addr, w_size, in.w.bits.data, in.w.bits.strb)._2
      w_err_in.valid := in.aw.valid && in.w.valid && !w_block && !w_ok && in.w.bits.last
      w_err_in.bits := in.aw.bits.id

      TLArbiter(TLArbiter.lowestIndexFirst)(out.a, (UInt(0), r_out), (in.aw.bits.len, w_out))

      val ok_b  = Wire(in.b)
      val err_b = Wire(in.b)
      val mux_b = Wire(in.b)
      val ok_r  = Wire(in.r)
      val err_r = Wire(in.r)
      val mux_r = Wire(in.r)

      val d_resp = Mux(out.d.bits.error, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY)
      val d_hasData = edgeOut.hasData(out.d.bits)
      val d_last = edgeOut.last(out.d)

      out.d.ready := Mux(d_hasData, ok_r.ready, ok_b.ready)
      ok_r.valid := out.d.valid && d_hasData
      ok_b.valid := out.d.valid && !d_hasData

      ok_r.bits.id   := out.d.bits.source >> 1
      ok_r.bits.data := out.d.bits.data
      ok_r.bits.resp := d_resp
      ok_r.bits.last := d_last

      r_err_out.ready := err_r.ready
      err_r.valid := r_err_out.valid
      err_r.bits.id   := r_err_out.bits.id
      err_r.bits.data := out.d.bits.data // don't care
      err_r.bits.resp := AXI4Parameters.RESP_DECERR
      err_r.bits.last := r_err_out.bits.last

      // AXI4 must hold R to one source until last
      val mux_lock_ok  = RegInit(Bool(false))
      val mux_lock_err = RegInit(Bool(false))
      when (ok_r .fire()) { mux_lock_ok  := !ok_r .bits.last }
      when (err_r.fire()) { mux_lock_err := !err_r.bits.last }
      assert (!mux_lock_ok || !mux_lock_err)

      // Prioritize err over ok (b/c err_r.valid comes from a register)
      mux_r.valid := (!mux_lock_err && ok_r.valid) || (!mux_lock_ok && err_r.valid)
      mux_r.bits  := Mux(!mux_lock_ok && err_r.valid, err_r.bits, ok_r.bits)
      ok_r.ready  := mux_r.ready && (mux_lock_ok || !err_r.valid)
      err_r.ready := mux_r.ready && !mux_lock_ok

      // AXI4 needs irrevocable behaviour
      in.r <> Queue.irrevocable(mux_r, 1, flow=true)

      ok_b.bits.id   := out.d.bits.source >> 1
      ok_b.bits.resp := d_resp

      w_err_out.ready := err_b.ready
      err_b.valid := w_err_out.valid
      err_b.bits.id   := w_err_out.bits
      err_b.bits.resp := AXI4Parameters.RESP_DECERR

      // Prioritize err over ok (b/c err_b.valid comes from a register)
      mux_b.valid := ok_b.valid || err_b.valid
      mux_b.bits  := Mux(err_b.valid, err_b.bits, ok_b.bits)
      ok_b.ready  := mux_b.ready && !err_b.valid
      err_b.ready := mux_b.ready

      // AXI4 needs irrevocable behaviour
      in.b <> Queue.irrevocable(mux_b, 1, flow=true)

      // Update flight trackers
      val r_set = in.ar.fire().asUInt << in.ar.bits.id
      val r_clr = (in.r.fire() && in.r.bits.last).asUInt << in.r.bits.id
      r_inflight := (r_inflight | r_set) & ~r_clr
      val w_set = in.aw.fire().asUInt << in.aw.bits.id
      val w_clr = in.b.fire().asUInt << in.b.bits.id
      w_inflight := (w_inflight | w_set) & ~w_clr

      // Unused channels
      out.b.ready := Bool(true)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
    }
  }
}

class AXI4BundleRError(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val id   = UInt(width = params.idBits)
  val last = Bool()
}

object AXI4ToTL
{
  def apply()(x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val tl = LazyModule(new AXI4ToTL)
    tl.node := x
    tl.node
  }
}
