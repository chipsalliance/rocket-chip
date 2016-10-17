// See LICENSE for license details.

package uncore.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._
import uncore.tilelink2._

case class AXI4ToTLNode() extends MixedNode(AXI4Imp, TLImp)(
  dFn = { case (1, Seq(AXI4MasterPortParameters(masters))) =>
    Seq(TLClientPortParameters(clients = masters.map { m =>
      TLClientParameters(
        sourceId = IdRange(m.id.start << 1, m.id.end << 1), // R+W ids are distinct
        nodePath = m.nodePath)
    }))
  },
  uFn = { case (1, Seq(TLManagerPortParameters(managers, beatBytes, _))) =>
    Seq(AXI4SlavePortParameters(beatBytes = beatBytes, slaves = managers.map { m =>
      AXI4SlaveParameters(
        address       = m.address,
        regionType    = m.regionType,
        executable    = m.executable,
        nodePath      = m.nodePath,
        supportsWrite = m.supportsPutPartial,
        supportsRead  = m.supportsGet,
        interleavedId = Some(0)) // TL2 never interleaves D beats
    }))
  },
  numPO = 1 to 1,
  numPI = 1 to 1)

class AXI4ToTL extends LazyModule
{
  val node = AXI4ToTLNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
      val out = node.bundleOut
    }

    val in = io.in(0)
    val out = io.out(0)
    val edgeIn = node.edgesIn(0)
    val edgeOut = node.edgesOut(0)
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
    val r_ok = edgeOut.manager.supportsGetSafe(r_addr, r_size) // !!! decode error

    in.ar.ready := r_out.ready && !r_block
    r_out.valid := in.ar.valid && !r_block
    r_out.bits := edgeOut.Get(in.ar.bits.id << 1 | UInt(1), r_addr, r_size)._2
    assert (!in.ar.valid || r_size1 === UIntToOH1(r_size, countBits)) // because aligned

    val w_out = Wire(out.a)
    val w_inflight = RegInit(UInt(0, width = numIds))
    val w_block = w_inflight(in.aw.bits.id)
    val w_size1 = in.aw.bits.bytes1()
    val w_size = OH1ToUInt(w_size1)
    val w_addr = in.aw.bits.addr
    val w_ok = edgeOut.manager.supportsPutPartialSafe(w_addr, w_size)

    in.aw.ready := w_out.ready && in.w.valid && in.w.bits.last && !w_block
    in.w.ready := w_out.ready && in.aw.valid && !w_block
    w_out.valid := in.aw.valid && in.w.valid && !w_block
    w_out.bits := edgeOut.Put(in.aw.bits.id << 1, w_addr, w_size, in.w.bits.data, in.w.bits.strb)._2
    assert (!in.aw.valid || w_size1 === UIntToOH1(w_size, countBits)) // because aligned

    TLArbiter(TLArbiter.lowestIndexFirst)(out.a, (UInt(0), r_out), (in.aw.bits.len, w_out))
    assert (!in.aw.valid || in.aw.bits.len === UInt(0) || in.aw.bits.size === UInt(log2Ceil(beatBytes))) // because aligned

    val out_d = Queue.irrevocable(out.d, 1, flow=true) // AXI4 requires irrevocable
    val d_resp = Mux(out_d.bits.error, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY)
    val d_hasData = edgeOut.hasData(out_d.bits)
    val (_, d_last, _) = edgeOut.firstlast(out_d.bits, out_d.fire())
    out_d.ready := Mux(d_hasData, in.r.ready, in.b.ready)

    in.r.valid := out_d.valid && d_hasData
    in.r.bits.id   := out_d.bits.source >> 1
    in.r.bits.data := out_d.bits.data
    in.r.bits.resp := d_resp
    in.r.bits.last := d_last

    in.b.valid := out_d.valid && !d_hasData
    in.b.bits.id   := out_d.bits.source >> 1
    in.b.bits.resp := d_resp

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

object AXI4ToTL
{
  def apply()(x: AXI4OutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = {
    val tl = LazyModule(new AXI4ToTL)
    tl.node := x
    tl.node
  }
}
