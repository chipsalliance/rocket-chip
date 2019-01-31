// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class AXI4ToTLNode(wcorrupt: Boolean = false)(implicit valName: ValName) extends MixedAdapterNode(AXI4Imp, TLImp)(
  dFn = { case AXI4MasterPortParameters(masters, userBits) =>
    masters.foreach { m => require (m.maxFlight.isDefined, "AXI4 must include a transaction maximum per ID to convert to TL") }
    val maxFlight = masters.map(_.maxFlight.get).max
    TLClientPortParameters(
      clients = masters.filter(_.maxFlight != Some(0)).flatMap { m =>
        for (id <- m.id.start until m.id.end)
          yield TLClientParameters(
            name        = s"${m.name} ID#${id}",
            sourceId    = IdRange(id * maxFlight*2, (id+1) * maxFlight*2), // R+W ids are distinct
            nodePath    = m.nodePath,
            requestFifo = true)
      })
  },
  uFn = { mp => AXI4SlavePortParameters(
    slaves = mp.managers.map { m =>
      val maxXfer = TransferSizes(1, mp.beatBytes * (1 << AXI4Parameters.lenBits))
      AXI4SlaveParameters(
        address       = m.address,
        resources     = m.resources,
        regionType    = m.regionType,
        executable    = m.executable,
        nodePath      = m.nodePath,
        supportsWrite = m.supportsPutPartial.intersect(maxXfer),
        supportsRead  = m.supportsGet.intersect(maxXfer),
        interleavedId = Some(0))}, // TL2 never interleaves D beats
    beatBytes = mp.beatBytes,
    wcorrupt = wcorrupt,
    minLatency = mp.minLatency)
  })

class AXI4ToTL(wcorrupt: Boolean = false)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4ToTLNode(wcorrupt)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val numIds = edgeIn.master.endId
      val beatBytes = edgeOut.manager.beatBytes
      val beatCountBits = AXI4Parameters.lenBits + (1 << AXI4Parameters.sizeBits) - 1
      val maxFlight = edgeIn.master.masters.map(_.maxFlight.get).max
      val logFlight = log2Ceil(maxFlight)
      val txnCountBits = log2Ceil(maxFlight+1) // wrap-around must not block b_allow
      val addedBits = logFlight + 1 // +1 for read vs. write source ID

      require (edgeIn.master.userBits == 0, "AXI4 user bits cannot be transported by TL")
      require (edgeIn.master.masters(0).aligned)
      edgeOut.manager.requireFifo()

      // Look for an Error device to redirect bad requests
      val errorDevs = edgeOut.manager.managers.filter(_.nodePath.last.lazyModule.className == "TLError")
      require (!errorDevs.isEmpty, "There is no TLError reachable from AXI4ToTL. One must be instantiated.")
      val errorDev = errorDevs.maxBy(_.maxTransfer)
      val error = errorDev.address.head.base
      require (errorDev.supportsPutPartial.contains(edgeOut.manager.maxTransfer),
        s"Error device supports ${errorDev.supportsPutPartial} PutPartial but must support ${edgeOut.manager.maxTransfer}")
      require (errorDev.supportsGet.contains(edgeOut.manager.maxTransfer),
        s"Error device supports ${errorDev.supportsGet} Get but must support ${edgeOut.manager.maxTransfer}")

      val r_out = Wire(out.a)
      val r_size1 = in.ar.bits.bytes1()
      val r_size = OH1ToUInt(r_size1)
      val r_ok = edgeOut.manager.supportsGetSafe(in.ar.bits.addr, r_size)
      val r_addr = Mux(r_ok, in.ar.bits.addr, UInt(error) | in.ar.bits.addr(log2Up(beatBytes)-1, 0))
      val r_count = RegInit(Vec.fill(numIds) { UInt(0, width = txnCountBits) })
      val r_id = Cat(in.ar.bits.id, r_count(in.ar.bits.id)(logFlight-1,0), UInt(0, width=1))

      assert (!in.ar.valid || r_size1 === UIntToOH1(r_size, beatCountBits)) // because aligned
      in.ar.ready := r_out.ready
      r_out.valid := in.ar.valid
      r_out.bits := edgeOut.Get(r_id, r_addr, r_size)._2

      val r_sel = UIntToOH(in.ar.bits.id, numIds)
      (r_sel.asBools zip r_count) foreach { case (s, r) =>
        when (in.ar.fire() && s) { r := r + UInt(1) }
      }

      val w_out = Wire(out.a)
      val w_size1 = in.aw.bits.bytes1()
      val w_size = OH1ToUInt(w_size1)
      val w_ok = edgeOut.manager.supportsPutPartialSafe(in.aw.bits.addr, w_size)
      val w_addr = Mux(w_ok, in.aw.bits.addr, UInt(error) | in.aw.bits.addr(log2Up(beatBytes)-1, 0))
      val w_count = RegInit(Vec.fill(numIds) { UInt(0, width = txnCountBits) })
      val w_id = Cat(in.aw.bits.id, w_count(in.aw.bits.id)(logFlight-1,0), UInt(1, width=1))

      assert (!in.aw.valid || w_size1 === UIntToOH1(w_size, beatCountBits)) // because aligned
      assert (!in.aw.valid || in.aw.bits.len === UInt(0) || in.aw.bits.size === UInt(log2Ceil(beatBytes))) // because aligned
      in.aw.ready := w_out.ready && in.w.valid && in.w.bits.last
      in.w.ready  := w_out.ready && in.aw.valid
      w_out.valid := in.aw.valid && in.w.valid
      w_out.bits := edgeOut.Put(w_id, w_addr, w_size, in.w.bits.data, in.w.bits.strb)._2
      in.w.bits.corrupt.foreach { w_out.bits.corrupt := _ }

      val w_sel = UIntToOH(in.aw.bits.id, numIds)
      (w_sel.asBools zip w_count) foreach { case (s, r) =>
        when (in.aw.fire() && s) { r := r + UInt(1) }
      }

      TLArbiter(TLArbiter.roundRobin)(out.a, (UInt(0), r_out), (in.aw.bits.len, w_out))

      val ok_b  = Wire(in.b)
      val ok_r  = Wire(in.r)

      val d_resp = Mux(out.d.bits.denied || out.d.bits.corrupt, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY)
      val d_hasData = edgeOut.hasData(out.d.bits)
      val d_last = edgeOut.last(out.d)

      out.d.ready := Mux(d_hasData, ok_r.ready, ok_b.ready)
      ok_r.valid := out.d.valid && d_hasData
      ok_b.valid := out.d.valid && !d_hasData

      ok_r.bits.id   := out.d.bits.source >> addedBits
      ok_r.bits.data := out.d.bits.data
      ok_r.bits.resp := d_resp
      ok_r.bits.last := d_last

      // AXI4 needs irrevocable behaviour
      in.r <> Queue.irrevocable(ok_r, 1, flow=true)

      ok_b.bits.id   := out.d.bits.source >> addedBits
      ok_b.bits.resp := d_resp

      // AXI4 needs irrevocable behaviour
      val q_b = Queue.irrevocable(ok_b, 1, flow=true)

      // We need to prevent sending B valid before the last W beat is accepted
      // TileLink allows early acknowledgement of a write burst, but AXI does not.
      val b_count = RegInit(Vec.fill(numIds) { UInt(0, width = txnCountBits) })
      val b_allow = b_count(in.b.bits.id) =/= w_count(in.b.bits.id)
      val b_sel = UIntToOH(in.b.bits.id, numIds)

      (b_sel.asBools zip b_count) foreach { case (s, r) =>
        when (in.b.fire() && s) { r := r + UInt(1) }
      }

      in.b.bits := q_b.bits
      in.b.valid := q_b.valid && b_allow
      q_b.ready := in.b.ready && b_allow

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
  def apply(wcorrupt: Boolean = false)(implicit p: Parameters) =
  {
    val axi42tl = LazyModule(new AXI4ToTL(wcorrupt))
    axi42tl.node
  }
}
