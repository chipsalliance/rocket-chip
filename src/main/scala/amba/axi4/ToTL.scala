// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class AXI4ToTLIdMapEntry(tlId: IdRange, axi4Id: IdRange, name: String)
  extends IdMapEntry
{
  val from = axi4Id
  val to = tlId
  val isCache = false
  val requestFifo = false
  val maxTransactionsInFlight = Some(tlId.size)
}

case class AXI4ToTLNode(wcorrupt: Boolean)(implicit valName: ValName) extends MixedAdapterNode(AXI4Imp, TLImp)(
  dFn = { case mp =>
    mp.masters.foreach { m => require (m.maxFlight.isDefined, "AXI4 must include a transaction maximum per ID to convert to TL") }
    val maxFlight = mp.masters.map(_.maxFlight.get).max
    TLMasterPortParameters.v1(
      clients = mp.masters.filter(_.maxFlight != Some(0)).flatMap { m =>
        for (id <- m.id.start until m.id.end)
          yield TLMasterParameters.v1(
            name        = s"${m.name} ID#${id}",
            sourceId    = IdRange(id * maxFlight*2, (id+1) * maxFlight*2), // R+W ids are distinct
            nodePath    = m.nodePath,
            requestFifo = true)
      },
      echoFields    = mp.echoFields,
      requestFields = AMBAProtField() +: mp.requestFields,
      responseKeys  = mp.responseKeys)
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
    minLatency = mp.minLatency,
    responseFields = mp.responseFields,
    requestKeys    = (if (wcorrupt) Seq(AMBACorrupt) else Seq()) ++ mp.requestKeys.filter(_ != AMBAProt))
  })

/**
  * Convert AXI4 master to TileLink.
  *
  * You can use this adapter to connect external AXI4 masters to TileLink bus topology.
  *
  * Setting wcorrupt=true is insufficient to enable w.user.corrupt.
  * One must additionally list it in the AXI4 master's requestFields.
  *
  * @param wcorrupt enable AMBACorrupt in w.user
  */
class AXI4ToTL(wcorrupt: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4ToTLNode(wcorrupt)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val numIds = edgeIn.master.endId
      val beatBytes = edgeOut.manager.beatBytes
      val beatCountBits = AXI4Parameters.lenBits + (1 << AXI4Parameters.sizeBits) - 1
      val maxFlight = edgeIn.master.masters.map(_.maxFlight.get).max
      val logFlight = log2Ceil(maxFlight)
      val txnCountBits = log2Ceil(maxFlight+1) // wrap-around must not block b_allow
      val addedBits = logFlight + 1 // +1 for read vs. write source ID

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

      val r_out = WireDefault(out.a)
      val r_size1 = in.ar.bits.bytes1()
      val r_size = OH1ToUInt(r_size1)
      val r_ok = edgeOut.manager.supportsGetSafe(in.ar.bits.addr, r_size)
      val r_addr = Mux(r_ok, in.ar.bits.addr, error.U | in.ar.bits.addr(log2Up(beatBytes)-1, 0))
      val r_count = RegInit(VecInit.fill(numIds) { 0.U(txnCountBits.W) })
      val r_id = if (maxFlight == 1) {
        Cat(in.ar.bits.id, 0.U(1.W))
      } else {
        Cat(in.ar.bits.id, r_count(in.ar.bits.id)(logFlight-1,0), 0.U(1.W))
      }

      assert (!in.ar.valid || r_size1 === UIntToOH1(r_size, beatCountBits)) // because aligned
      in.ar.ready := r_out.ready
      r_out.valid := in.ar.valid
      r_out.bits :<= edgeOut.Get(r_id, r_addr, r_size)._2

      Connectable.waiveUnmatched(r_out.bits.user, in.ar.bits.user) match {
        case (lhs, rhs) => lhs.squeezeAll :<= rhs.squeezeAll
      }

      r_out.bits.user.lift(AMBAProt).foreach { rprot =>
        rprot.privileged :=  in.ar.bits.prot(0)
        rprot.secure     := !in.ar.bits.prot(1)
        rprot.fetch      :=  in.ar.bits.prot(2)
        rprot.bufferable :=  in.ar.bits.cache(0)
        rprot.modifiable :=  in.ar.bits.cache(1)
        rprot.readalloc  :=  in.ar.bits.cache(2)
        rprot.writealloc :=  in.ar.bits.cache(3)
      }

      val r_sel = UIntToOH(in.ar.bits.id, numIds)
      (r_sel.asBools zip r_count) foreach { case (s, r) =>
        when (in.ar.fire && s) { r := r + 1.U }
      }

      val w_out = WireDefault(out.a)
      val w_size1 = in.aw.bits.bytes1()
      val w_size = OH1ToUInt(w_size1)
      val w_ok = edgeOut.manager.supportsPutPartialSafe(in.aw.bits.addr, w_size)
      val w_addr = Mux(w_ok, in.aw.bits.addr, error.U | in.aw.bits.addr(log2Up(beatBytes)-1, 0))
      val w_count = RegInit(VecInit.fill(numIds) { 0.U(txnCountBits.W) })
      val w_id = if (maxFlight == 1) {
        Cat(in.aw.bits.id, 1.U(1.W))
      } else {
        Cat(in.aw.bits.id, w_count(in.aw.bits.id)(logFlight-1,0), 1.U(1.W))
      }

      assert (!in.aw.valid || w_size1 === UIntToOH1(w_size, beatCountBits)) // because aligned
      assert (!in.aw.valid || in.aw.bits.len === 0.U || in.aw.bits.size === log2Ceil(beatBytes).U) // because aligned
      in.aw.ready := w_out.ready && in.w.valid && in.w.bits.last
      in.w.ready  := w_out.ready && in.aw.valid
      w_out.valid := in.aw.valid && in.w.valid
      w_out.bits :<= edgeOut.Put(w_id, w_addr, w_size, in.w.bits.data, in.w.bits.strb)._2
      in.w.bits.user.lift(AMBACorrupt).foreach { w_out.bits.corrupt := _ }

      Connectable.waiveUnmatched(w_out.bits.user, in.aw.bits.user) match {
        case (lhs, rhs) => lhs.squeezeAll :<= rhs.squeezeAll
      }

      w_out.bits.user.lift(AMBAProt).foreach { wprot =>
        wprot.privileged :=  in.aw.bits.prot(0)
        wprot.secure     := !in.aw.bits.prot(1)
        wprot.fetch      :=  in.aw.bits.prot(2)
        wprot.bufferable :=  in.aw.bits.cache(0)
        wprot.modifiable :=  in.aw.bits.cache(1)
        wprot.readalloc  :=  in.aw.bits.cache(2)
        wprot.writealloc :=  in.aw.bits.cache(3)
      }

      val w_sel = UIntToOH(in.aw.bits.id, numIds)
      (w_sel.asBools zip w_count) foreach { case (s, r) =>
        when (in.aw.fire && s) { r := r + 1.U }
      }

      TLArbiter(TLArbiter.roundRobin)(out.a, (0.U, r_out), (in.aw.bits.len, w_out))

      val ok_b  = WireDefault(in.b)
      val ok_r  = WireDefault(in.r)

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
      ok_r.bits.user :<= out.d.bits.user

      // AXI4 needs irrevocable behaviour
      in.r :<>= Queue.irrevocable(ok_r, 1, flow=true)

      ok_b.bits.id   := out.d.bits.source >> addedBits
      ok_b.bits.resp := d_resp
      ok_b.bits.user :<= out.d.bits.user

      // AXI4 needs irrevocable behaviour
      val q_b = Queue.irrevocable(ok_b, 1, flow=true)

      // We need to prevent sending B valid before the last W beat is accepted
      // TileLink allows early acknowledgement of a write burst, but AXI does not.
      val b_count = RegInit(VecInit.fill(numIds) { 0.U(txnCountBits.W) })
      val b_allow = b_count(in.b.bits.id) =/= w_count(in.b.bits.id)
      val b_sel = UIntToOH(in.b.bits.id, numIds)

      (b_sel.asBools zip b_count) foreach { case (s, r) =>
        when (in.b.fire && s) { r := r + 1.U }
      }

      in.b.bits :<= q_b.bits
      in.b.valid := q_b.valid && b_allow
      q_b.ready := in.b.ready && b_allow

      // Unused channels
      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B
    }
  }
}

class AXI4BundleRError(params: AXI4BundleParameters) extends AXI4BundleBase(params)
{
  val id   = UInt(params.idBits.W)
  val last = Bool()
}

object AXI4ToTL
{
  def apply(wcorrupt: Boolean = true)(implicit p: Parameters) =
  {
    val axi42tl = LazyModule(new AXI4ToTL(wcorrupt))
    axi42tl.node
  }
}
