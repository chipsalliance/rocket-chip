// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import scala.math.{min,max}
import TLMessages._

class TLCacheCork(unsafe: Boolean = false)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { case cp =>
      cp.copy(clients = cp.clients.map { c => c.copy(
        sourceId = IdRange(c.sourceId.start*2, c.sourceId.end*2))})},
    managerFn = { case mp =>
      mp.copy(managers = mp.managers.map { m => m.copy(
        regionType         = if (m.regionType == RegionType.UNCACHED) RegionType.TRACKED else m.regionType,
        supportsAcquireB   = m.supportsGet,
        supportsAcquireT   = m.supportsPutFull)})})

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      require (edgeIn.client.clients.size == 1 || unsafe, "Only one client can safely use a TLCacheCork")
      require (edgeIn.client.clients.filter(_.supportsProbe).size <= 1, "Only one caching client allowed")
      edgeOut.manager.managers.foreach { case m =>
        require (!m.supportsAcquireB, "Cannot support caches beyond the Cork")
      }

      // The Cork turns [Acquire=>Get] => [AccessAckData=>GrantData]
      //            and [ReleaseData=>PutFullData] => [AccessAck=>ReleaseAck]
      // We need to encode information sufficient to reverse the transformation in output.
      // A caveat is that we get Acquire+Release with the same source and must keep the
      // source unique after transformation onto the A channel.
      // The coding scheme is:
      //   Put: 1, Release: 0 => AccessAck
      //   *: 0, Acquire: 1 => AccessAckData

      // Take requests from A to A
      val isPut = in.a.bits.opcode === PutFullData || in.a.bits.opcode === PutPartialData
      val a_a = Wire(out.a)
      a_a <> in.a
      a_a.bits.source := in.a.bits.source << 1 | Mux(isPut, UInt(1), UInt(0))

      // Transform Acquire into Get
      when (in.a.bits.opcode === Acquire) {
        a_a.bits.opcode := Get
        a_a.bits.param  := UInt(0)
        a_a.bits.source := in.a.bits.source << 1 | UInt(1)
      }

      // Take ReleaseData from C to A; Release from C to D
      val c_a = Wire(out.a)
      c_a.valid := in.c.valid && in.c.bits.opcode === ReleaseData
      c_a.bits.opcode  := PutFullData
      c_a.bits.param   := UInt(0)
      c_a.bits.size    := in.c.bits.size
      c_a.bits.source  := in.c.bits.source << 1
      c_a.bits.address := in.c.bits.address
      c_a.bits.mask    := edgeOut.mask(in.c.bits.address, in.c.bits.size)
      c_a.bits.data    := in.c.bits.data

      val c_d = Wire(in.d)
      c_d.valid := in.c.valid && in.c.bits.opcode === Release
      c_d.bits.opcode  := ReleaseAck
      c_d.bits.param   := UInt(0)
      c_d.bits.size    := in.c.bits.size
      c_d.bits.source  := in.c.bits.source
      c_d.bits.sink    := UInt(0)
      c_d.bits.addr_lo := in.c.bits.address
      c_d.bits.data    := UInt(0)
      c_d.bits.error   := Bool(false)

      assert (!in.c.valid || in.c.bits.opcode === Release || in.c.bits.opcode === ReleaseData)
      in.c.ready := Mux(in.c.bits.opcode === Release, c_d.ready, c_a.ready)

      // Discard E
      in.e.ready := Bool(true)

      // Block B; should never happen
      out.b.ready := Bool(false)
      assert (!out.b.valid)

      // Take responses from D and transform them
      val d_d = Wire(in.d)
      d_d <> out.d
      d_d.bits.source := out.d.bits.source >> 1

      when (out.d.bits.opcode === AccessAckData && out.d.bits.source(0)) {
        d_d.bits.opcode := GrantData
        d_d.bits.param  := TLPermissions.toT
      }
      when (out.d.bits.opcode === AccessAck && !out.d.bits.source(0)) {
        d_d.bits.opcode := ReleaseAck
      }

      // Combine the sources of messages into the channels
      TLArbiter(TLArbiter.lowestIndexFirst)(out.a, (edgeOut.numBeats1(c_a.bits), c_a), (edgeOut.numBeats1(a_a.bits), a_a))
      TLArbiter(TLArbiter.lowestIndexFirst)(in.d,  (edgeIn .numBeats1(d_d.bits), d_d), (UInt(0), Queue(c_d, 2)))

      // Tie off unused ports
      in.b.valid := Bool(false)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
    }
  }
}

object TLCacheCork
{
  // applied to the TL source node; y.node := TLCacheCork()(x.node)
  def apply(unsafe: Boolean = false)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val cork = LazyModule(new TLCacheCork(unsafe))
    cork.node := x
    cork.node
  }
}
