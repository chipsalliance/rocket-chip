// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.{min,max}
import TLMessages._

class TLCacheCork(unsafe: Boolean = false)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { case cp =>
      cp.copy(clients = cp.clients.map { c => c.copy(
        supportsProbe = TransferSizes.none,
        sourceId = IdRange(c.sourceId.start*2, c.sourceId.end*2))})},
    managerFn = { case mp =>
      mp.copy(
        endSinkId = 1,
        managers = mp.managers.map { m => m.copy(
          supportsAcquireB = if (m.regionType == RegionType.UNCACHED) m.supportsGet     else m.supportsAcquireB,
          supportsAcquireT = if (m.regionType == RegionType.UNCACHED) m.supportsPutFull else m.supportsAcquireT)})})

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val clients = edgeIn.client.clients
      val caches = clients.filter(_.supportsProbe)
      require (clients.size == 1 || caches.size == 0 || unsafe, "Only one client can safely use a TLCacheCork")
      require (caches.size <= 1 || unsafe, "Only one caching client allowed")
      edgeOut.manager.managers.foreach { case m =>
        require (!m.supportsAcquireB || unsafe, "Cannot support caches beyond the Cork")
        require (m.regionType <= RegionType.UNCACHED)
      }

      // The Cork turns [Acquire=>Get] => [AccessAckData=>GrantData]
      //            and [ReleaseData=>PutFullData] => [AccessAck=>ReleaseAck]
      // We need to encode information sufficient to reverse the transformation in output.
      // A caveat is that we get Acquire+Release with the same source and must keep the
      // source unique after transformation onto the A channel.
      // The coding scheme is:
      //   Put: 1, Release: 0 => AccessAck
      //   *: 0, Acquire: 1 => AccessAckData

      // Take requests from A to A or D (if BtoT Acquire)
      val a_a = Wire(out.a)
      val a_d = Wire(in.d)
      val isPut = in.a.bits.opcode === PutFullData || in.a.bits.opcode === PutPartialData
      val toD = (in.a.bits.opcode === AcquireBlock && in.a.bits.param === TLPermissions.BtoT) ||
                (in.a.bits.opcode === AcquirePerm)
      in.a.ready := Mux(toD, a_d.ready, a_a.ready)

      a_a.valid := in.a.valid && !toD
      a_a.bits := in.a.bits
      a_a.bits.source := in.a.bits.source << 1 | Mux(isPut, UInt(1), UInt(0))

      // Transform Acquire into Get
      when (in.a.bits.opcode === AcquireBlock || in.a.bits.opcode === AcquirePerm) {
        a_a.bits.opcode := Get
        a_a.bits.param  := UInt(0)
        a_a.bits.source := in.a.bits.source << 1 | UInt(1)
      }

      // Upgrades are instantly successful
      a_d.valid := in.a.valid && toD
      a_d.bits := edgeIn.Grant(
        fromSink = UInt(0),
        toSource = in.a.bits.source,
        lgSize   = in.a.bits.size,
        capPermissions = TLPermissions.toT)

      // Take ReleaseData from C to A; Release from C to D
      val c_a = Wire(out.a)
      c_a.valid := in.c.valid && in.c.bits.opcode === ReleaseData
      c_a.bits := edgeOut.Put(
        fromSource = in.c.bits.source << 1,
        toAddress  = in.c.bits.address,
        lgSize     = in.c.bits.size,
        data       = in.c.bits.data)._2

      // Releases without Data succeed instantly
      val c_d = Wire(in.d)
      c_d.valid := in.c.valid && in.c.bits.opcode === Release
      c_d.bits := edgeIn.ReleaseAck(in.c.bits)

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
        d_d.bits.param := TLPermissions.toT
      }
      when (out.d.bits.opcode === AccessAck && !out.d.bits.source(0)) {
        d_d.bits.opcode := ReleaseAck
      }

      // Combine the sources of messages into the channels
      TLArbiter(TLArbiter.lowestIndexFirst)(out.a, (edgeOut.numBeats1(c_a.bits), c_a), (edgeOut.numBeats1(a_a.bits), a_a))
      TLArbiter(TLArbiter.lowestIndexFirst)(in.d,  (edgeIn .numBeats1(d_d.bits), d_d), (UInt(0), Queue(c_d, 2)), (UInt(0), Queue(a_d, 2)))

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
    cork.node :=? x
    cork.node
  }
}
