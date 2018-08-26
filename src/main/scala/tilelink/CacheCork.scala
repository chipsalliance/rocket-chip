// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.{min,max}
import TLMessages._

class TLCacheCork(unsafe: Boolean = false, sinkIds: Int = 8)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { case cp =>
      cp.copy(clients = cp.clients.map { c => c.copy(
        supportsProbe = TransferSizes.none,
        sourceId = IdRange(c.sourceId.start*2, c.sourceId.end*2))})},
    managerFn = { case mp =>
      mp.copy(
        endSinkId = if (mp.managers.exists(_.regionType == RegionType.UNCACHED)) sinkIds else 0,
        managers = mp.managers.map { m => m.copy(
          supportsAcquireB = if (m.regionType == RegionType.UNCACHED) m.supportsGet     else m.supportsAcquireB,
          supportsAcquireT = if (m.regionType == RegionType.UNCACHED) m.supportsPutFull else m.supportsAcquireT,
          alwaysGrantsT    = if (m.regionType == RegionType.UNCACHED) m.supportsPutFull else m.alwaysGrantsT)})})

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      // If this adapter does not need to do anything, toss all the above work and just directly connect
      if (!edgeIn.manager.anySupportAcquireB) {
        out <> in
      } else {
        val clients = edgeIn.client.clients
        val caches = clients.filter(_.supportsProbe)
        require (clients.size == 1 || caches.size == 0 || unsafe, s"Only one client can safely use a TLCacheCork; ${clients.map(_.name)}")
        require (caches.size <= 1 || unsafe, s"Only one caching client allowed; ${clients.map(_.name)}")
        edgeOut.manager.managers.foreach { case m =>
          require (!m.supportsAcquireB || unsafe, s"Cannot support caches beyond the Cork; ${m.name}")
          require (m.regionType <= RegionType.UNCACHED)
        }

        // The Cork turns [Acquire=>Get] => [AccessAckData=>GrantData]
        //            and [ReleaseData=>PutFullData] => [AccessAck=>ReleaseAck]
        // We need to encode information sufficient to reverse the transformation in output.
        // A caveat is that we get Acquire+Release with the same source and must keep the
        // source unique after transformation onto the A channel.
        // The coding scheme is:
        //   Release, AcquireBlock.BtoT, AcquirePerm => instant response
        //   Put{Full,Partial}Data: 1, ReleaseData: 0 => AccessAck
        //   {Arithmetic,Logical}Data,Get: 0, Acquire: 1 => AccessAckData
        //   Hint:0 => HintAck

        // The CacheCork can potentially send the same source twice if a client sends
        // simultaneous Release and AMO/Get with the same source. It will still correctly
        // decode the messages based on the D.opcode, but the double use violates the spec.
        // Fortunately, no masters we know of behave this way!

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
          data       = in.c.bits.data,
          corrupt    = in.c.bits.corrupt)._2

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

        // Track in-flight sinkIds
        val pool = Module(new IDPool(sinkIds))
        pool.io.free.valid := in.e.fire()
        pool.io.free.bits  := in.e.bits.sink

        val in_d = Wire(in.d)
        val d_first = edgeOut.first(in_d)
        val d_grant = in_d.bits.opcode === GrantData || in_d.bits.opcode === Grant
        pool.io.alloc.ready := in.d.fire() && d_first && d_grant
        in.d.valid := in_d.valid && (pool.io.alloc.valid || !d_first || !d_grant)
        in_d.ready := in.d.ready && (pool.io.alloc.valid || !d_first || !d_grant)
        in.d.bits := in_d.bits
        in.d.bits.sink := pool.io.alloc.bits holdUnless d_first

        // Take responses from D and transform them
        val d_d = Wire(in.d)
        d_d <> out.d
        d_d.bits.source := out.d.bits.source >> 1

        // Record if a target was writable and auto-promote toT if it was
        // This is structured so that the vector can be constant prop'd away
        val wSourceVec = Reg(Vec(edgeIn.client.endSourceId, Bool()))
        val aWOk = edgeIn.manager.fastProperty(in.a.bits.address, !_.supportsPutFull.none, (b:Boolean) => Bool(b))
        val dWOk = wSourceVec(d_d.bits.source)
        val bypass = Bool(edgeIn.manager.minLatency == 0) && in.a.valid && in.a.bits.source === d_d.bits.source
        val dWHeld = Mux(bypass, aWOk, dWOk) holdUnless d_first

        when (in.a.fire()) {
          wSourceVec(in.a.bits.source) := aWOk
        }

        // Wipe out any unused registers
        edgeIn.client.unusedSources.foreach { id =>
          wSourceVec(id) := Bool(edgeIn.manager.anySupportPutFull)
        }

        when (out.d.bits.opcode === AccessAckData && out.d.bits.source(0)) {
          d_d.bits.opcode := GrantData
          d_d.bits.param := Mux(dWHeld, TLPermissions.toT, TLPermissions.toB)
        }
        when (out.d.bits.opcode === AccessAck && !out.d.bits.source(0)) {
          d_d.bits.opcode := ReleaseAck
        }

        // Combine the sources of messages into the channels
        TLArbiter(TLArbiter.lowestIndexFirst)(out.a, (edgeOut.numBeats1(c_a.bits), c_a), (edgeOut.numBeats1(a_a.bits), a_a))
        TLArbiter(TLArbiter.lowestIndexFirst)(in_d,  (edgeIn .numBeats1(d_d.bits), d_d), (UInt(0), Queue(c_d, 2)), (UInt(0), Queue(a_d, 2)))

        // Tie off unused ports
        in.b.valid := Bool(false)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }
    }
  }
}

object TLCacheCork
{
  def apply(unsafe: Boolean = false, sinkIds: Int = 8)(implicit p: Parameters): TLNode =
  {
    val cork = LazyModule(new TLCacheCork(unsafe, sinkIds))
    cork.node
  }
}
