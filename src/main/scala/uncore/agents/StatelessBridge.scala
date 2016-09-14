// See LICENSE for license details.

package uncore.agents

import Chisel._
import uncore.coherence._
import uncore.tilelink._
import uncore.constants._
import uncore.devices._
import cde.{Parameters, Field, Config}

/** The ManagerToClientStateless Bridge does not maintain any state for the messages
  *  which pass through it. It simply passes the messages back and forth without any
  *  tracking or translation. 
  *  
  * This can reduce area and timing in very constrained situations:
  *   - The Manager and Client implement the same coherence protocol
  *   - There are no probe or finish messages.
  *   - The outer transaction ID is large enough to handle all possible inner
  *     transaction IDs, such that no remapping state must be maintained.
  *
  * This bridge DOES NOT keep the uncached channel coherent with the cached
  * channel. Uncached requests to blocks cached by the L1 will not probe the L1.
  * As a result, uncached reads to cached blocks will get stale data until
  * the L1 performs a voluntary writeback, and uncached writes to cached blocks
  * will get lost, as the voluntary writeback from the L1 will overwrite the
  * changes. If your tile relies on probing the L1 data cache in order to
  * share data between the instruction cache and data cache (e.g. you are using
  * a non-blocking L1 D$) or if the tile has uncached channels capable of
  * writes (e.g. Hwacha and other RoCC accelerators), DO NOT USE THIS BRIDGE.
  */

class ManagerToClientStatelessBridge(_clock: Clock = null, _reset: Bool = null)
    (implicit p: Parameters) extends HierarchicalCoherenceAgent(_clock, _reset)(p) {
  val icid = io.inner.tlClientIdBits
  val ixid = io.inner.tlClientXactIdBits
  val oxid = io.outer.tlClientXactIdBits

  val innerCoh = io.inner.tlCoh.getClass
  val outerCoh = io.outer.tlCoh.getClass

  // Stateless Bridge is only usable in certain constrained situations.
  // Sanity check its usage here.

  require(io.inner.tlNCachingClients <= 1)
  require(icid + ixid <= oxid)
  require(innerCoh eq outerCoh,
    s"Coherence policies do not match: inner is ${innerCoh.getSimpleName}, outer is ${outerCoh.getSimpleName}")

  io.outer.acquire.valid := io.inner.acquire.valid
  io.inner.acquire.ready := io.outer.acquire.ready
  io.outer.acquire.bits := io.inner.acquire.bits
  io.outer.acquire.bits.client_xact_id := Cat(io.inner.acquire.bits.client_id, io.inner.acquire.bits.client_xact_id)

  io.outer.release.valid := io.inner.release.valid
  io.inner.release.ready := io.outer.release.ready
  io.outer.release.bits := io.inner.release.bits
  io.outer.release.bits.client_xact_id := Cat(io.inner.release.bits.client_id, io.inner.release.bits.client_xact_id)

  io.inner.grant.valid := io.outer.grant.valid
  io.outer.grant.ready := io.inner.grant.ready
  io.inner.grant.bits := io.outer.grant.bits
  io.inner.grant.bits.client_xact_id := io.outer.grant.bits.client_xact_id(ixid-1, 0)
  io.inner.grant.bits.client_id := io.outer.grant.bits.client_xact_id(icid+ixid-1, ixid)

  io.inner.probe.valid := Bool(false)
  io.inner.finish.ready := Bool(true)

  disconnectOuterProbeAndFinish()
}
