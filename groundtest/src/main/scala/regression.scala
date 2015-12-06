package groundtest

import Chisel._
import uncore._
import junctions.{MMIOBase, ParameterizedBundle}
import cde.Parameters

class RegressionIO(implicit val p: Parameters) extends GroundTestIO()(p) {
  val start = Bool(INPUT)
}

abstract class Regression(implicit val p: Parameters) extends Module {
  val io = new RegressionIO
}

/**
 * This was a bug in which the TileLinkIONarrower logic screwed up
 * when a PutBlock request and a narrow Get request are sent to it at the
 * same time. Repeating this sequence enough times will cause a queue to
 * get filled up and deadlock the system.
 */
class IOGetAfterPutBlockRegression(implicit p: Parameters)
    extends Regression()(p) with HasTileLinkParameters {

  val nRuns = 7
  val run = Reg(init = UInt(0, log2Up(nRuns + 1)))

  val (put_beat, put_done) = Counter(
    io.mem.acquire.fire() && io.mem.acquire.bits.hasData(), tlDataBeats)

  val started = Reg(init = Bool(false))
  val put_sent = Reg(init = Bool(false))
  val get_sent = Reg(init = Bool(false))
  val put_acked = Reg(init = Bool(false))
  val get_acked = Reg(init = Bool(false))
  val both_acked = put_acked && get_acked

  when (!started && io.start) { started := Bool(true) }

  io.mem.acquire.valid := !put_sent && started
  io.mem.acquire.bits := PutBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(0),
    addr_beat = put_beat,
    data = UInt(0))
  io.mem.grant.ready := Bool(true)

  io.cache.req.valid := !get_sent && started
  io.cache.req.bits.addr := UInt(p(MMIOBase))
  io.cache.req.bits.typ := MT_W
  io.cache.req.bits.cmd := M_XRD
  io.cache.req.bits.tag := UInt(0)
  io.cache.req.bits.kill := Bool(false)
  io.cache.req.bits.phys := Bool(true)

  when (put_done) { put_sent := Bool(true) }
  when (io.cache.req.fire()) { get_sent := Bool(true) }
  when (io.mem.grant.fire()) { put_acked := Bool(true) }
  when (io.cache.resp.valid) { get_acked := Bool(true) }

  when (both_acked) {
    when (run < UInt(nRuns - 1)) {
      put_sent := Bool(false)
      get_sent := Bool(false)
    }
    put_acked := Bool(false)
    get_acked := Bool(false)
    run := run + UInt(1)
  }

  io.finished := (run === UInt(nRuns))
}

class RegressionTest(implicit p: Parameters) extends GroundTest()(p) {

  val regressions = Seq(
    Module(new IOGetAfterPutBlockRegression))
  val regressIOs = Vec(regressions.map(_.io))
  val regress_idx = Reg(init = UInt(0, log2Up(regressions.size + 1)))
  val all_done = (regress_idx === UInt(regressions.size))
  val start = Reg(init = Bool(true))

  when (start) { start := Bool(false) }

  regressIOs.zipWithIndex.foreach { case (regress, i) =>
    val me = regress_idx === UInt(i)
    regress.start := me && start
    regress.mem.acquire.ready := io.mem.acquire.ready && me
    regress.mem.grant.valid := io.mem.grant.valid && me
    regress.mem.grant.bits := io.mem.grant.bits
    regress.cache.req.ready := io.cache.req.ready && me
    regress.cache.resp.valid := io.cache.resp.valid && me
  }

  val cur_regression = regressIOs(regress_idx)
  val cur_acquire = cur_regression.mem.acquire
  val cur_grant = cur_regression.mem.grant
  val cur_cache_req = cur_regression.cache.req

  io.mem.acquire.valid := cur_acquire.valid
  io.mem.acquire.bits := cur_acquire.bits
  io.mem.grant.ready := cur_grant.ready
  io.cache.req.valid := cur_cache_req.valid
  io.cache.req.bits := cur_cache_req.bits

  when (cur_regression.finished && !all_done) {
    start := Bool(true)
    regress_idx := regress_idx + UInt(1)
  }

  io.finished := all_done

  val timeout = Timer(5000, start, cur_regression.finished)
  assert(!timeout, "Regression timed out")
}
