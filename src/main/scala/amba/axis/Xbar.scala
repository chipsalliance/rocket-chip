// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axis

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class AXISXbar(beatBytes: Int, policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) extends LazyModule
{
  val node = AXISNexusNode(
    masterFn  = { seq =>
      seq.foreach { port => require(port.userFields == seq(0).userFields) }
      seq(0).v1copy(
        beatBytes = Some(beatBytes),
        masters = (AXISXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.masters map { master => master.v1copy(sourceId = master.sourceId.shift(range.start))}})},
    slaveFn = { seq =>
      seq(0).v1copy(
        beatBytes = Some(beatBytes),
        slaves = (AXISXbar.mapOutputIds(seq) zip seq) flatMap { case (range, port) =>
          port.slaves.map { slave => slave.v1copy(destinationId = slave.destinationId + range.start)}})})

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip

    // Grab the port ID mapping
    val inputIdRanges = AXISXbar.mapInputIds(edgesIn.map(_.master))
    val outputIdRanges = AXISXbar.mapOutputIds(edgesOut.map(_.slave))

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = AXISBundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int) = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

    // Transform input bundle sources (dest uses global namespace on both sides)
    val in = Wire(Vec(io_in.size, AXISBundle(wide_bundle)))
    for (i <- 0 until in.size) {
      in(i) :<>= io_in(i)
      in(i).bits.lift(AXISId) foreach { _ := io_in(i).bits.id | inputIdRanges(i).start.U }
    }

    // Transform output bundle sinks (id use global namespace on both sides)
    val out = Wire(Vec(io_out.size, AXISBundle(wide_bundle)))
    for (o <- 0 until out.size) {
      io_out(o) :<>= out(o)
      io_out(o).bits.lift(AXISDest) foreach { _ := trim(out(o).bits.dest, outputIdRanges(o).size) }
    }

    // Fanout the input sources to the output sinks
    val request = in.map { i => outputIdRanges.map { o => o.contains(i.bits.dest) } }
    val ports = (in zip request) map { case (i, r) => AXISXbar.fanout(i, r) }

    // Arbitrate amongst the sources
    (out zip ports.transpose) map { case (o, i) => AXISXbar.arbitrate(policy)(o, i) }
  }
}

object AXISXbar
{
  def apply(beatBytes: Int, policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters): AXISNode =
  {
    val xbar = LazyModule(new AXISXbar(beatBytes, policy))
    xbar.node
  }

  def mapInputIds (ports: Seq[AXISMasterPortParameters]) = TLXbar.assignRanges(ports.map(_.endSourceId))
  def mapOutputIds(ports: Seq[AXISSlavePortParameters]) = TLXbar.assignRanges(ports.map(_.endDestinationId))

  def arbitrate(policy: TLArbiter.Policy)(sink: AXISBundle, sources: Seq[AXISBundle]): Unit = {
    if (sources.isEmpty) {
      sink.valid := false.B
    } else if (sources.size == 1) {
      sink :<>= sources.head
    } else {
      // The number of beats which remain to be sent
      val idle = RegInit(true.B)
      when (sink.valid) { idle := sink.bits.last && sink.ready }

      // Who wants access to the sink?
      val valids = sources.map(_.valid)
      // Arbitrate amongst the requests
      val readys = VecInit(policy(valids.size, Cat(valids.reverse), idle).asBools)
      // Which request wins arbitration?
      val winner = VecInit((readys zip valids) map { case (r,v) => r&&v })

      // Confirm the policy works properly
      require (readys.size == valids.size)
      // Never two winners
      val prefixOR = winner.scanLeft(false.B)(_||_).init
      assert((prefixOR zip winner) map { case (p,w) => !p || !w } reduce {_ && _})
      // If there was any request, there is a winner
      assert (!valids.reduce(_||_) || winner.reduce(_||_))

      // The one-hot source granted access in the previous cycle
      val state = RegInit(VecInit.tabulate(sources.size)(_ => false.B))
      val muxState = Mux(idle, winner, state)
      state := muxState

      val allowed = Mux(idle, readys, state)
      (sources zip allowed) foreach { case (s, r) => s.ready := sink.ready && r }
      sink.valid := Mux(idle, valids.reduce(_||_), Mux1H(state, valids))
      Connectable.waiveUnmatched(sink.bits, Mux1H(muxState, sources.map(_.bits))) match {
        case (lhs, rhs) => lhs.squeezeAll :<= rhs.squeezeAll
      }
    }
  }

  def fanout(input: AXISBundle, select: Seq[Bool]): Seq[AXISBundle] = {
    val filtered = Wire(Vec(select.size, chiselTypeOf(input)))
    for (i <- 0 until select.size) {
      Connectable.waiveUnmatched(filtered(i).bits, input.bits) match {
        case (lhs, rhs) => lhs :<= rhs
      }
      filtered(i).valid := input.valid && (select(i) || (select.size == 1).B)
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}
