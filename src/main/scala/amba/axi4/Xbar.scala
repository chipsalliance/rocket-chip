// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.unittest._
import freechips.rocketchip.tilelink._

class AXI4Xbar(
  arbitrationPolicy: TLArbiter.Policy = TLArbiter.roundRobin,
  maxFlightPerId:    Int = 7,
  awQueueDepth:      Int = 2)(implicit p: Parameters) extends LazyModule
{
  require (maxFlightPerId >= 1)
  require (awQueueDepth >= 1)

  val node = AXI4NexusNode(
    masterFn  = { seq =>
      seq(0).copy(
        userBits = seq.map(_.userBits).max,
        masters = (AXI4Xbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.masters map { master => master.copy(id = master.id.shift(range.start)) }
        }
      )
    },
    slaveFn = { seq =>
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        wcorrupt = seq.exists(_.wcorrupt),
        slaves = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"Xbar data widths don't match: ${port.slaves.map(_.name)} has ${port.beatBytes}B vs ${seq(0).slaves.map(_.name)} has ${seq(0).beatBytes}B")
          port.slaves
        }
      )
    })

  lazy val module = new LazyModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip

    // Grab the port ID mapping
    val inputIdRanges = AXI4Xbar.mapInputIds(edgesIn.map(_.master))

    // Find a good mask for address decoding
    val port_addrs = edgesOut.map(_.slave.slaves.map(_.address).flatten)
    val routingMask = AddressDecoder(port_addrs)
    val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
    val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))

    // To route W we need to record where the AWs went
    val awIn  = Seq.fill(io_in .size) { Module(new Queue(UInt(width = io_out.size), awQueueDepth, flow = true)) }
    val awOut = Seq.fill(io_out.size) { Module(new Queue(UInt(width = io_in .size), awQueueDepth, flow = true)) }

    val requestARIO = io_in.map  { i => Vec(outputPorts.map   { o => o(i.ar.bits.addr) }) }
    val requestAWIO = io_in.map  { i => Vec(outputPorts.map   { o => o(i.aw.bits.addr) }) }
    val requestROI  = io_out.map { o => inputIdRanges.map { i => i.contains(o.r.bits.id) } }
    val requestBOI  = io_out.map { o => inputIdRanges.map { i => i.contains(o.b.bits.id) } }

    // W follows the path dictated by the AW Q
    for (i <- 0 until io_in.size) { awIn(i).io.enq.bits := requestAWIO(i).asUInt }
    val requestWIO = awIn.map { q => if (io_out.size > 1) q.io.deq.bits.asBools else Seq(Bool(true)) }

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = AXI4BundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Transform input bundles
    val in = Wire(Vec(io_in.size, AXI4Bundle(wide_bundle)))
    for (i <- 0 until in.size) {
      in(i) <> io_in(i)

      // Handle size = 1 gracefully (Chisel3 empty range is broken)
      def trim(id: UInt, size: Int) = if (size <= 1) UInt(0) else id(log2Ceil(size)-1, 0)
      // Manipulate the AXI IDs to differentiate masters
      val r = inputIdRanges(i)
      in(i).aw.bits.id := io_in(i).aw.bits.id | UInt(r.start)
      in(i).ar.bits.id := io_in(i).ar.bits.id | UInt(r.start)
      io_in(i).r.bits.id := trim(in(i).r.bits.id, r.size)
      io_in(i).b.bits.id := trim(in(i).b.bits.id, r.size)

      if (io_out.size > 1) {
        // Block A[RW] if we switch ports, to ensure responses stay ordered (also: beware the dining philosophers)
        val endId = edgesIn(i).master.endId
        val arFIFOMap = Wire(init = Vec.fill(endId) { Bool(true) })
        val awFIFOMap = Wire(init = Vec.fill(endId) { Bool(true) })
        val arSel = UIntToOH(io_in(i).ar.bits.id, endId)
        val awSel = UIntToOH(io_in(i).aw.bits.id, endId)
        val rSel  = UIntToOH(io_in(i).r .bits.id, endId)
        val bSel  = UIntToOH(io_in(i).b .bits.id, endId)
        val arTag = OHToUInt(requestARIO(i).asUInt, io_out.size)
        val awTag = OHToUInt(requestAWIO(i).asUInt, io_out.size)

        for (master <- edgesIn(i).master.masters) {
          def idTracker(port: UInt, req_fire: Bool, resp_fire: Bool) = {
            if (master.maxFlight == Some(0)) {
              Bool(true)
            } else {
              val legalFlight = master.maxFlight.getOrElse(maxFlightPerId+1)
              val flight = legalFlight min maxFlightPerId
              val canOverflow = legalFlight > flight
              val count = RegInit(UInt(0, width = log2Ceil(flight+1)))
              val last = Reg(UInt(width = log2Ceil(io_out.size)))
              count := count + req_fire.asUInt - resp_fire.asUInt
              assert (!resp_fire || count =/= UInt(0))
              assert (!req_fire  || count =/= UInt(flight))
              when (req_fire) { last := port }
              // No need to track where it went if we cap it at 1 request
              val portMatch = if (flight == 1) { Bool(true) } else { last === port }
              (count === UInt(0) || portMatch) && (Bool(!canOverflow) || count =/= UInt(flight))
            }
          }

          for (id <- master.id.start until master.id.end) {
            arFIFOMap(id) := idTracker(
              arTag,
              arSel(id) && io_in(i).ar.fire(),
              rSel(id) && io_in(i).r.fire() && io_in(i).r.bits.last)
            awFIFOMap(id) := idTracker(
              awTag,
              awSel(id) && io_in(i).aw.fire(),
              bSel(id) && io_in(i).b.fire())
          }
        }

        val allowAR = arFIFOMap(io_in(i).ar.bits.id)
        in(i).ar.valid := io_in(i).ar.valid && allowAR
        io_in(i).ar.ready := in(i).ar.ready && allowAR

        // Keep in mind that slaves may do this: awready := wvalid, wready := awvalid
        // To not cause a loop, we cannot have: wvalid := awready

        // Block AW if we cannot record the W destination
        val allowAW = awFIFOMap(io_in(i).aw.bits.id)
        val latched = RegInit(Bool(false)) // cut awIn(i).enq.valid from awready
        in(i).aw.valid := io_in(i).aw.valid && (latched || awIn(i).io.enq.ready) && allowAW
        io_in(i).aw.ready := in(i).aw.ready && (latched || awIn(i).io.enq.ready) && allowAW
        awIn(i).io.enq.valid := io_in(i).aw.valid && !latched
        when (awIn(i).io.enq.fire()) { latched := Bool(true) }
        when (in(i).aw.fire()) { latched := Bool(false) }

        // Block W if we do not have an AW destination
        in(i).w.valid := io_in(i).w.valid && awIn(i).io.deq.valid // depends on awvalid (but not awready)
        io_in(i).w.ready := in(i).w.ready && awIn(i).io.deq.valid
        awIn(i).io.deq.ready := io_in(i).w.valid && io_in(i).w.bits.last && in(i).w.ready
      }
    }

    // Transform output bundles
    val out = Wire(Vec(io_out.size, AXI4Bundle(wide_bundle)))
    for (i <- 0 until out.size) {
      io_out(i) <> out(i)

      if (io_in.size > 1) {
        // Block AW if we cannot record the W source
        val latched = RegInit(Bool(false)) // cut awOut(i).enq.valid from awready
        io_out(i).aw.valid := out(i).aw.valid && (latched || awOut(i).io.enq.ready)
        out(i).aw.ready := io_out(i).aw.ready && (latched || awOut(i).io.enq.ready)
        awOut(i).io.enq.valid := out(i).aw.valid && !latched
        when (awOut(i).io.enq.fire()) { latched := Bool(true) }
        when (out(i).aw.fire()) { latched := Bool(false) }

        // Block W if we do not have an AW source
        io_out(i).w.valid := out(i).w.valid && awOut(i).io.deq.valid // depends on awvalid (but not awready)
        out(i).w.ready := io_out(i).w.ready && awOut(i).io.deq.valid
        awOut(i).io.deq.ready := out(i).w.valid && out(i).w.bits.last && io_out(i).w.ready
      }
    }

    // Fanout the input sources to the output sinks
    def transpose[T](x: Seq[Seq[T]]) = Seq.tabulate(x(0).size) { i => Seq.tabulate(x.size) { j => x(j)(i) } }
    val portsAROI = transpose((in  zip requestARIO) map { case (i, r) => AXI4Xbar.fanout(i.ar, r) })
    val portsAWOI = transpose((in  zip requestAWIO) map { case (i, r) => AXI4Xbar.fanout(i.aw, r) })
    val portsWOI  = transpose((in  zip requestWIO)  map { case (i, r) => AXI4Xbar.fanout(i.w,  r) })
    val portsRIO  = transpose((out zip requestROI)  map { case (o, r) => AXI4Xbar.fanout(o.r,  r) })
    val portsBIO  = transpose((out zip requestBOI)  map { case (o, r) => AXI4Xbar.fanout(o.b,  r) })

    // Arbitrate amongst the sources
    for (o <- 0 until out.size) {
      awOut(o).io.enq.bits := // Record who won AW arbitration to select W
        AXI4Arbiter.returnWinner(arbitrationPolicy)(out(o).aw, portsAWOI(o):_*).asUInt
      AXI4Arbiter(arbitrationPolicy)(out(o).ar, portsAROI(o):_*)
      // W arbitration is informed by the Q, not policy
      out(o).w.valid := Mux1H(awOut(o).io.deq.bits, portsWOI(o).map(_.valid))
      out(o).w.bits  := Mux1H(awOut(o).io.deq.bits, portsWOI(o).map(_.bits))
      portsWOI(o).zipWithIndex.map { case (p, i) =>
        if (in.size > 1) {
          p.ready := out(o).w.ready && awOut(o).io.deq.bits(i)
        } else {
          p.ready := out(o).w.ready
        }
      }
    }

    for (i <- 0 until in.size) {
      AXI4Arbiter(arbitrationPolicy)(in(i).r, portsRIO(i):_*)
      AXI4Arbiter(arbitrationPolicy)(in(i).b, portsBIO(i):_*)
    }
  }
}

object AXI4Xbar
{
  def apply(
    arbitrationPolicy: TLArbiter.Policy = TLArbiter.roundRobin,
    maxFlightPerId:    Int = 7,
    awQueueDepth:      Int = 2)(implicit p: Parameters) =
  {
    val axi4xbar = LazyModule(new AXI4Xbar(arbitrationPolicy, maxFlightPerId, awQueueDepth))
    axi4xbar.node
  }

  def mapInputIds(ports: Seq[AXI4MasterPortParameters]) = TLXbar.assignRanges(ports.map(_.endId))

  // Replicate an input port to each output port
  def fanout[T <: AXI4BundleBase](input: IrrevocableIO[T], select: Seq[Bool]) = {
    val filtered = Wire(Vec(select.size, input))
    for (i <- 0 until select.size) {
      filtered(i).bits := input.bits
      filtered(i).valid := input.valid && select(i)
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}

object AXI4Arbiter
{
  def apply[T <: Data](policy: TLArbiter.Policy)(sink: IrrevocableIO[T], sources: IrrevocableIO[T]*) {
    if (sources.isEmpty) {
      sink.valid := Bool(false)
    } else {
      returnWinner(policy)(sink, sources:_*)
    }
  }
  def returnWinner[T <: Data](policy: TLArbiter.Policy)(sink: IrrevocableIO[T], sources: IrrevocableIO[T]*) = {
    require (!sources.isEmpty)

    // The arbiter is irrevocable; when !idle, repeat last request
    val idle = RegInit(Bool(true))

    // Who wants access to the sink?
    val valids = sources.map(_.valid)
    val anyValid = valids.reduce(_ || _)
    // Arbitrate amongst the requests
    val readys = Vec(policy(valids.size, Cat(valids.reverse), idle).asBools)
    // Which request wins arbitration?
    val winner = Vec((readys zip valids) map { case (r,v) => r&&v })

    // Confirm the policy works properly
    require (readys.size == valids.size)
    // Never two winners
    val prefixOR = winner.scanLeft(Bool(false))(_||_).init
    assert((prefixOR zip winner) map { case (p,w) => !p || !w } reduce {_ && _})
    // If there was any request, there is a winner
    assert (!anyValid || winner.reduce(_||_))

    // The one-hot source granted access in the previous cycle
    val state = RegInit(Vec.fill(sources.size)(Bool(false)))
    val muxState = Mux(idle, winner, state)
    state := muxState

    // Determine when we go idle
    when (anyValid) { idle := Bool(false) }
    when (sink.fire()) { idle := Bool(true) }

    if (sources.size > 1) {
      val allowed = Mux(idle, readys, state)
      (sources zip allowed) foreach { case (s, r) =>
        s.ready := sink.ready && r
      }
    } else {
      sources(0).ready := sink.ready
    }

    sink.valid := Mux(idle, anyValid, Mux1H(state, valids))
    sink.bits := Mux1H(muxState, sources.map(_.bits))
    muxState
  }
}

class AXI4XbarFuzzTest(name: String, txns: Int, nMasters: Int, nSlaves: Int)(implicit p: Parameters) extends LazyModule
{
  val xbar = AXI4Xbar()
  val slaveSize = 0x1000
  val masterBandSize = slaveSize >> log2Ceil(nMasters)
  def filter(i: Int) = TLFilter.mSelectIntersect(AddressSet(i * masterBandSize, ~BigInt(slaveSize - masterBandSize)))

  val slaves = Seq.tabulate(nSlaves) { i => LazyModule(new AXI4RAM(AddressSet(slaveSize * i, slaveSize-1))) }
  slaves.foreach { s => (s.node
    := AXI4Fragmenter()
    := AXI4Buffer(BufferParams.flow)
    := AXI4Buffer(BufferParams.flow)
    := AXI4Delayer(0.25)
    := xbar) }

  val masters = Seq.fill(nMasters) { LazyModule(new TLFuzzer(txns, 4, nOrdered = Some(1))) }
  masters.zipWithIndex.foreach { case (m, i) => (xbar
    := AXI4Delayer(0.25)
    := AXI4Deinterleaver(4096)
    := TLToAXI4()
    := TLFilter(filter(i))
    := TLRAMModel(s"${name} Master $i")
    := m.node) }

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := masters.map(_.module.io.finished).reduce(_ || _)
  }
}

class AXI4XbarTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut21 = Module(LazyModule(new AXI4XbarFuzzTest("Xbar DUT21", txns, 2, 1)).module)
  val dut12 = Module(LazyModule(new AXI4XbarFuzzTest("Xbar DUT12", txns, 1, 2)).module)
  val dut22 = Module(LazyModule(new AXI4XbarFuzzTest("Xbar DUT22", txns, 2, 2)).module)
  io.finished := Seq(dut21, dut12, dut22).map(_.io.finished).reduce(_ || _)
}
