// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// Trades off slave port proximity against routing resource cost
object ForceFanout
{
  def apply[T](
    a: TriStateValue = TriStateValue.unset,
    b: TriStateValue = TriStateValue.unset,
    c: TriStateValue = TriStateValue.unset,
    d: TriStateValue = TriStateValue.unset,
    e: TriStateValue = TriStateValue.unset)(body: Parameters => T)(implicit p: Parameters) =
  {
    body(p.alterPartial {
      case ForceFanoutKey => p(ForceFanoutKey) match {
        case ForceFanoutParams(pa, pb, pc, pd, pe) =>
          ForceFanoutParams(a.update(pa), b.update(pb), c.update(pc), d.update(pd), e.update(pe))
      }
    })
  }
}

private case class ForceFanoutParams(a: Boolean, b: Boolean, c: Boolean, d: Boolean, e: Boolean)
private case object ForceFanoutKey extends Field(ForceFanoutParams(false, false, false, false, false))

class TLXbar(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) extends LazyModule
{
  val node = TLNexusNode(
    clientFn  = { seq =>
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client => client.copy(
            sourceId = client.sourceId.shift(range.start)
          )}
        }
      )
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"Xbar data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager => manager.copy(
            fifoId = manager.fifoId.map(fifoIdMapper(_))
          )}
        }
      )
    })

  lazy val module = new LazyModuleImp(this) {
    if ((node.in.size * node.out.size) > (8*32)) {
      println (s"!!! WARNING !!!")
      println (s" Your TLXbar ($name) is very large, with ${node.in.size} Masters and ${node.out.size} Slaves.")
      println (s"!!! WARNING !!!")
    }

    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip

    // Grab the port ID mapping
    val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
    val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))

    // Find a good mask for address decoding
    val port_addrs = edgesOut.map(_.manager.managers.map(_.address).flatten)
    val routingMask = AddressDecoder(port_addrs)
    val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
    val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))

    // Print the address mapping
    if (false) {
      println("Xbar mapping:")
      route_addrs.foreach { p =>
        print(" ")
        p.foreach { a => print(s" ${a}") }
        println("")
      }
      println("--")
    }

    // Print the ID mapping
    if (false) {
      println(s"XBar ${name} mapping:")
      (edgesIn zip inputIdRanges).zipWithIndex.foreach { case ((edge, id), i) =>
        println(s"\t$i assigned ${id} for ${edge.client.clients.map(_.name).mkString(", ")}")
      }
      println("")
    }

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = TLBundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int) = if (size <= 1) UInt(0) else id(log2Ceil(size)-1, 0)

    // Transform input bundle sources (sinks use global namespace on both sides)
    val in = Wire(Vec(io_in.size, TLBundle(wide_bundle)))
    for (i <- 0 until in.size) {
      val r = inputIdRanges(i)

      in(i).a <> io_in(i).a
      io_in(i).d <> in(i).d
      in(i).a.bits.source := io_in(i).a.bits.source | UInt(r.start)
      io_in(i).d.bits.source := trim(in(i).d.bits.source, r.size)

      if (edgesIn(i).client.anySupportProbe && edgesOut.exists(_.manager.anySupportAcquireB)) {
        in(i).c <> io_in(i).c
        in(i).e <> io_in(i).e
        io_in(i).b <> in(i).b
        in(i).c.bits.source := io_in(i).c.bits.source | UInt(r.start)
        io_in(i).b.bits.source := trim(in(i).b.bits.source, r.size)
      } else {
        in(i).c.valid := Bool(false)
        in(i).e.valid := Bool(false)
        in(i).b.ready := Bool(false)
        io_in(i).c.ready := Bool(true)
        io_in(i).e.ready := Bool(true)
        io_in(i).b.valid := Bool(false)
      }
    }

    // Transform output bundle sinks (sources use global namespace on both sides)
    val out = Wire(Vec(io_out.size, TLBundle(wide_bundle)))
    for (i <- 0 until out.size) {
      val r = outputIdRanges(i)

      io_out(i).a <> out(i).a
      out(i).d <> io_out(i).d
      out(i).d.bits.sink := io_out(i).d.bits.sink | UInt(r.start)

      if (edgesOut(i).manager.anySupportAcquireB && edgesIn.exists(_.client.anySupportProbe)) {
        io_out(i).c <> out(i).c
        io_out(i).e <> out(i).e
        out(i).b <> io_out(i).b
        io_out(i).e.bits.sink := trim(out(i).e.bits.sink, r.size)
      } else {
        out(i).c.ready := Bool(false)
        out(i).e.ready := Bool(false)
        out(i).b.valid := Bool(false)
        io_out(i).c.valid := Bool(false)
        io_out(i).e.valid := Bool(false)
        io_out(i).b.ready := Bool(true)
      }
    }

    val addressA = (in zip edgesIn) map { case (i, e) => e.address(i.a.bits) }
    val addressC = (in zip edgesIn) map { case (i, e) => e.address(i.c.bits) }

    val requestAIO = addressA.map { i => outputPorts.map { o => o(i) } }
    val requestCIO = addressC.map { i => outputPorts.map { o => o(i) } }
    val requestBOI = out.map { o => inputIdRanges.map  { i => i.contains(o.b.bits.source) } }
    val requestDOI = out.map { o => inputIdRanges.map  { i => i.contains(o.d.bits.source) } }
    val requestEIO = in.map  { i => outputIdRanges.map { o => o.contains(i.e.bits.sink) } }

    val beatsAI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.a.bits) }
    val beatsBO = (out zip edgesOut) map { case (o, e) => e.numBeats1(o.b.bits) }
    val beatsCI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.c.bits) }
    val beatsDO = (out zip edgesOut) map { case (o, e) => e.numBeats1(o.d.bits) }
    val beatsEI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.e.bits) }

    // Which pairs support support transfers
    def transpose[T](x: Seq[Seq[T]]) = if (x.isEmpty) Nil else Seq.tabulate(x(0).size) { i => Seq.tabulate(x.size) { j => x(j)(i) } }
    def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data zip mask).filter(_._2).map(_._1)

    // Fanout the input sources to the output sinks
    val portsAOI = transpose((in  zip requestAIO) map { case (i, r) => TLXbar.fanout(i.a, r, edgesOut.map(_.params(ForceFanoutKey).a)) })
    val portsBIO = transpose((out zip requestBOI) map { case (o, r) => TLXbar.fanout(o.b, r, edgesIn .map(_.params(ForceFanoutKey).b)) })
    val portsCOI = transpose((in  zip requestCIO) map { case (i, r) => TLXbar.fanout(i.c, r, edgesOut.map(_.params(ForceFanoutKey).c)) })
    val portsDIO = transpose((out zip requestDOI) map { case (o, r) => TLXbar.fanout(o.d, r, edgesIn .map(_.params(ForceFanoutKey).d)) })
    val portsEOI = transpose((in  zip requestEIO) map { case (i, r) => TLXbar.fanout(i.e, r, edgesOut.map(_.params(ForceFanoutKey).e)) })

    // Arbitrate amongst the sources
    for (o <- 0 until out.size) {
      val allowI = Seq.tabulate(in.size) { i =>
        edgesIn(i).client.anySupportProbe &&
        edgesOut(o).manager.anySupportAcquireB
      }
      TLArbiter(policy)(out(o).a,       (beatsAI zip portsAOI(o)        ):_*)
      TLArbiter(policy)(out(o).c, filter(beatsCI zip portsCOI(o), allowI):_*)
      TLArbiter(policy)(out(o).e, filter(beatsEI zip portsEOI(o), allowI):_*)
    }

    for (i <- 0 until in.size) {
      val allowO = Seq.tabulate(out.size) { o =>
        edgesIn(i).client.anySupportProbe &&
        edgesOut(o).manager.anySupportAcquireB
      }
      TLArbiter(policy)(in(i).b, filter(beatsBO zip portsBIO(i), allowO):_*)
      TLArbiter(policy)(in(i).d,       (beatsDO zip portsDIO(i)        ):_*)
    }
  }
}

object TLXbar
{
  def apply(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters): TLNode =
  {
    val xbar = LazyModule(new TLXbar(policy))
    xbar.node
  }

  def mapInputIds (ports: Seq[TLClientPortParameters ]) = assignRanges(ports.map(_.endSourceId))
  def mapOutputIds(ports: Seq[TLManagerPortParameters]) = assignRanges(ports.map(_.endSinkId))

  def assignRanges(sizes: Seq[Int]) = {
    val pow2Sizes = sizes.map { z => if (z == 0) 0 else 1 << log2Ceil(z) }
    val tuples = pow2Sizes.zipWithIndex.sortBy(_._1) // record old index, then sort by increasing size
    val starts = tuples.scanRight(0)(_._1 + _).tail // suffix-sum of the sizes = the start positions
    val ranges = (tuples zip starts) map { case ((sz, i), st) =>
      (if (sz == 0) IdRange(0,0) else IdRange(st, st+sz), i)
    }
    ranges.sortBy(_._2).map(_._1) // Restore orignal order
  }

  def relabeler() = {
    var idFactory = 0
    () => {
      val fifoMap = scala.collection.mutable.HashMap.empty[Int, Int]
      (x: Int) => {
        if (fifoMap.contains(x)) fifoMap(x) else {
          val out = idFactory
          idFactory = idFactory + 1
          fifoMap += (x -> out)
          out
        }
      }
    }
  }

  // Replicate an input port to each output port
  def fanout[T <: TLChannel](input: DecoupledIO[T], select: Seq[Bool], force: Seq[Boolean] = Nil) = {
    val filtered = Wire(Vec(select.size, input))
    for (i <- 0 until select.size) {
      filtered(i).bits := (if (force.lift(i).getOrElse(false)) IdentityModule(input.bits) else input.bits)
      filtered(i).valid := input.valid && select(i)
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

class TLRAMXbar(nManagers: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("Xbar"))
  val xbar = LazyModule(new TLXbar)

  xbar.node := TLDelayer(0.1) := model.node := fuzz.node
  (0 until nManagers) foreach { n =>
    val ram  = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    ram.node := TLFragmenter(4, 256) := TLDelayer(0.1) := xbar.node
  }

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMXbarTest(nManagers: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMXbar(nManagers,txns)).module)
  io.finished := dut.io.finished
}

class TLMulticlientXbar(nManagers: Int, nClients: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val xbar = LazyModule(new TLXbar)

  val fuzzers = (0 until nClients) map { n =>
    val fuzz = LazyModule(new TLFuzzer(txns))
    xbar.node := TLDelayer(0.1) := fuzz.node
    fuzz
  }

  (0 until nManagers) foreach { n =>
    val ram = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    ram.node := TLFragmenter(4, 256) := TLDelayer(0.1) := xbar.node
  }

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzzers.last.module.io.finished
  }
}

class TLMulticlientXbarTest(nManagers: Int, nClients: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLMulticlientXbar(nManagers, nClients, txns)).module)
  io.finished := dut.io.finished
}
