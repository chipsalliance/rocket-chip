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

    // Not every master need connect to every slave on every channel; determine which connections are necessary
    val reachableIO = edgesIn.map { cp => edgesOut.map { mp =>
      cp.client.clients.exists { c => mp.manager.managers.exists { m =>
        c.visibility.exists { ca => m.address.exists { ma =>
          ca.overlaps(ma)}}}}
      }.toVector}.toVector
    val probeIO = (edgesIn zip reachableIO).map { case (cp, reachableO) =>
      (edgesOut zip reachableO).map { case (mp, reachable) =>
        reachable && cp.client.anySupportProbe && mp.manager.managers.exists(_.regionType >= RegionType.TRACKED)
      }.toVector}.toVector
    val releaseIO = (edgesIn zip reachableIO).map { case (cp, reachableO) =>
      (edgesOut zip reachableO).map { case (mp, reachable) =>
        reachable && cp.client.anySupportProbe && mp.manager.anySupportAcquireB
      }.toVector}.toVector

    val connectAIO = reachableIO
    val connectBIO = probeIO
    val connectCIO = releaseIO
    val connectDIO = reachableIO
    val connectEIO = releaseIO

    def transpose[T](x: Seq[Seq[T]]) = if (x.isEmpty) Nil else Vector.tabulate(x(0).size) { i => Vector.tabulate(x.size) { j => x(j)(i) } }
    val connectAOI = transpose(connectAIO)
    val connectBOI = transpose(connectBIO)
    val connectCOI = transpose(connectCIO)
    val connectDOI = transpose(connectDIO)
    val connectEOI = transpose(connectEIO)

    // Grab the port ID mapping
    val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
    val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = TLBundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int) = if (size <= 1) UInt(0) else id(log2Ceil(size)-1, 0)

    // Transform input bundle sources (sinks use global namespace on both sides)
    val in = Wire(Vec(io_in.size, TLBundle(wide_bundle)))
    for (i <- 0 until in.size) {
      val r = inputIdRanges(i)

      if (connectAIO(i).exists(x=>x)) {
        in(i).a <> io_in(i).a
        in(i).a.bits.source := io_in(i).a.bits.source | UInt(r.start)
      } else {
        in(i).a.valid := Bool(false)
        io_in(i).a.ready := Bool(true)
      }

      if (connectBIO(i).exists(x=>x)) {
        io_in(i).b <> in(i).b
        io_in(i).b.bits.source := trim(in(i).b.bits.source, r.size)
      } else {
        in(i).b.ready := Bool(true)
        io_in(i).b.valid := Bool(false)
      }

      if (connectCIO(i).exists(x=>x)) {
        in(i).c <> io_in(i).c
        in(i).c.bits.source := io_in(i).c.bits.source | UInt(r.start)
      } else {
        in(i).c.valid := Bool(false)
        io_in(i).c.ready := Bool(true)
      }

      if (connectDIO(i).exists(x=>x)) {
        io_in(i).d <> in(i).d
        io_in(i).d.bits.source := trim(in(i).d.bits.source, r.size)
      } else {
        in(i).d.ready := Bool(true)
        io_in(i).d.valid := Bool(false)
      }

      if (connectEIO(i).exists(x=>x)) {
        in(i).e <> io_in(i).e
      } else {
        in(i).e.valid := Bool(false)
        io_in(i).e.ready := Bool(true)
      }
    }

    // Transform output bundle sinks (sources use global namespace on both sides)
    val out = Wire(Vec(io_out.size, TLBundle(wide_bundle)))
    for (o <- 0 until out.size) {
      val r = outputIdRanges(o)

      if (connectAOI(o).exists(x=>x)) {
        io_out(o).a <> out(o).a
      } else {
        out(o).a.ready := Bool(true)
        io_out(o).a.valid := Bool(false)
      }

      if (connectBOI(o).exists(x=>x)) {
        out(o).b <> io_out(o).b
      } else {
        out(o).b.valid := Bool(false)
        io_out(o).b.ready := Bool(true)
      }

      if (connectCOI(o).exists(x=>x)) {
        io_out(o).c <> out(o).c
      } else {
        out(o).c.ready := Bool(true)
        io_out(o).c.valid := Bool(false)
      }

      if (connectDOI(o).exists(x=>x)) {
        out(o).d <> io_out(o).d
        out(o).d.bits.sink := io_out(o).d.bits.sink | UInt(r.start)
      } else {
        out(o).d.valid := Bool(false)
        io_out(o).d.ready := Bool(true)
      }

      if (connectEOI(o).exists(x=>x)) {
        io_out(o).e <> out(o).e
        io_out(o).e.bits.sink := trim(out(o).e.bits.sink, r.size)
      } else {
        out(o).e.ready := Bool(true)
        io_out(o).e.valid := Bool(false)
      }
    }

    // Filter a list to only those elements selected
    def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data zip mask).filter(_._2).map(_._1)

    // Based on input=>output connectivity, create per-input minimal address decode circuits
    val requiredAC = (connectAIO ++ connectCIO).distinct
    val outputPortFns: Map[Vector[Boolean], Seq[UInt => Bool]] = requiredAC.map { connectO =>
      val port_addrs = edgesOut.map(_.manager.managers.flatMap(_.address))
      val routingMask = AddressDecoder(filter(port_addrs, connectO))
      val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))

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

      (connectO, route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _)))
    }.toMap

    // Print the ID mapping
    if (false) {
      println(s"XBar ${name} mapping:")
      (edgesIn zip inputIdRanges).zipWithIndex.foreach { case ((edge, id), i) =>
        println(s"\t$i assigned ${id} for ${edge.client.clients.map(_.name).mkString(", ")}")
      }
      println("")
    }

    val addressA = (in zip edgesIn) map { case (i, e) => e.address(i.a.bits) }
    val addressC = (in zip edgesIn) map { case (i, e) => e.address(i.c.bits) }

    def unique(x: Vector[Boolean]) = Bool(x.filter(x=>x).size <= 1)
    val requestAIO = (connectAIO zip addressA) map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
    val requestCIO = (connectCIO zip addressC) map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
    val requestBOI = out.map { o => inputIdRanges.map  { i => i.contains(o.b.bits.source) } }
    val requestDOI = out.map { o => inputIdRanges.map  { i => i.contains(o.d.bits.source) } }
    val requestEIO = in.map  { i => outputIdRanges.map { o => o.contains(i.e.bits.sink) } }

    val beatsAI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.a.bits) }
    val beatsBO = (out zip edgesOut) map { case (o, e) => e.numBeats1(o.b.bits) }
    val beatsCI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.c.bits) }
    val beatsDO = (out zip edgesOut) map { case (o, e) => e.numBeats1(o.d.bits) }
    val beatsEI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.e.bits) }

    // Fanout the input sources to the output sinks
    val portsAOI = transpose((in  zip requestAIO) map { case (i, r) => TLXbar.fanout(i.a, r, edgesOut.map(_.params(ForceFanoutKey).a)) })
    val portsBIO = transpose((out zip requestBOI) map { case (o, r) => TLXbar.fanout(o.b, r, edgesIn .map(_.params(ForceFanoutKey).b)) })
    val portsCOI = transpose((in  zip requestCIO) map { case (i, r) => TLXbar.fanout(i.c, r, edgesOut.map(_.params(ForceFanoutKey).c)) })
    val portsDIO = transpose((out zip requestDOI) map { case (o, r) => TLXbar.fanout(o.d, r, edgesIn .map(_.params(ForceFanoutKey).d)) })
    val portsEOI = transpose((in  zip requestEIO) map { case (i, r) => TLXbar.fanout(i.e, r, edgesOut.map(_.params(ForceFanoutKey).e)) })

    // Arbitrate amongst the sources
    for (o <- 0 until out.size) {
      TLArbiter(policy)(out(o).a, filter(beatsAI zip portsAOI(o), connectAOI(o)):_*)
      TLArbiter(policy)(out(o).c, filter(beatsCI zip portsCOI(o), connectCOI(o)):_*)
      TLArbiter(policy)(out(o).e, filter(beatsEI zip portsEOI(o), connectEOI(o)):_*)
    }

    for (i <- 0 until in.size) {
      TLArbiter(policy)(in(i).b, filter(beatsBO zip portsBIO(i), connectBIO(i)):_*)
      TLArbiter(policy)(in(i).d, filter(beatsDO zip portsDIO(i), connectDIO(i)):_*)
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
  def fanout[T <: TLChannel](input: DecoupledIO[T], select: Seq[Bool], force: Seq[Boolean] = Nil): Seq[DecoupledIO[T]] = {
    val filtered = Wire(Vec(select.size, input))
    for (i <- 0 until select.size) {
      filtered(i).bits := (if (force.lift(i).getOrElse(false)) IdentityModule(input.bits) else input.bits)
      filtered(i).valid := input.valid && (select(i) || Bool(select.size == 1))
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
