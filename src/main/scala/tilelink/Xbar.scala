// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class TLXbar(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) extends LazyModule
{
  val node = TLNexusNode(
    numClientPorts  = 1 to 999,
    numManagerPorts = 1 to 999,
    clientFn  = { seq =>
      require (!seq.exists(_.unsafeAtomics) || seq.size == 1,
        "An unsafe atomic port can not be combined with any other!")
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
      val outputIdRanges = TLXbar.mapOutputIds(seq)
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        endSinkId = outputIdRanges.map(_.map(_.end).getOrElse(0)).max,
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
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    if ((io.in.size * io.out.size) > (8*32)) {
      println (s"!!! WARNING !!!")
      println (s" Your TLXbar ($name) is very large, with ${io.in.size} Masters and ${io.out.size} Slaves.")
      println (s"!!! WARNING !!!")
    }
    // Grab the port ID mapping
    val inputIdRanges = TLXbar.mapInputIds(node.edgesIn.map(_.client))
    val outputIdRanges = TLXbar.mapOutputIds(node.edgesOut.map(_.manager))

    // Find a good mask for address decoding
    val port_addrs = node.edgesOut.map(_.manager.managers.map(_.address).flatten)
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
      (node.edgesIn zip inputIdRanges).zipWithIndex.foreach { case ((edge, id), i) =>
        println(s"\t$i assigned ${id} for ${edge.client.clients.map(_.name).mkString(", ")}")
      }
      println("")
    }

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = TLBundleParameters.union(io.in.map(_.params) ++ io.out.map(_.params))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int) = if (size <= 1) UInt(0) else id(log2Ceil(size)-1, 0)

    // Transform input bundle sources (sinks use global namespace on both sides)
    val in = Wire(Vec(io.in.size, TLBundle(wide_bundle)))
    for (i <- 0 until in.size) {
      val r = inputIdRanges(i)

      in(i).a <> io.in(i).a
      io.in(i).d <> in(i).d
      in(i).a.bits.source := io.in(i).a.bits.source | UInt(r.start)
      io.in(i).d.bits.source := trim(in(i).d.bits.source, r.size)

      if (node.edgesIn(i).client.anySupportProbe && node.edgesOut.exists(_.manager.anySupportAcquireB)) {
        in(i).c <> io.in(i).c
        in(i).e <> io.in(i).e
        io.in(i).b <> in(i).b
        in(i).c.bits.source := io.in(i).c.bits.source | UInt(r.start)
        io.in(i).b.bits.source := trim(in(i).b.bits.source, r.size)
      } else {
        in(i).c.valid := Bool(false)
        in(i).e.valid := Bool(false)
        in(i).b.ready := Bool(false)
        io.in(i).c.ready := Bool(true)
        io.in(i).e.ready := Bool(true)
        io.in(i).b.valid := Bool(false)
      }
    }

    // Transform output bundle sinks (sources use global namespace on both sides)
    val out = Wire(Vec(io.out.size, TLBundle(wide_bundle)))
    for (i <- 0 until out.size) {
      val r = outputIdRanges(i)

      io.out(i).a <> out(i).a
      out(i).d <> io.out(i).d
      out(i).d.bits.sink := io.out(i).d.bits.sink | UInt(r.map(_.start).getOrElse(0))

      if (node.edgesOut(i).manager.anySupportAcquireB && node.edgesIn.exists(_.client.anySupportProbe)) {
        io.out(i).c <> out(i).c
        io.out(i).e <> out(i).e
        out(i).b <> io.out(i).b
        io.out(i).e.bits.sink := trim(out(i).e.bits.sink, r.map(_.size).getOrElse(0))
      } else {
        out(i).c.ready := Bool(false)
        out(i).e.ready := Bool(false)
        out(i).b.valid := Bool(false)
        io.out(i).c.valid := Bool(false)
        io.out(i).e.valid := Bool(false)
        io.out(i).b.ready := Bool(true)
      }
    }

    val addressA = (in zip node.edgesIn) map { case (i, e) => e.address(i.a.bits) }
    val addressC = (in zip node.edgesIn) map { case (i, e) => e.address(i.c.bits) }

    val requestAIO = Vec(addressA.map { i => Vec(outputPorts.map { o => o(i) }) })
    val requestCIO = Vec(addressC.map { i => Vec(outputPorts.map { o => o(i) }) })
    val requestBOI = Vec(out.map { o => Vec(inputIdRanges.map  { i => i.contains(o.b.bits.source) }) })
    val requestDOI = Vec(out.map { o => Vec(inputIdRanges.map  { i => i.contains(o.d.bits.source) }) })
    val requestEIO = Vec(in.map  { i => Vec(outputIdRanges.map { o => o.map(_.contains(i.e.bits.sink)).getOrElse(Bool(false)) }) })

    val beatsAI = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats1(i.a.bits) })
    val beatsBO = Vec((out zip node.edgesOut) map { case (o, e) => e.numBeats1(o.b.bits) })
    val beatsCI = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats1(i.c.bits) })
    val beatsDO = Vec((out zip node.edgesOut) map { case (o, e) => e.numBeats1(o.d.bits) })
    val beatsEI = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats1(i.e.bits) })

    // Which pairs support support transfers
    def transpose[T](x: Seq[Seq[T]]) = Seq.tabulate(x(0).size) { i => Seq.tabulate(x.size) { j => x(j)(i) } }
    def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data zip mask).filter(_._2).map(_._1)

    // Fanout the input sources to the output sinks
    val portsAOI = transpose((in  zip requestAIO) map { case (i, r) => TLXbar.fanout(i.a, r) })
    val portsBIO = transpose((out zip requestBOI) map { case (o, r) => TLXbar.fanout(o.b, r) })
    val portsCOI = transpose((in  zip requestCIO) map { case (i, r) => TLXbar.fanout(i.c, r) })
    val portsDIO = transpose((out zip requestDOI) map { case (o, r) => TLXbar.fanout(o.d, r) })
    val portsEOI = transpose((in  zip requestEIO) map { case (i, r) => TLXbar.fanout(i.e, r) })

    // Arbitrate amongst the sources
    for (o <- 0 until out.size) {
      val allowI = Seq.tabulate(in.size) { i =>
        node.edgesIn(i).client.anySupportProbe &&
        node.edgesOut(o).manager.anySupportAcquireB
      }
      TLArbiter(policy)(out(o).a,       (beatsAI zip portsAOI(o)        ):_*)
      TLArbiter(policy)(out(o).c, filter(beatsCI zip portsCOI(o), allowI):_*)
      TLArbiter(policy)(out(o).e, filter(beatsEI zip portsEOI(o), allowI):_*)
    }

    for (i <- 0 until in.size) {
      val allowO = Seq.tabulate(out.size) { o =>
        node.edgesIn(i).client.anySupportProbe &&
        node.edgesOut(o).manager.anySupportAcquireB
      }
      TLArbiter(policy)(in(i).b, filter(beatsBO zip portsBIO(i), allowO):_*)
      TLArbiter(policy)(in(i).d,       (beatsDO zip portsDIO(i)        ):_*)
    }
  }
}

object TLXbar
{
  def mapInputIds (ports: Seq[TLClientPortParameters ]) = assignRanges(ports.map(_.endSourceId)).map(_.get)
  def mapOutputIds(ports: Seq[TLManagerPortParameters]) = assignRanges(ports.map(_.endSinkId))

  def assignRanges(sizes: Seq[Int]) = {
    val pow2Sizes = sizes.map { z => if (z == 0) 0 else 1 << log2Ceil(z) }
    val tuples = pow2Sizes.zipWithIndex.sortBy(_._1) // record old index, then sort by increasing size
    val starts = tuples.scanRight(0)(_._1 + _).tail // suffix-sum of the sizes = the start positions
    val ranges = (tuples zip starts) map { case ((sz, i), st) =>
      (if (sz == 0) None else Some(IdRange(st, st+sz)), i)
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
  def fanout[T <: TLChannel](input: DecoupledIO[T], select: Seq[Bool]) = {
    val filtered = Wire(Vec(select.size, input))
    for (i <- 0 until select.size) {
      filtered(i).bits := IdentityModule(input.bits) // force fanout of wires
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

  model.node := fuzz.node
  xbar.node := TLDelayer(0.1)(model.node)
  (0 until nManagers) foreach { n =>
    val ram  = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    ram.node := TLFragmenter(4, 256)(TLDelayer(0.1)(xbar.node))
  }

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMXbarTest(nManagers: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  io.finished := Module(LazyModule(new TLRAMXbar(nManagers,txns)).module).io.finished
}

class TLMulticlientXbar(nManagers: Int, nClients: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val xbar = LazyModule(new TLXbar)

  val fuzzers = (0 until nClients) map { n =>
    val fuzz = LazyModule(new TLFuzzer(txns))
    xbar.node := TLDelayer(0.1)(fuzz.node)
    fuzz
  }

  (0 until nManagers) foreach { n =>
    val ram  = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    ram.node := TLFragmenter(4, 256)(TLDelayer(0.1)(xbar.node))
  }

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzzers.last.module.io.finished
  }
}

class TLMulticlientXbarTest(nManagers: Int, nClients: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  io.finished := Module(LazyModule(new TLMulticlientXbar(nManagers, nClients, txns)).module).io.finished
}
