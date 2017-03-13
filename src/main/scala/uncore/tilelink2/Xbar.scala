// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import config._
import diplomacy._

class TLXbar(policy: TLArbiter.Policy = TLArbiter.lowestIndexFirst)(implicit p: Parameters) extends LazyModule
{
  def mapInputIds (ports: Seq[TLClientPortParameters ]) = assignRanges(ports.map(_.endSourceId))
  def mapOutputIds(ports: Seq[TLManagerPortParameters]) = assignRanges(ports.map(_.endSinkId))

  def assignRanges(sizes: Seq[Int]) = {
    val pow2Sizes = sizes.map(1 << log2Ceil(_))
    val tuples = pow2Sizes.zipWithIndex.sortBy(_._1) // record old index, then sort by increasing size
    val starts = tuples.scanRight(0)(_._1 + _).tail // suffix-sum of the sizes = the start positions
    val ranges = (tuples zip starts) map { case ((sz, i), st) => (IdRange(st, st+sz), i) }
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

  val node = TLNexusNode(
    numClientPorts  = 1 to 32,
    numManagerPorts = 1 to 32,
    clientFn  = { seq =>
      // An unsafe atomic port can not be combined with any other!
      require (!seq.exists(_.unsafeAtomics) || seq.size == 1)
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        clients = (mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client => client.copy(
            sourceId = client.sourceId.shift(range.start)
          )}
        }
      )
    },
    managerFn = { seq =>
      val fifoIdFactory = relabeler()
      val outputIdRanges = mapOutputIds(seq)
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        endSinkId = outputIdRanges.map(_.end).max,
        managers = ManagerUnification(seq.flatMap { port =>
          // println(s"${port.managers.map(_.name)} ${port.beatBytes} vs ${seq(0).managers.map(_.name)} ${seq(0).beatBytes}")
          require (port.beatBytes == seq(0).beatBytes)
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager => manager.copy(
            fifoId = manager.fifoId.map(fifoIdMapper(_))
          )}
        })
      )
    })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    // Grab the port ID mapping
    val inputIdRanges = mapInputIds(node.edgesIn.map(_.client))
    val outputIdRanges = mapOutputIds(node.edgesOut.map(_.manager))

    // Find a good mask for address decoding
    val port_addrs = node.edgesOut.map(_.manager.managers.map(_.address).flatten)
    val routingMask = AddressDecoder(port_addrs)
    val route_addrs = port_addrs.map(_.map(_.widen(~routingMask)).distinct)
    val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))

    // Print the mapping
    if (false) {
      println("Xbar mapping:")
      route_addrs.foreach { p =>
        print(" ")
        p.foreach { a => print(s" ${a}") }
        println("")
      }
      println("--")
    }

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = io.in(0).params.union(io.out(0).params)

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int) = if (size <= 1) UInt(0) else id(log2Ceil(size)-1, 0)

    // Transform input bundle sources (sinks use global namespace on both sides)
    val in = Wire(Vec(io.in.size, TLBundle(wide_bundle)))
    for (i <- 0 until in.size) {
      val r = inputIdRanges(i)
      in(i) <> io.in(i)
      // prefix sources
      in(i).a.bits.source := io.in(i).a.bits.source | UInt(r.start)
      in(i).c.bits.source := io.in(i).c.bits.source | UInt(r.start)
      // defix sources
      io.in(i).b.bits.source := trim(in(i).b.bits.source, r.size)
      io.in(i).d.bits.source := trim(in(i).d.bits.source, r.size)
    }

    // Transform output bundle sinks (sources use global namespace on both sides)
    val out = Wire(Vec(io.out.size, TLBundle(wide_bundle)))
    for (i <- 0 until out.size) {
      val r = outputIdRanges(i)
      io.out(i) <> out(i)
      // prefix sinks
      out(i).d.bits.sink := io.out(i).d.bits.sink | UInt(r.start)
      // defix sinks
      io.out(i).e.bits.sink := trim(out(i).e.bits.sink, r.size)
    }

    val addressA = (in zip node.edgesIn) map { case (i, e) => e.address(i.a.bits) }
    val addressC = (in zip node.edgesIn) map { case (i, e) => e.address(i.c.bits) }

    val requestAIO = Vec(addressA.map { i => Vec(outputPorts.map { o => o(i) }) })
    val requestCIO = Vec(addressC.map { i => Vec(outputPorts.map { o => o(i) }) })
    val requestBOI = Vec(out.map { o => Vec(inputIdRanges.map  { i => i.contains(o.b.bits.source) }) })
    val requestDOI = Vec(out.map { o => Vec(inputIdRanges.map  { i => i.contains(o.d.bits.source) }) })
    val requestEIO = Vec(in.map  { i => Vec(outputIdRanges.map { o => o.contains(i.e.bits.sink)   }) })

    val beatsAI = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats1(i.a.bits) })
    val beatsBO = Vec((out zip node.edgesOut) map { case (o, e) => e.numBeats1(o.b.bits) })
    val beatsCI = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats1(i.c.bits) })
    val beatsDO = Vec((out zip node.edgesOut) map { case (o, e) => e.numBeats1(o.d.bits) })
    val beatsEI = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats1(i.e.bits) })

    // Which pairs support support transfers
    def transpose[T](x: Seq[Seq[T]]) = Seq.tabulate(x(0).size) { i => Seq.tabulate(x.size) { j => x(j)(i) } }
    def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data zip mask).filter(_._2).map(_._1)

    // Replicate an input port to each output port
    def fanout[T <: TLChannel](input: DecoupledIO[T], select: Seq[Bool]) = {
      val filtered = Wire(Vec(select.size, input))
      for (i <- 0 until select.size) {
        filtered(i).bits := input.bits
        filtered(i).valid := input.valid && select(i)
      }
      input.ready := Mux1H(select, filtered.map(_.ready))
      filtered
    }

    // Fanout the input sources to the output sinks
    val portsAOI = transpose((in  zip requestAIO) map { case (i, r) => fanout(i.a, r) })
    val portsBIO = transpose((out zip requestBOI) map { case (o, r) => fanout(o.b, r) })
    val portsCOI = transpose((in  zip requestCIO) map { case (i, r) => fanout(i.c, r) })
    val portsDIO = transpose((out zip requestDOI) map { case (o, r) => fanout(o.d, r) })
    val portsEOI = transpose((in  zip requestEIO) map { case (i, r) => fanout(i.e, r) })

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

/** Synthesizeable unit tests */
import unittest._

class TLRAMXbar(nManagers: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel)
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

class TLRAMXbarTest(nManagers: Int)(implicit p: Parameters) extends UnitTest(timeout = 500000) {
  io.finished := Module(LazyModule(new TLRAMXbar(nManagers)).module).io.finished
}

class TLMulticlientXbar(nManagers: Int, nClients: Int)(implicit p: Parameters) extends LazyModule {
  val xbar = LazyModule(new TLXbar)

  val fuzzers = (0 until nClients) map { n =>
    val fuzz = LazyModule(new TLFuzzer(5000))
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

class TLMulticlientXbarTest(nManagers: Int, nClients: Int)(implicit p: Parameters) extends UnitTest(timeout = 5000000) {
  io.finished := Module(LazyModule(new TLMulticlientXbar(nManagers, nClients)).module).io.finished
}
