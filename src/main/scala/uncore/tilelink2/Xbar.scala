// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

object TLXbar
{
  def lowestIndex(requests: Vec[Bool], execute: Bool) = {
    // lowest-index first is stateless; ignore execute
    val ors = Vec(requests.scanLeft(Bool(false))(_ || _).init) // prefix-OR
    Vec((ors zip requests) map { case (o, r) => !o && r })
  }
}

class TLXbar(policy: (Vec[Bool], Bool) => Seq[Bool] = TLXbar.lowestIndex) extends LazyModule
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

  val node = TLAdapterNode(
    numClientPorts  = 1 to 32,
    numManagerPorts = 1 to 32,
    clientFn  = { seq =>
      val clients = (mapInputIds(seq) zip seq) flatMap { case (range, port) =>
        port.clients map { client => client.copy(
          sourceId = client.sourceId.shift(range.start)
        )}
      }
      TLClientPortParameters(clients)
    },
    managerFn = { seq =>
      val fifoIdFactory = relabeler()
      val managers = (mapOutputIds(seq) zip seq) flatMap { case (range, port) =>
        require (port.beatBytes == seq(0).beatBytes)
        val fifoIdMapper = fifoIdFactory()
        port.managers map { manager => manager.copy(
          sinkId = manager.sinkId.shift(range.start),
          fifoId = manager.fifoId.map(fifoIdMapper(_))
        )}
      }
      TLManagerPortParameters(managers, seq(0).beatBytes)
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
    def transpose(x: Seq[Seq[Bool]]) = Vec.tabulate(x(0).size) { i => Vec.tabulate(x.size) { j => x(j)(i) } }

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

    // The crossbar cross-connection state; defined later
    val grantedAIO = Wire(Vec(in .size, Vec(out.size, Bool())))
    val grantedBOI = Wire(Vec(out.size, Vec(in .size, Bool())))
    val grantedCIO = Wire(Vec(in .size, Vec(out.size, Bool())))
    val grantedDOI = Wire(Vec(out.size, Vec(in .size, Bool())))
    val grantedEIO = Wire(Vec(in .size, Vec(out.size, Bool())))

    val grantedAOI = transpose(grantedAIO)
    val grantedBIO = transpose(grantedBOI)
    val grantedCOI = transpose(grantedCIO)
    val grantedDIO = transpose(grantedDOI)
    val grantedEOI = transpose(grantedEIO)

    // Mux1H passes a single-source through unmasked. That's bad for control.
    def Mux1C(sel: Seq[Bool], ctl: Seq[Bool]) = (sel zip ctl).map{ case (a,b) => a && b }.reduce(_ || _)

    // Mux clients to managers
    for (o <- 0 until out.size) {
      out(o).a.valid := Mux1C(grantedAOI(o), in.map(_.a.valid))
      out(o).a.bits  := Mux1H(grantedAOI(o), in.map(_.a.bits))
      out(o).b.ready := Mux1C(grantedBOI(o), in.map(_.b.ready))
      out(o).c.valid := Mux1C(grantedCOI(o), in.map(_.c.valid))
      out(o).c.bits  := Mux1H(grantedCOI(o), in.map(_.c.bits))
      out(o).d.ready := Mux1C(grantedDOI(o), in.map(_.d.ready))
      out(o).e.valid := Mux1C(grantedEOI(o), in.map(_.e.valid))
      out(o).e.bits  := Mux1H(grantedEOI(o), in.map(_.e.bits))
    }

    // Mux managers to clients
    for (i <- 0 until in.size) {
      in(i).a.ready := Mux1C(grantedAIO(i), out.map(_.a.ready))
      in(i).b.valid := Mux1C(grantedBIO(i), out.map(_.b.valid))
      in(i).b.bits  := Mux1H(grantedBIO(i), out.map(_.b.bits))
      in(i).c.ready := Mux1C(grantedCIO(i), out.map(_.c.ready))
      in(i).d.valid := Mux1C(grantedDIO(i), out.map(_.d.valid))
      in(i).d.bits  := Mux1H(grantedDIO(i), out.map(_.d.bits))
      in(i).e.ready := Mux1C(grantedEIO(i), out.map(_.e.ready))
    }

    val addressA = (in zip node.edgesIn) map { case (i, e) => (i.a.valid, e.address(i.a.bits)) }
    val addressC = (in zip node.edgesIn) map { case (i, e) => (i.c.valid, e.address(i.c.bits)) }

    val requestAIO = Vec(addressA.map { i => Vec(outputPorts.map { o => i._1 && o(i._2) }) })
    val requestCIO = Vec(addressC.map { i => Vec(outputPorts.map { o => i._1 && o(i._2) }) })

    val requestBOI = Vec(out.map { o => Vec(inputIdRanges.map  { i => o.b.valid && i.contains(o.b.bits.source) }) })
    val requestDOI = Vec(out.map { o => Vec(inputIdRanges.map  { i => o.d.valid && i.contains(o.d.bits.source) }) })
    val requestEIO = Vec(in.map  { i => Vec(outputIdRanges.map { o => i.e.valid && o.contains(i.e.bits.sink)   }) })

    val beatsA = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats(i.a.bits) })
    val beatsB = Vec((out zip node.edgesOut) map { case (o, e) => e.numBeats(o.b.bits) })
    val beatsC = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats(i.c.bits) })
    val beatsD = Vec((out zip node.edgesOut) map { case (o, e) => e.numBeats(o.d.bits) })
    val beatsE = Vec((in  zip node.edgesIn)  map { case (i, e) => e.numBeats(i.e.bits) })

    // Which pairs support support transfers
    val maskIO = Vec.tabulate(in.size) { i => Vec.tabulate(out.size) { o => 
      Bool(node.edgesIn(i).client.anySupportProbe && node.edgesOut(o).manager.anySupportAcquire)
    } }
    val maskOI = transpose(maskIO)

    // Mask out BCE channel connections (to be optimized away) for transfer-incapable pairings
    def mask(a: Seq[Seq[Bool]], b: Seq[Seq[Bool]]) = 
      Vec((a zip b) map { case (x, y) => Vec((x zip y) map { case (a, b) => a && b }) })

    grantedAIO :=      arbitrate(     requestAIO,          beatsA, out.map(_.a.fire()))
    grantedBOI := mask(arbitrate(mask(requestBOI, maskOI), beatsB, in .map(_.b.fire())), maskOI)
    grantedCIO := mask(arbitrate(mask(requestCIO, maskIO), beatsC, out.map(_.c.fire())), maskIO)
    grantedDOI :=      arbitrate(     requestDOI,          beatsD, in .map(_.d.fire()))
    grantedEIO := mask(arbitrate(mask(requestEIO, maskIO), beatsE, out.map(_.e.fire())), maskIO)

    def arbitrate(request: Seq[Seq[Bool]], beats: Seq[UInt], progress: Seq[Bool]) = {
      request foreach { row => require (row.size == progress.size) } // consistent # of resources
      request foreach { resources => // only one resource is requested
        val prefixOR = resources.scanLeft(Bool(false))(_ || _).init
        assert (!(prefixOR zip resources).map{case (a, b) => a && b}.reduce(_ || _))
      }
      transpose((transpose(request) zip progress).map { case (r,p) => arbitrate1(r, beats, p) })
    }

    def arbitrate1(requests: Vec[Bool], beats: Seq[UInt], progress: Bool) = {
      require (requests.size == beats.size) // consistent # of requesters

      val beatsLeft = RegInit(UInt(0))
      val idle = beatsLeft === UInt(0)

      // Apply policy to select which requester wins
      val winners = Vec(policy(requests, idle))

      // Winners must be a subset of requests
      assert ((winners zip requests).map { case (w,r) => !w || r } .reduce(_ && _))
      // There must be only one winner
      val prefixOR = winners.scanLeft(Bool(false))(_ || _).init
      assert ((prefixOR zip winners).map { case (p,w) => !p || !w }.reduce(_ && _))

      // Supposing we take the winner as input, how many beats must be sent?
      val maskedBeats = (winners zip beats).map { case (w,b) => Mux(w, b, UInt(0)) }
      val initBeats = maskedBeats.reduceLeft(_ | _) // no winner => 0 beats
      // What is the counter state before progress?
      val todoBeats = Mux(idle, initBeats, beatsLeft)
      // Apply progress and register the result
      beatsLeft := todoBeats - progress.asUInt
      assert (!progress || todoBeats =/= UInt(0)) // underflow should be impossible

      // The previous arbitration state of the resource
      val state = RegInit(Vec.fill(requests.size)(Bool(false)))
      // Only take a new value while idle
      val muxState = Mux(idle, winners, state)
      state := muxState

      muxState
    }
  }
}
