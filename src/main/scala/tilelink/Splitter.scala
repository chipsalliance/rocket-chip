// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

case class SplitterArg[T](newSize: Int, ports: Seq[T])
case class TLSplitterNode(
  clientFn:        SplitterArg[TLClientPortParameters]  => Seq[TLClientPortParameters],
  managerFn:       SplitterArg[TLManagerPortParameters] => Seq[TLManagerPortParameters])(
  implicit valName: ValName)
  extends TLCustomNode
{
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (oKnown == 0, s"${name} (a splitter) appears right of a := or :*=; use a :=* instead${lazyModule.line}")
    require (iStars == 0, s"${name} (a splitter) cannot appear left of a :*=; did you mean :=*?${lazyModule.line}")
    (0, iKnown)
  }
  def mapParamsD(n: Int, p: Seq[TLClientPortParameters]): Seq[TLClientPortParameters] = {
    require (p.size == 0 || n % p.size == 0, s"Diplomacy bug; splitter inputs do not divide outputs")
    val out = clientFn(SplitterArg(n, p))
    require (out.size == n, s"${name} created the wrong number of outputs from inputs${lazyModule.line}")
    out
  }
  def mapParamsU(n: Int, p: Seq[TLManagerPortParameters]): Seq[TLManagerPortParameters] = {
    require (n == 0 || p.size % n == 0, s"Diplomacy bug; splitter outputs indivisable by inputs")
    val out = managerFn(SplitterArg(n, p))
    require (out.size == n, s"${name} created the wrong number of inputs from outputs${lazyModule.line}")
    out
  }
}

class TLSplitter(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) extends LazyModule
{
  val node = TLSplitterNode(
    clientFn  = { case SplitterArg(newSize, ports) =>
      if (newSize == 0) Nil else
      Seq.fill(newSize / ports.size) { ports }.flatten
    },
    managerFn = { case SplitterArg(newSize, ports) =>
      if (newSize == 0) Nil else
      ports.grouped(newSize).toList.transpose.map { seq =>
        val fifoIdFactory = TLXbar.relabeler()
        seq(0).copy(
          minLatency = seq.map(_.minLatency).min,
          endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
          managers = seq.flatMap { port =>
            require (port.beatBytes == seq(0).beatBytes,
              s"Splitter data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
            val fifoIdMapper = fifoIdFactory()
            port.managers map { manager => manager.copy(
              fifoId = manager.fifoId.map(fifoIdMapper(_))
            )}
          }
        )
      }
    })

  lazy val module = new LazyModuleImp(this) {
    def group[T](x: Seq[T]) =
      if (x.isEmpty) Nil else x.grouped(node.in.size).toList.transpose

    if (node.out.size == node.in.size) {
      (node.in zip node.out) foreach { case ((i, _), (o, _)) => o <> i }
    } else (node.in zip group(node.out)) foreach {
      case ((io_in, edgeIn), seq) =>
      val (io_out, edgesOut) = seq.unzip

      // Grab the port ID mapping
      val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))

      // Find a good mask for address decoding
      val port_addrs = edgesOut.map(_.manager.managers.map(_.address).flatten)
      val routingMask = AddressDecoder(port_addrs)
      val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
      val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))

      // We need an intermediate size of bundle with the widest possible identifiers
      val wide_bundle = TLBundleParameters.union(Seq(io_in.params) ++ io_out.map(_.params))

      // Transform input bundle sources (sinks use global namespace on both sides)
      val in = Wire(TLBundle(wide_bundle))
      in.a <> io_in.a
      io_in.d <> in.d

      if (edgeIn.client.anySupportProbe && edgesOut.exists(_.manager.anySupportAcquireB)) {
        in.c <> io_in.c
        in.e <> io_in.e
        io_in.b <> in.b
      } else {
        in.c.valid := Bool(false)
        in.e.valid := Bool(false)
        in.b.ready := Bool(false)
        io_in.c.ready := Bool(true)
        io_in.e.ready := Bool(true)
        io_in.b.valid := Bool(false)
      }

      // Handle size = 1 gracefully (Chisel3 empty range is broken)
      def trim(id: UInt, size: Int) = if (size <= 1) UInt(0) else id(log2Ceil(size)-1, 0)

      // Transform output bundle sinks (sources use global namespace on both sides)
      val out = Wire(Vec(io_out.size, TLBundle(wide_bundle)))
      for (i <- 0 until out.size) {
        val r = outputIdRanges(i)

        io_out(i).a <> out(i).a
        out(i).d <> io_out(i).d
        out(i).d.bits.sink := io_out(i).d.bits.sink | UInt(r.start)

        if (edgesOut(i).manager.anySupportAcquireB && edgeIn.client.anySupportProbe) {
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

      val requestA = Vec(outputPorts.map { o => o(in.a.bits.address) })
      val requestC = Vec(outputPorts.map { o => o(in.c.bits.address) })
      val requestE = Vec(outputIdRanges.map { o => o.contains(in.e.bits.sink) })
      (out.map(_.a) zip TLXbar.fanout(in.a, requestA)) foreach { case (o, i) => o <> i }
      (out.map(_.c) zip TLXbar.fanout(in.c, requestC)) foreach { case (o, i) => o <> i }
      (out.map(_.e) zip TLXbar.fanout(in.e, requestE)) foreach { case (o, i) => o <> i }

      val beatsB = Vec((out zip edgesOut) map { case (o, e) => e.numBeats1(o.b.bits) })
      val beatsD = Vec((out zip edgesOut) map { case (o, e) => e.numBeats1(o.d.bits) })
      TLArbiter(policy)(in.b, (beatsB zip out.map(_.b)):_*)
      TLArbiter(policy)(in.d, (beatsD zip out.map(_.d)):_*)
    }
  }
}
