// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import config._
import diplomacy._

class TLSplitter(policy: TLArbiter.Policy = TLArbiter.lowestIndexFirst)(implicit p: Parameters) extends LazyModule
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
        val outputIdRanges = TLXbar.mapOutputIds(seq)
        seq(0).copy(
          minLatency = seq.map(_.minLatency).min,
          endSinkId = outputIdRanges.map(_.map(_.end).getOrElse(0)).max,
          managers = ManagerUnification(seq.zipWithIndex.flatMap { case (port, i) =>
            require (port.beatBytes == seq(0).beatBytes,
              s"Splitter data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
            val fifoIdMapper = fifoIdFactory()
            port.managers map { manager => manager.copy(
              fifoId = manager.fifoId.map(fifoIdMapper(_))
            )}
          })
        )
      }
    })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    def group[T](x: Seq[T]) =
      if (x.isEmpty) Nil else x.grouped(node.edgesIn.size).toList.transpose

    ((node.edgesIn zip io.in) zip (group(node.edgesOut) zip group(io.out))) foreach {
      case ((edgeIn, io_in), (edgesOut, io_out)) =>

      // Grab the port ID mapping
      val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))

      // Find a good mask for address decoding
      val port_addrs = edgesOut.map(_.manager.managers.map(_.address).flatten)
      val routingMask = AddressDecoder(port_addrs)
      val route_addrs = port_addrs.map(_.map(_.widen(~routingMask)).distinct)
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
        out(i).d.bits.sink := io_out(i).d.bits.sink | UInt(r.map(_.start).getOrElse(0))

        if (edgesOut(i).manager.anySupportAcquireB && edgeIn.client.anySupportProbe) {
          io_out(i).c <> out(i).c
          io_out(i).e <> out(i).e
          out(i).b <> io_out(i).b
          io_out(i).e.bits.sink := trim(out(i).e.bits.sink, r.map(_.size).getOrElse(0))
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
      val requestE = Vec(outputIdRanges.map { o => o.map(_.contains(in.e.bits.sink)).getOrElse(Bool(false)) })
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
