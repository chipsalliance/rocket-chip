// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import scala.math.{min,max}

class APBFanout()(implicit p: Parameters) extends LazyModule {
  val node = APBNexusNode(
    masterFn = { case Seq(m) => m },
    slaveFn  = { seq => seq(0).copy(slaves = seq.flatMap(_.slaves)) })

  lazy val module = new LazyModuleImp(this) {
    if (node.edges.in.size >= 1) {
      require (node.edges.in.size == 1, "APBFanout does not support multiple masters")
      require (node.edges.out.size > 0, "APBFanout requires at least one slave")

      val (in, _) = node.in(0)

      // Require consistent bus widths
      val (io_out, edgesOut) = node.out.unzip
      val port0 = edgesOut(0).slave
      edgesOut.foreach { edge =>
        val port = edge.slave
        require (port.beatBytes == port0.beatBytes,
          s"${port.slaves.map(_.name)} ${port.beatBytes} vs ${port0.slaves.map(_.name)} ${port0.beatBytes}")
      }

      val port_addrs = edgesOut.map(_.slave.slaves.map(_.address).flatten)
      val routingMask = AddressDecoder(port_addrs)
      val route_addrs = port_addrs.map(_.map(_.widen(~routingMask)).distinct)

      val sel = Vec(route_addrs.map(seq => seq.map(_.contains(in.paddr)).reduce(_ || _)))
      (sel zip io_out) foreach { case (sel, out) =>
        out := in
        out.psel    := sel && in.psel
        out.penable := sel && in.penable
      }

      in.pready  := !Mux1H(sel, io_out.map(!_.pready))
      in.pslverr :=  Mux1H(sel, io_out.map(_.pslverr))
      in.prdata  :=  Mux1H(sel, io_out.map(_.prdata))
    }
  }
}
