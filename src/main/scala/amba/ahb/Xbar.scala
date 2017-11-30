// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import scala.math.{min,max}

class AHBFanout()(implicit p: Parameters) extends LazyModule {
  val node = AHBNexusNode(
    masterFn = { case Seq(m) => m },
    slaveFn  = { seq => seq(0).copy(slaves = seq.flatMap(_.slaves)) })

  lazy val module = new LazyModuleImp(this) {
    if (node.edges.in.size >= 1) {
      require (node.edges.in.size == 1, "AHBFanout does not support multiple masters")
      require (node.edges.out.size > 0, "AHBFanout requires at least one slave")

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

      val (in, _) = node.in(0)
      val a_sel = Vec(route_addrs.map(seq => seq.map(_.contains(in.haddr)).reduce(_ || _)))
      val d_sel = Reg(a_sel)

      when (in.hready) { d_sel := a_sel }
      (a_sel zip io_out) foreach { case (sel, out) =>
        out := in
        out.hsel := in.hsel && sel
      }

      in.hreadyout := !Mux1H(d_sel, io_out.map(!_.hreadyout))
      in.hresp     :=  Mux1H(d_sel, io_out.map(_.hresp))
      in.hrdata    :=  Mux1H(d_sel, io_out.map(_.hrdata))
    }
  }
}
