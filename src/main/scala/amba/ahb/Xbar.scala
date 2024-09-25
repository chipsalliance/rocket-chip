// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import chisel3.util.Mux1H

import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import freechips.rocketchip.diplomacy.AddressDecoder
import freechips.rocketchip.util.BundleField


class AHBFanout()(implicit p: Parameters) extends LazyModule {
  val node = new AHBFanoutNode(
    managerFn = { case Seq(m) => m },
    subordinateFn  = { seq =>
      seq(0).copy(
        subordinates = seq.flatMap(_.subordinates),
        requestKeys    = seq.flatMap(_.requestKeys).distinct,
        responseFields = BundleField.union(seq.flatMap(_.responseFields))) }
  ){
    override def circuitIdentity = outputs == 1 && inputs == 1
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    if (node.edges.in.size >= 1) {
      require (node.edges.in.size == 1, "AHBFanout does not support multiple managers")
      require (node.edges.out.size > 0, "AHBFanout requires at least one subordinate")
      node.edges.out.foreach { eo => require (eo.subordinate.lite, s"AHBFanout only supports AHB-Lite subordinates (${eo.subordinate.subordinates.map(_.name)})") }

      // Require consistent bus widths
      val (io_out, edgesOut) = node.out.unzip
      val port0 = edgesOut(0).subordinate
      edgesOut.foreach { edge =>
        val port = edge.subordinate
        require (port.beatBytes == port0.beatBytes,
          s"${port.subordinates.map(_.name)} ${port.beatBytes} vs ${port0.subordinates.map(_.name)} ${port0.beatBytes}")
      }

      val port_addrs = edgesOut.map(_.subordinate.subordinates.map(_.address).flatten)
      val routingMask = AddressDecoder(port_addrs)
      val route_addrs = port_addrs.map(_.map(_.widen(~routingMask)).distinct)

      val (in, _) = node.in(0)
      val a_sel = VecInit(route_addrs.map(seq => seq.map(_.contains(in.haddr)).reduce(_ || _)))
      val d_sel = RegInit(a_sel)

      when (in.hready) { d_sel := a_sel }
      (a_sel zip io_out) foreach { case (sel, out) =>
        out.squeezeAll :<>= in.squeezeAll
        out.hsel := in.hsel && sel
        out.hmanager.map { _ := 0.U }
      }

      in.hreadyout := !Mux1H(d_sel, io_out.map(!_.hreadyout))
      in.hresp     :=  Mux1H(d_sel, io_out.map(_.hresp))
      in.hrdata    :=  Mux1H(d_sel, io_out.map(_.hrdata))
    }
  }
}

class AHBArbiter()(implicit p: Parameters) extends LazyModule {
  val node = AHBArbiterNode(
    managerFn = { case seq => seq(0).copy(managers = seq.flatMap(_.managers)) },
    subordinateFn  = { case Seq(s) => s })

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    if (node.edges.in.size >= 1) {
      require (node.edges.out.size == 1, "AHBArbiter requires exactly one subordinate")
      require (node.edges.in.size == 1, "TODO: support more than one manager")

      val (in,  _) = node.in(0)
      val (out, _) = node.out(0)

      out.hmastlock := in.lock()
      out.hsel      := in.busreq()
      out.hready    := out.hreadyout
      in.hready     := out.hreadyout
      out.htrans    := in.htrans
      out.hsize     := in.hsize
      out.hburst    := in.hburst
      out.hwrite    := in.hwrite
      out.hprot     := in.hprot
      out.haddr     := in.haddr
      out.hwdata    := in.hwdata
      in.hrdata     := out.hrdata
      in.hresp      := out.hresp // zero-extended
      in.hgrant.foreach { _ := true.B }
      out.hmanager.foreach { _ := 0.U }
      out.hauser :<= in.hauser
      in.hduser :<= out.hduser
    }
  }
}
