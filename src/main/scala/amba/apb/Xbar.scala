// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._
import chisel3.util.Mux1H

import org.chipsalliance.cde.config.Parameters

import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import freechips.rocketchip.diplomacy.AddressDecoder
import freechips.rocketchip.util.BundleField

class APBFanout()(implicit p: Parameters) extends LazyModule {
  val node = new APBNexusNode(
    managerFn = { case Seq(m) => m },
    subordinateFn  = { seq =>
      seq(0).copy(
        subordinates         = seq.flatMap(_.subordinates),
        requestKeys    = seq.flatMap(_.requestKeys).distinct,
        responseFields = BundleField.union(seq.flatMap(_.responseFields))) }
  ){
    override def circuitIdentity = outputs == 1 && inputs == 1
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    if (node.edges.in.size >= 1) {
      require (node.edges.in.size == 1, "APBFanout does not support multiple managers")
      require (node.edges.out.size > 0, "APBFanout requires at least one subordinate")

      val (in, _) = node.in(0)

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

      val sel = VecInit(route_addrs.map(seq => seq.map(_.contains(in.paddr)).reduce(_ || _)))
      (sel zip io_out) foreach { case (sel, out) =>
        out.squeezeAll :<>= in.squeezeAll
        out.psel    := sel && in.psel
        out.penable := sel && in.penable
      }

      in.pready  := !Mux1H(sel, io_out.map(!_.pready))
      in.pslverr :=  Mux1H(sel, io_out.map(_.pslverr))
      in.prdata  :=  Mux1H(sel, io_out.map(_.prdata))
    }
  }
}
