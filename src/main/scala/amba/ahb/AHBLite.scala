// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

class AHBLite()(implicit p: Parameters) extends LazyModule {
  val node = AHBMasterAdapterNode(
    masterFn = { mp => mp },
    slaveFn  = { sp => sp.copy(lite = false) })

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      require (edgeOut.slave.lite) // or else this adapter is pointless

      out.hmastlock.get := in.hlock.get
      in.hgrant.get := true.B
      in.hresp := out.hresp // zero-extended

      in.hready := out.hready
      out.htrans := in.htrans
      out.hsize  := in.hsize
      out.hburst := in.hburst
      out.hwrite := in.hwrite
      out.hprot  := in.hprot
      out.haddr  := in.haddr
      out.hwdata := in.hwdata
      out.hauser :<>= in.hauser
      in.hduser :<>= out.hduser
      in.hrdata := out.hrdata
    }
  }
}

object AHBLite {
  def apply()(implicit p: Parameters) = {
    val ahblite = LazyModule(new AHBLite)
    ahblite.node
  }
}
