// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.{min,max}

class AXI4IdIndexer(idBits: Int)(implicit p: Parameters) extends LazyModule
{
  require (idBits >= 0)

  val node = AXI4AdapterNode(
    masterFn = { mp =>
      // Create one new "master" per ID
      val masters = Array.tabulate(1 << idBits) { i => AXI4MasterParameters(
         name      = "",
         id        = IdRange(i, i+1),
         aligned   = true,
         maxFlight = Some(0))
      }
      // Accumluate the names of masters we squish
      val names = Array.fill(1 << idBits) { new scala.collection.mutable.HashSet[String]() }
      // Squash the information from original masters into new ID masters
      mp.masters.foreach { m =>
        for (i <- m.id.start until m.id.end) {
          val j = i % (1 << idBits)
          val old = masters(j)
          names(j) += m.name
          masters(j) = old.copy(
            aligned   = old.aligned && m.aligned,
            maxFlight = old.maxFlight.flatMap { o => m.maxFlight.map { n => o+n } })
        }
      }
      mp.copy(
        userBits = mp.userBits + max(0, log2Ceil(mp.endId) - idBits),
        masters  = masters.zipWithIndex.map { case (m,i) => m.copy(name = names(i).toList.mkString(", "))})
    },
    slaveFn = { sp => sp
    })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>

      // Leave everything mostly untouched
      out.ar <> in.ar
      out.aw <> in.aw
      out.w <> in.w
      in.b <> out.b
      in.r <> out.r

      val bits = log2Ceil(edgeIn.master.endId) - idBits
      if (bits > 0) {
        // (in.aX.bits.id >> idBits).width = bits > 0
        out.ar.bits.user.get := Cat(in.ar.bits.user.toList ++ Seq(in.ar.bits.id >> idBits))
        out.aw.bits.user.get := Cat(in.aw.bits.user.toList ++ Seq(in.aw.bits.id >> idBits))
        // user.isDefined => width > 0
        in.r.bits.user.foreach { _ := out.r.bits.user.get >> bits }
        in.b.bits.user.foreach { _ := out.b.bits.user.get >> bits }
        // Special care is needed in case of 0 idBits, b/c .id has width 1 still
        if (idBits == 0) {
          out.ar.bits.id := UInt(0)
          out.aw.bits.id := UInt(0)
          in.r.bits.id := out.r.bits.user.get
          in.b.bits.id := out.b.bits.user.get
        } else {
          in.r.bits.id := Cat(out.r.bits.user.get, out.r.bits.id)
          in.b.bits.id := Cat(out.b.bits.user.get, out.b.bits.id)
        }
      }
    }
  }
}

object AXI4IdIndexer
{
  def apply(idBits: Int)(implicit p: Parameters): AXI4Node =
  {
    val axi4index = LazyModule(new AXI4IdIndexer(idBits))
    axi4index.node
  }
}
