// See LICENSE for license details.
package freechips.rocketchip.tilelink

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.reflect.ClassTag

class TLUserUniformer[T <: UserBits : ClassTag](default: UInt)(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode(
    clientFn  = { cp =>
      val collected = cp.clients.map(_.userBits.collect { case x: T => x }) 
      val longest = collected.sortBy(_.length).reverse.head
      require(collected.forall(x => x.isEmpty || x.map(_.width).forall(y => y == longest.head.width)),
        s"Cant make disjoint sets of user bits uniform $collected")
      // TODO figure out how to support different widths, which is hard because
      // they might missing or not or be in different orders per client
      cp.copy(
        clients = cp.clients.map { c =>
          val diff = longest.filterNot(c.userBits.contains(_))
          c.copy(userBits = c.userBits ++ diff) 
        }
      )
    },
    managerFn = { mp => mp })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      out.a.bits.user.foreach {
        val prev = edgeIn.getUserOrElse[T](in.a.bits, 0.U)
        val mux = edgeOut.putUser(
          in.a.bits.user.getOrElse(default), 
          prev.map { x: UInt => { (y: TLClientParameters) => x } }
        )
        _ := mux(out.a.bits.source)
      }
    }
  }
}
