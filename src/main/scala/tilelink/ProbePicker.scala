// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

/* A ProbePicker is used to unify multiple cache banks into one logical cache  */
class ProbePicker(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn = { p =>
      // The ProbePicker assembles multiple clients based on the assumption they are contiguous in the clients list
      // This should be true for custers of xbar :=* BankBinder connections
      def combine(next: TLClientParameters, pair: (TLClientParameters, Seq[TLClientParameters])) = {
        val (head, output) = pair
        if (head.visibility.exists(x => next.visibility.exists(_.overlaps(x)))) {
          (next, head +: output) // pair is not banked, push head without merging
        } else {
          def redact(x: TLClientParameters) = x.copy(sourceId = IdRange(0,1), nodePath = Nil, visibility = Seq(AddressSet(0, ~0)))
          require (redact(next) == redact(head))
          val merge = head.copy(
            sourceId = IdRange(
              head.sourceId.start min next.sourceId.start,
              head.sourceId.end   max next.sourceId.end),
            visibility = AddressSet.unify(head.visibility ++ next.visibility))
          (merge, output)
        }
      }
      val myNil: Seq[TLClientParameters] = Nil
      val (head, output) = p.clients.init.foldRight((p.clients.last, myNil))(combine)
      p.copy(clients = head +: output)
    },
    managerFn = { p => p })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Based on address, adjust source to route to the correct bank
      if (edgeIn.client.clients.size != edgeOut.client.clients.size) {
        in.b.bits.source := Mux1H(
          edgeOut.client.clients.map(_.sourceId contains out.b.bits.source),
          edgeOut.client.clients.map { c =>
            val banks = edgeIn.client.clients.filter(c.sourceId contains _.sourceId)
            if (banks.size == 1) {
              out.b.bits.source // allow sharing the value between single-bank cases
            } else {
              Mux1H(
                banks.map(_.visibility.map(_ contains out.b.bits.address).reduce(_ || _)),
                banks.map(_.sourceId.start.U))
            }
          }
        )
      }
    }
  }
}

object ProbePicker
{
  def apply()(implicit p: Parameters): TLNode = {
    val picker = LazyModule(new ProbePicker)
    picker.node
  }
}
