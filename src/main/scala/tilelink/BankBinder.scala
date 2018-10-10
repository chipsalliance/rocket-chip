// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

case class BankBinderNode(mask: BigInt)(implicit valName: ValName) extends TLCustomNode
{
  val ids = AddressSet.enumerateMask(mask)

  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    val ports = ids.size
    val oStar = if (oStars == 0) 0 else (ports - oKnown) / oStars
    val iStar = if (iStars == 0) 0 else (ports - iKnown) / iStars
    require (ports == iKnown + iStar*iStars, s"${name} must have ${ports} inputs, but has ${iKnown} + ${iStar}*${iStars}$lazyModule.line}")
    require (ports == oKnown + oStar*oStars, s"${name} must have ${ports} outputs, but has ${iKnown} + ${iStar}*${iStars}$lazyModule.line}")
    (iStar, oStar)
  }

  def mapParamsD(n: Int, p: Seq[TLClientPortParameters]): Seq[TLClientPortParameters] =
    (p zip ids) map { case (cp, id) => cp.copy(clients = cp.clients.map { c => c.copy(visibility = c.visibility.flatMap { a =>
      a.intersect(AddressSet(id, ~mask))})})}

  def mapParamsU(n: Int, p: Seq[TLManagerPortParameters]): Seq[TLManagerPortParameters] =
    (p zip ids) map { case (mp, id) => mp.copy(managers = mp.managers.map { m => m.copy(address = m.address.flatMap { a =>
      a.intersect(AddressSet(id, ~mask))})})}
}

/* A BankBinder is used to divide contiguous memory regions into banks, suitable for a cache  */
class BankBinder(mask: BigInt)(implicit p: Parameters) extends LazyModule
{
  val node = BankBinderNode(mask)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
    }
  }
}

object BankBinder
{
  def apply(mask: BigInt)(implicit p: Parameters): TLNode = {
    val binder = LazyModule(new BankBinder(mask))
    binder.node
  }
}
