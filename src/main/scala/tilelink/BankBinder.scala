// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

case class BankBinderNode(mask: BigInt)(implicit valName: ValName) extends TLCustomNode
{
  private val bit = mask & -mask
  val maxXfer = TransferSizes(1, if (bit == 0 || bit > 4096) 4096 else bit.toInt)
  val ids = AddressSet.enumerateMask(mask)

  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    val ports = ids.size
    val oStar = if (oStars == 0) 0 else (ports - oKnown) / oStars
    val iStar = if (iStars == 0) 0 else (ports - iKnown) / iStars
    require (ports == iKnown + iStar*iStars, s"${name} must have ${ports} inputs, but has ${iKnown} + ${iStar}*${iStars} (at ${lazyModule.line})")
    require (ports == oKnown + oStar*oStars, s"${name} must have ${ports} outputs, but has ${oKnown} + ${oStar}*${oStars} (at ${lazyModule.line})")
    (iStar, oStar)
  }

  def mapParamsD(n: Int, p: Seq[TLMasterPortParameters]): Seq[TLMasterPortParameters] =
    (p zip ids) map { case (cp, id) => cp.v1copy(clients = cp.clients.map { c => c.v1copy(
      visibility         = c.visibility.flatMap { a => a.intersect(AddressSet(id, ~mask))},
      supportsProbe      = c.supports.probe      intersect maxXfer,
      supportsArithmetic = c.supports.arithmetic intersect maxXfer,
      supportsLogical    = c.supports.logical    intersect maxXfer,
      supportsGet        = c.supports.get        intersect maxXfer,
      supportsPutFull    = c.supports.putFull    intersect maxXfer,
      supportsPutPartial = c.supports.putPartial intersect maxXfer,
      supportsHint       = c.supports.hint       intersect maxXfer)})}

  def mapParamsU(n: Int, p: Seq[TLSlavePortParameters]): Seq[TLSlavePortParameters] =
    (p zip ids) map { case (mp, id) => mp.v1copy(managers = mp.managers.flatMap { m =>
      val addresses = m.address.flatMap(a => a.intersect(AddressSet(id, ~mask)))
      if (addresses.nonEmpty)
        Some(m.v1copy(
          address            = addresses,
          supportsAcquireT   = m.supportsAcquireT   intersect maxXfer,
          supportsAcquireB   = m.supportsAcquireB   intersect maxXfer,
          supportsArithmetic = m.supportsArithmetic intersect maxXfer,
          supportsLogical    = m.supportsLogical    intersect maxXfer,
          supportsGet        = m.supportsGet        intersect maxXfer,
          supportsPutFull    = m.supportsPutFull    intersect maxXfer,
          supportsPutPartial = m.supportsPutPartial intersect maxXfer,
          supportsHint       = m.supportsHint       intersect maxXfer))
      else None
    })}
}

/* A BankBinder is used to divide contiguous memory regions into banks, suitable for a cache  */
class BankBinder(mask: BigInt)(implicit p: Parameters) extends LazyModule
{
  val node = BankBinderNode(mask)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
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

  def apply(nBanks: Int, granularity: Int)(implicit p: Parameters): TLNode = {
    if (nBanks > 0) apply(granularity * (nBanks-1))
    else TLTempNode()
  }
}
