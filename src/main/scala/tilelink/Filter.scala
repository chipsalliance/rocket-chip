// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.{min,max}

class TLFilter(
  Mfilter: TLManagerParameters => Option[TLManagerParameters] = TLFilter.Midentity,
  Cfilter: TLClientParameters  => Option[TLClientParameters]  = TLFilter.Cidentity
  )(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { cp => cp.copy(clients = cp.clients.flatMap { c =>
      val out = Cfilter(c)
      out.map { o => // Confirm the filter only REMOVES capability
        require (c.sourceId.contains(o.sourceId))
        require (c.supportsProbe.contains(o.supportsProbe))
        require (c.supportsArithmetic.contains(o.supportsArithmetic))
        require (c.supportsLogical.contains(o.supportsLogical))
        require (c.supportsGet.contains(o.supportsGet))
        require (c.supportsPutFull.contains(o.supportsPutFull))
        require (c.supportsPutPartial.contains(o.supportsPutPartial))
        require (c.supportsHint.contains(o.supportsHint))
        require (!c.requestFifo || o.requestFifo)
      }
      out
    })},
    managerFn = { mp =>
      val managers = mp.managers.flatMap { m =>
        val out = Mfilter(m)
        out.map { o => // Confirm the filter only REMOVES capability
          o.address.foreach { a => require (m.address.map(_.contains(a)).reduce(_||_)) }
          require (o.regionType <= m.regionType)
          // we allow executable to be changed both ways
          require (m.supportsAcquireT.contains(o.supportsAcquireT))
          require (m.supportsAcquireB.contains(o.supportsAcquireB))
          require (m.supportsArithmetic.contains(o.supportsArithmetic))
          require (m.supportsLogical.contains(o.supportsLogical))
          require (m.supportsGet.contains(o.supportsGet))
          require (m.supportsPutFull.contains(o.supportsPutFull))
          require (m.supportsPutPartial.contains(o.supportsPutPartial))
          require (m.supportsHint.contains(o.supportsHint))
          require (!o.fifoId.isDefined || m.fifoId == o.fifoId)
        }
        out
      }
      mp.copy(managers = managers,
              endSinkId = if (managers.exists(_.supportsAcquireB)) mp.endSinkId else 0)
    })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // In case the inner interface removes Acquire, tie-off the channels
      if (!edgeIn.manager.anySupportAcquireB) {
        in.b.valid := Bool(false)
        in.c.ready := Bool(true)
        in.e.ready := Bool(true)
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }
    }
  }
}

object TLFilter
{
  def Midentity: TLManagerParameters => Option[TLManagerParameters] = { m => Some(m) }
  def Cidentity: TLClientParameters => Option[TLClientParameters] = { c => Some(c) }
  def Mmask(select: AddressSet): TLManagerParameters => Option[TLManagerParameters] = { m =>
    val filtered = m.address.map(_.intersect(select)).flatten
    val alignment = select.alignment /* alignment 0 means 'select' selected everything */
    val maxTransfer = 1 << 30
    val capTransfer = if (alignment == 0 || alignment > maxTransfer) maxTransfer else alignment.toInt
    val cap = TransferSizes(1, capTransfer)
    if (filtered.isEmpty) { None } else {
      Some(m.copy(
        address            = filtered,
        supportsAcquireT   = m.supportsAcquireT  .intersect(cap),
        supportsAcquireB   = m.supportsAcquireB  .intersect(cap),
        supportsArithmetic = m.supportsArithmetic.intersect(cap),
        supportsLogical    = m.supportsLogical   .intersect(cap),
        supportsGet        = m.supportsGet       .intersect(cap),
        supportsPutFull    = m.supportsPutFull   .intersect(cap),
        supportsPutPartial = m.supportsPutPartial.intersect(cap),
        supportsHint       = m.supportsHint      .intersect(cap)))
    }
  }
  def Mnocache: TLManagerParameters => Option[TLManagerParameters] = { m =>
    if (m.supportsAcquireB) None else Some(m)
  }
  def Mcache: TLManagerParameters => Option[TLManagerParameters] = { m =>
    if (m.supportsAcquireB) Some(m) else None
  }
  def Cnocache: TLClientParameters => Option[TLClientParameters] = { c =>
    if (c.supportsProbe) None else Some(c)
  }

  // applied to the TL source node; y.node := TLBuffer(x.node)
  def apply(
    Mfilter: TLManagerParameters => Option[TLManagerParameters] = TLFilter.Midentity,
    Cfilter: TLClientParameters  => Option[TLClientParameters]  = TLFilter.Cidentity
    )(implicit p: Parameters): TLNode =
  {
    val filter = LazyModule(new TLFilter(Mfilter, Cfilter))
    filter.node
  }
}
