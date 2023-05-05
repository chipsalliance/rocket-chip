// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

class TLFilter(
  mfilter: TLFilter.ManagerFilter = TLFilter.mIdentity,
  cfilter: TLFilter.ClientFilter  = TLFilter.cIdentity
  )(implicit p: Parameters) extends LazyModule
{
  val node = new TLAdapterNode(
    clientFn  = { cp => cp.v1copy(clients = cp.clients.flatMap { c =>
      val out = cfilter(c)
      out.map { o => // Confirm the filter only REMOVES capability
        require (c.sourceId.contains(o.sourceId))
        require (c.supports.probe.contains(o.supports.probe))
        require (c.supports.arithmetic.contains(o.supports.arithmetic))
        require (c.supports.logical.contains(o.supports.logical))
        require (c.supports.get.contains(o.supports.get))
        require (c.supports.putFull.contains(o.supports.putFull))
        require (c.supports.putPartial.contains(o.supports.putPartial))
        require (c.supports.hint.contains(o.supports.hint))
        require (!c.requestFifo || o.requestFifo)
      }
      out
    })},
    managerFn = { mp =>
      val managers = mp.managers.flatMap { m =>
        val out = mfilter(m)
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
      mp.v1copy(managers = managers,
              endSinkId = if (managers.exists(_.supportsAcquireB)) mp.endSinkId else 0)
    }
  ) {
    override def circuitIdentity = true
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // In case the inner interface removes Acquire, tie-off the channels
      if (!edgeIn.manager.anySupportAcquireB) {
        in.b.valid := false.B
        in.c.ready := true.B
        in.e.ready := true.B
        out.b.ready := true.B
        out.c.valid := false.B
        out.e.valid := false.B
      }
    }
  }
}

object TLFilter
{
  type ManagerFilter = TLSlaveParameters => Option[TLSlaveParameters]
  type ClientFilter = TLMasterParameters => Option[TLMasterParameters]

  // preserve manager visibility
  def mIdentity: ManagerFilter = { m => Some(m) }
  // preserve client visibility
  def cIdentity: ClientFilter = { c => Some(c) }

  // make only the intersected address sets visible
  def mSelectIntersect(select: AddressSet): ManagerFilter = { m =>
    val filtered = m.address.map(_.intersect(select)).flatten
    val alignment = select.alignment /* alignment 0 means 'select' selected everything */
    transferSizeHelper(m, filtered, alignment)
  }

  // make everything except the intersected address sets visible
  def mSubtract(excepts: Seq[AddressSet]): ManagerFilter = { m =>
    val filtered = excepts.foldLeft(m.address) { (a,e) => a.flatMap(_.subtract(e)) }
    val alignment: BigInt = if (filtered.isEmpty) 0 else filtered.map(_.alignment).min
    transferSizeHelper(m, filtered, alignment)
  }

  def mSubtract(except: AddressSet): ManagerFilter = { m =>
    mSubtract(Seq(except))(m)
  }

  // adjust supported transfer sizes based on filtered intersection
  private def transferSizeHelper(m: TLSlaveParameters, filtered: Seq[AddressSet], alignment: BigInt): Option[TLSlaveParameters] = {
    val maxTransfer = 1 << 30
    val capTransfer = if (alignment == 0 || alignment > maxTransfer) maxTransfer else alignment.toInt
    val cap = TransferSizes(1, capTransfer)
    if (filtered.isEmpty) { None } else {
      Some(m.v1copy(
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

  // hide any fully contained address sets
  def mHideContained(containedBy: AddressSet): ManagerFilter = { m =>
    val filtered = m.address.filterNot(containedBy.contains(_))
    if (filtered.isEmpty) None else Some(m.v1copy(address = filtered))
  }
  // hide all cacheable managers
  def mHideCacheable: ManagerFilter = { m =>
    if (m.supportsAcquireB) None else Some(m)
  }
  // make visible only cacheable managers
  def mSelectCacheable: ManagerFilter = { m =>
    if (m.supportsAcquireB) Some(m) else None
  }
  // cacheable managers cannot be acquired from
  def mMaskCacheable: ManagerFilter = { m =>
    if (m.supportsAcquireB) {
      Some(m.v1copy(
        regionType       = RegionType.UNCACHED,
        supportsAcquireB = TransferSizes.none,
        supportsAcquireT = TransferSizes.none,
        alwaysGrantsT    = false))
    } else { Some(m) }
  }
  // only cacheable managers are visible, but cannot be acquired from
  def mSelectAndMaskCacheable: ManagerFilter = { m =>
    if (m.supportsAcquireB) {
      Some(m.v1copy(
        regionType       = RegionType.UNCACHED,
        supportsAcquireB = TransferSizes.none,
        supportsAcquireT = TransferSizes.none,
        alwaysGrantsT    = false))
    } else { None }
  }
  // hide all caching clients
  def cHideCaching: ClientFilter = { c =>
    if (c.supports.probe) None else Some(c)
  }
  // onyl caching clients are visible
  def cSelectCaching: ClientFilter = { c =>
    if (c.supports.probe) Some(c) else None
  }

  // removes resources from managers
  def mResourceRemover: ManagerFilter = { m =>
    Some(m.v2copy(resources=Nil))
  }

  // default application applies neither type of filter unless overridden
  def apply(
    mfilter: ManagerFilter = TLFilter.mIdentity,
    cfilter: ClientFilter  = TLFilter.cIdentity
    )(implicit p: Parameters): TLNode =
  {
    val filter = LazyModule(new TLFilter(mfilter, cfilter))
    filter.node
  }
}
