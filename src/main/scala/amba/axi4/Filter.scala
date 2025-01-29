// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import org.chipsalliance.cde.config.Parameters

import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import freechips.rocketchip.diplomacy.{AddressSet, TransferSizes}

class AXI4Filter(
  Sfilter: AXI4SubordinateParameters  => Option[AXI4SubordinateParameters]   = AXI4Filter.Sidentity,
  Mfilter: AXI4ManagerParameters => Option[AXI4ManagerParameters]  = AXI4Filter.Midentity
  )(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AdapterNode(
    subordinateFn  = { sp => sp.copy(subordinates = sp.subordinates.flatMap { s =>
      val out = Sfilter(s)
      out.foreach { o => // Confirm the filter only REMOVES capability
        o.address.foreach { a => require (s.address.map(_.contains(a)).reduce(_||_)) }
        require (o.regionType <= s.regionType)
        // we allow executable to be changed both ways
        require (s.supportsWrite.contains(o.supportsWrite))
        require (s.supportsRead .contains(o.supportsRead))
        require (!o.interleavedId.isDefined || s.interleavedId == o.interleavedId)
      }
      out
    })},
    managerFn = { mp => mp.copy(managers = mp.managers.flatMap { m =>
      val out = Mfilter(m)
      out.foreach { o => require (m.id.contains(o.id)) }
      out
    })})

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out :<>= in
    }
  }
}

object AXI4Filter
{
  def Midentity: AXI4ManagerParameters => Option[AXI4ManagerParameters] = { m => Some(m) }
  def Sidentity: AXI4SubordinateParameters  => Option[AXI4SubordinateParameters]  = { s => Some(s) }
  def Smask(select: AddressSet): AXI4SubordinateParameters  => Option[AXI4SubordinateParameters] = { s =>
    val filtered = s.address.map(_.intersect(select)).flatten
    val alignment = select.alignment /* alignment 0 means 'select' selected everything */
    val maxTransfer = 1 << 30
    val capTransfer = if (alignment == 0 || alignment > maxTransfer) maxTransfer else alignment.toInt
    val cap = TransferSizes(1, capTransfer)
    if (filtered.isEmpty) { None } else {
      Some(s.copy(
        address       = filtered,
        supportsWrite = s.supportsWrite.intersect(cap),
        supportsRead  = s.supportsRead .intersect(cap)))
    }
  }

  def apply(
    Sfilter: AXI4SubordinateParameters  => Option[AXI4SubordinateParameters]   = AXI4Filter.Sidentity,
    Mfilter: AXI4ManagerParameters => Option[AXI4ManagerParameters]  = AXI4Filter.Midentity
    )(implicit p: Parameters): AXI4Node =
  {
    val axi4filt = LazyModule(new AXI4Filter(Sfilter, Mfilter))
    axi4filt.node
  }
}
