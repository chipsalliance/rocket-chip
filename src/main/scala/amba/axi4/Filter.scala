// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

class AXI4Filter(
  Sfilter: AXI4SlaveParameters  => Option[AXI4SlaveParameters]   = AXI4Filter.Sidentity,
  Mfilter: AXI4MasterParameters => Option[AXI4MasterParameters]  = AXI4Filter.Midentity
  )(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AdapterNode(
    slaveFn  = { sp => sp.copy(slaves = sp.slaves.flatMap { s =>
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
    masterFn = { mp => mp.copy(masters = mp.masters.flatMap { m =>
      val out = Mfilter(m)
      out.foreach { o => require (m.id.contains(o.id)) }
      out
    })})

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
    }
  }
}

object AXI4Filter
{
  def Midentity: AXI4MasterParameters => Option[AXI4MasterParameters] = { m => Some(m) }
  def Sidentity: AXI4SlaveParameters  => Option[AXI4SlaveParameters]  = { s => Some(s) }
  def Smask(select: AddressSet): AXI4SlaveParameters  => Option[AXI4SlaveParameters] = { s =>
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
    Sfilter: AXI4SlaveParameters  => Option[AXI4SlaveParameters]   = AXI4Filter.Sidentity,
    Mfilter: AXI4MasterParameters => Option[AXI4MasterParameters]  = AXI4Filter.Midentity
    )(implicit p: Parameters): AXI4Node =
  {
    val axi4filt = LazyModule(new AXI4Filter(Sfilter, Mfilter))
    axi4filt.node
  }
}
