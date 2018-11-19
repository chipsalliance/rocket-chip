// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMPerformanceMonitor(
  specifications: List[OMSpecification],
  hasBasicCounters: Boolean,
  nAdditionalCounters: Int,
  _types: Seq[String] = Seq("OMPerformanceMonitor", "OMComponent", "OMCompoundType")
) extends OMComponent
