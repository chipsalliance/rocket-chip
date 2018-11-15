// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMPerformanceMonitor(
  specifications: List[OMSpecification],
  hasBasicCounters: Boolean,
  nAdditionalCounters: Int
) extends OMComponent
