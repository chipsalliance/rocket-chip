// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.rocket.RocketCoreParams

case class OMPerformanceMonitor(
  specifications: List[OMSpecification],
  hasBasicCounters: Boolean,
  nAdditionalCounters: Int,
  _types: Seq[String] = Seq("OMPerformanceMonitor", "OMComponent", "OMCompoundType")
) extends OMComponent

object PerformanceMonitor {
  def permon(coreParams: RocketCoreParams): Option[OMPerformanceMonitor] = {
    if (coreParams.haveBasicCounters || coreParams.nPerfCounters > 0) {
      Some(OMPerformanceMonitor(
        specifications = List[OMSpecification](PrivilegedArchitectureExtensions.specVersion(MachineLevelISA, "1.10")),
        hasBasicCounters = coreParams.haveBasicCounters,
        nAdditionalCounters = coreParams.nPerfCounters
      ))
    }
    else {
      None
    }
  }
}