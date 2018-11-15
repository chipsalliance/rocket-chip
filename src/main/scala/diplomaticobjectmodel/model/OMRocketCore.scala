// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMRocketBranchPredictor(
  nBtbEntries: Int,
  nBhtEntries: Int,
  nRasEntries: Int
) extends OMBranchPredictor

case class OMRocketCore(
  isa: OMISA,
  mulDiv: Option[OMMulDiv],
  performanceMonitor: Option[OMPerformanceMonitor],
  pmp: Option[OMPMP],
  documentationName: String,
  hartIds: Seq[Int],
  hasTrace: Boolean,
  hasVectoredInterrupts: Boolean,
  interruptLatency: Int,
  nLocalInterrupts: Int,
  nBreakpoints: Int,
  branchPredictor: Option[OMRocketBranchPredictor],
  dcache: Option[OMDCache],
  icache: Option[OMICache]
) extends OMCore
