// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.rocket.{BTBParams, MulDivParams}

case class OMRocketBranchPredictor(
  nBtbEntries: Int,
  nBhtEntries: Int,
  nRasEntries: Int,
  _types: Seq[String] = Seq("OMRocketBranchPredictor", "OMBranchPredictor", "OMComponent", "OMCompoundType")
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
  icache: Option[OMICache],
  _types: Seq[String] = Seq("OMRocketCore", "OMCore", "OMComponent", "OMCompoundType")
) extends OMCore

object BTB {
  def makeOMI(p: BTBParams): OMRocketBranchPredictor = {
    OMRocketBranchPredictor(
      nBtbEntries = p.nEntries,
      nBhtEntries = p.bhtParams.map(_.nEntries).getOrElse(0),
      nRasEntries = p.nRAS
    )
  }
}
