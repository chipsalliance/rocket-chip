// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.rocket.BTBParams

case class OMRocketBranchPredictor(
  nBtbEntries: Int,
  nBhtEntries: Int,
  nRasEntries: Int,
  _types: Seq[String] = Seq("OMRocketBranchPredictor", "OMBranchPredictor", "OMComponent", "OMCompoundType")
) extends OMBranchPredictor

case class OMRocketCore(
  isa: OMISA,
  mulDiv: Option[OMMulDiv],
  fpu: Option[OMFPU],
  performanceMonitor: Option[OMPerformanceMonitor],
  pmp: Option[OMPMP],
  documentationName: String,
  hartIds: Seq[Int],
  hasVectoredInterrupts: Boolean,
  interruptLatency: Int,
  nLocalInterrupts: Int,
  rnmiPresent: Boolean,
  unmiPresent: Boolean,
  nBreakpoints: Int,
  mcontextWidth: Int,
  scontextWidth: Int,
  branchPredictor: Option[OMRocketBranchPredictor],
  dcache: Option[OMDCache],
  icache: Option[OMICache],
  busErrorUnit: Option[OMBusError],
  hasClockGate: Boolean,
  hasSCIE: Boolean,
  vmPresent: Boolean,
  utlb: Option[OMUTLB],
  _types: Seq[String] = Seq("OMRocketCore", "OMCore", "OMComponent", "OMCompoundType")
) extends OMCore

object OMBTB {
  def makeOMI(p: BTBParams): OMRocketBranchPredictor = {
    OMRocketBranchPredictor(
      nBtbEntries = p.nEntries,
      nBhtEntries = p.bhtParams.map(_.nEntries).getOrElse(0),
      nRasEntries = p.nRAS
    )
  }
}
