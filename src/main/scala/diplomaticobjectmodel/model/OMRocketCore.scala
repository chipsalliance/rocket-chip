// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.diplomacy.ResourceBindings
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.rocket.{BTBParams, DCacheParams, ICacheParams}

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
  nBreakpoints: Int,
  branchPredictor: Option[OMRocketBranchPredictor],
  dcache: Option[OMDCache],
  icache: Option[OMICache],
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

object OMCaches {
  def dcache(p: DCacheParams, resourceBindings: Option[ResourceBindings]): OMDCache = {
    OMDCache(
      memoryRegions = resourceBindings.map(DiplomaticObjectModelAddressing.getOMMemoryRegions("DCache", _)).getOrElse(Nil),
      interrupts = Nil,
      nSets = p.nSets,
      nWays = p.nWays,
      blockSizeBytes = p.blockBytes,
      dataMemorySizeBytes = p.nSets * p.nWays * p.blockBytes,
      dataECC = p.dataECC.map(OMECC.getCode(_)),
      tagECC = p.tagECC.map(OMECC.getCode(_)),
      nTLBEntries = p.nTLBEntries
    )
  }

  def icache(p: ICacheParams, resourceBindings: Option[ResourceBindings]): OMICache = {
    OMICache(
      memoryRegions = resourceBindings.map(DiplomaticObjectModelAddressing.getOMMemoryRegions("ICache", _)).getOrElse(Nil),
      interrupts = Nil,
      nSets = p.nSets,
      nWays = p.nWays,
      blockSizeBytes = p.blockBytes,
      dataMemorySizeBytes = p.nSets * p.nWays * p.blockBytes,
      dataECC = p.dataECC.map(OMECC.getCode),
      tagECC = p.tagECC.map(OMECC.getCode),
      nTLBEntries = p.nTLBEntries,
      maxTimSize = p.nSets * (p.nWays-1) * p.blockBytes
    )
  }

}
