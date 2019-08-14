// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMCore extends OMComponent{
  def isa: OMISA
  def mulDiv: Option[OMMulDiv]
  def performanceMonitor: Option[OMPerformanceMonitor]
  def pmp: Option[OMPMP]
  def documentationName: String
  def hartIds: Seq[Int]
  def hasVectoredInterrupts: Boolean
  def interruptLatency: Int
  def nLocalInterrupts: Int
  def nBreakpoints: Int
}
