// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMMemory extends OMCompoundType {
  def description: String
  def addressWidth: Int
  def dataWidth: Int
  def depth: Int
  def writeMaskGranularity: Int
  def rtlModule: OMRTLModule
}