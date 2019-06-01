// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMMemory(
  description: String,
  addressWidth: Int,
  dataWidth: Int,
  depth: BigInt,
  writeMaskGranularity: Int,
  _types: Seq[String] = Seq("OMMemory")
)

