// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMPMP(
  specifications: Seq[OMSpecification],
  nRegions: Int,
  granularity: Int
) extends OMComponent
