// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMPMP(
  specifications: Seq[OMSpecification],
  nRegions: Int,
  granularity: Int,
  _types: Seq[String] = Seq("OMPMP", "OMComponent", "OMCompoundType")
) extends OMComponent
