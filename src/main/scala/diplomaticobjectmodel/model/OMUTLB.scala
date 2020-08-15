// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMUTLB(
  utlbEntries: Int,
  memories: Seq[OMSRAM],
  _types: Seq[String] = Seq("OMUTLB", "OMComponent", "OMCompoundType")
) extends OMComponent
