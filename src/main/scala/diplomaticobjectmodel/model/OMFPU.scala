// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMFPU(
  fLen: Int,
  _types: Seq[String] = Seq("OMFPU", "OMComponent", "OMCompoundType")
) extends OMComponent
