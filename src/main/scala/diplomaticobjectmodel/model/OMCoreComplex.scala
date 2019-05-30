// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMCoreComplex(
  components: Seq[OMComponent],
  documentationName: String,
  _types: Seq[String] = Seq("OMCoreComplex", "OMComponent", "OMCompoundType")
) extends OMComponent
