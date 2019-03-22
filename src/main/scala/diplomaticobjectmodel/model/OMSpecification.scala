// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMSpecification(
  name: String,
  version: String,
  _types: Seq[String] = Seq("OMSpecification")
)

trait OMCustomExtensionSpecification{
  def name: String
  def version: String
  def _types: Seq[String] = Seq("OMCustomExtensionSpecification", "OMSpecification")
}
