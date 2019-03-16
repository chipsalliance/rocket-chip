// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMSpec {
  def name: String
  def version: String
  def _types: Seq[String]
}

case class OMSpecification(
  name: String,
  version: String,
  _types: Seq[String] = Seq("OMISASpecification", "OMSpecification")
) extends OMSpec

trait OMCustomExtensionSpecification extends OMSpec {
  def name: String
  def version: String
  def _types: Seq[String] = Seq("OMCustomExtensionSpecification", "OMSpecification")
}
