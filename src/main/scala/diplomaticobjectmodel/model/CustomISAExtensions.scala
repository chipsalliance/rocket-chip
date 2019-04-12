// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


import scala.collection.mutable

trait OMCustomExtensionSpecification{
  def name: String
  def version: String
  def _types: Seq[String] = Seq("OMCustomExtensionSpecification", "OMSpecification")
}

case class Xsifivecflushdlone(
  version: String = "0.1",
  name: String = "Cache Flush/Power Down Instructions custom extension specification",
  override val _types: Seq[String] = Seq("OMXsifivecflushdlone", "OMCustomExtensionSpecification", "OMSpecification")
) extends OMCustomExtensionSpecification
