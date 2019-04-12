// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


import scala.collection.mutable

trait OMCustomExtensionSpecification{
  def name: String
  def version: String
  def _types: Seq[String] = Seq("OMCustomExtensionSpecification", "OMSpecification")
}

case class Xsifivecflushdlone(
  name: String,
  version: String,
  override val _types: Seq[String] = Seq("OMXsifivecflushdlone", "OMCustomExtensionSpecification", "OMSpecification")
) extends OMCustomExtensionSpecification


trait OMCustomExtensionType extends OMEnum
case object XsifivecflushdloneKey extends OMCustomExtensionType

object CustomExtensionsLibrary {

  private def cflushFunc: (String) => OMCustomExtensionSpecification = (s: String) => Xsifivecflushdlone("SiFive Extension for Cache Flush", s)

  private val lib = Map[OMCustomExtensionType, (String) => OMCustomExtensionSpecification](
    (XsifivecflushdloneKey -> cflushFunc)
  )

  def get(key: OMCustomExtensionType): (String) => OMCustomExtensionSpecification =
    lib.get(key).getOrElse(throw new IllegalArgumentException("Error: key: $key not found in custom extension library."))
}

class CustomISAExtensions {
  private val customSpecifications = mutable.ArrayBuffer[OMCustomExtensionSpecification]()

  def add(key: OMCustomExtensionType, version: String): Unit = {
    val f = CustomExtensionsLibrary.get(key)(version)
    customSpecifications += f
  }

  def get(): List[OMCustomExtensionSpecification] = customSpecifications.toList
}
