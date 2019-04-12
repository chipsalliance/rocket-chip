package diplomaticobjectmodel.model

import freechips.rocketchip.diplomaticobjectmodel.model.{OMEnum, Xsifivecflushdlone}

trait OMCustomExtensionSpecification{
  def name: String
  def version: String
  def _types: Seq[String] = Seq("OMCustomExtensionSpecification", "OMSpecification")
}

object CustomISAExtensions {

  trait OMCustomExtensionType extends OMEnum
  case object XsifivecflushdloneKey extends OMCustomExtensionType

  def cflush: (String) => OMCustomExtensionSpecification = (s: String) => Xsifivecflushdlone("SiFive Extension for Cache Flush", s)

  val customSpecifications = Map[OMCustomExtensionType, (String) => OMCustomExtensionSpecification](
    XsifivecflushdloneKey -> cflush
  )

}
