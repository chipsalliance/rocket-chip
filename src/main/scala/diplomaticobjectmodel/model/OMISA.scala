// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMExtensionType

case object M extends OMExtensionType
case object A extends OMExtensionType
case object F extends OMExtensionType
case object D extends OMExtensionType
case object C extends OMExtensionType
case object U extends OMExtensionType
case object S extends OMExtensionType

trait OMAddressTranslationMode extends OMEnum
object Sv32 extends OMAddressTranslationMode
object Sv39 extends OMAddressTranslationMode
object Sv48 extends OMAddressTranslationMode

trait OMBaseInstructionSet extends OMEnum
object RV32E extends OMBaseInstructionSet
object RV32I extends OMBaseInstructionSet
object RV64I extends OMBaseInstructionSet
object RV128I extends OMBaseInstructionSet

case class OMISA(
                  _types: Seq[String],
                  xLen: Int,
                  baseSpecification: OMSpecification,
                  base: OMBaseInstructionSet,
                  m: Option[OMSpecification],
                  a: Option[OMSpecification],
                  f: Option[OMSpecification],
                  d: Option[OMSpecification],
                  c: Option[OMSpecification],
                  u: Option[OMSpecification],
                  s: Option[OMSpecification],
                  addressTranslationModes: Seq[OMAddressTranslationMode]
) extends OMCompoundType

object OMISA extends OMCompoundType {
  override def getTypes: Seq[String] = Seq[String]("ISA") ++ super.getTypes

  def apply(xLen: Int,
            baseSpecification: OMSpecification,
            base: OMBaseInstructionSet,
            m: Option[OMSpecification],
            a: Option[OMSpecification],
            f: Option[OMSpecification],
            d: Option[OMSpecification],
            c: Option[OMSpecification],
            u: Option[OMSpecification],
            s: Option[OMSpecification],
            addressTranslationModes: Seq[OMAddressTranslationMode]): OMISA = {
    OMISA(
      // _types = getTypes,
      xLen = xLen,
      baseSpecification = baseSpecification,
      base = base,
      m = m,
      a = a,
      f = f,
      d = d,
      c = c,
      u = u,
      s = s,
      addressTranslationModes = addressTranslationModes
    )
  }
}
