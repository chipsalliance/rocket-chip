// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMExtensionType extends OMEnum
case object M extends OMExtensionType
case object A extends OMExtensionType
case object F extends OMExtensionType
case object D extends OMExtensionType
case object C extends OMExtensionType
case object U extends OMExtensionType
case object S extends OMExtensionType

trait OMAddressTranslationMode extends OMEnum
case object Sv32 extends OMAddressTranslationMode
case object Sv39 extends OMAddressTranslationMode
case object Sv48 extends OMAddressTranslationMode

trait OMBaseInstructionSet extends OMEnum
case object RV32E extends OMBaseInstructionSet
case object RV32I extends OMBaseInstructionSet
case object RV64I extends OMBaseInstructionSet
case object RV128I extends OMBaseInstructionSet

case class OMISA(
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
  addressTranslationModes: Seq[OMAddressTranslationMode],
  _types: Seq[String] = Seq("OMISA", "OMCompoundType")
) extends OMCompoundType
