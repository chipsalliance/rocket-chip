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

sealed trait OMAddressTranslationMode

case object Sv32 extends OMAddressTranslationMode
case object Sv39 extends OMAddressTranslationMode
case object Sv48 extends OMAddressTranslationMode

sealed trait OMBaseInstructionSet

case object RV32E extends OMBaseInstructionSet {
  val base = "RV32E"
}
case object RV32I extends OMBaseInstructionSet {
  val base = "RV32I"
}
case object RV64I extends OMBaseInstructionSet {
  val base = "RV64I"
}
case object RV128I extends OMBaseInstructionSet {
  val base = "RV128I"
}

case class OMISA(
                  _types: Seq[String] = Nil,
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
) extends OMCompoundType {
  override def getTypes(): Seq[String] = Seq[String]("ISA") ++ super.getTypes
}
