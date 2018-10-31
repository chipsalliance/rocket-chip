// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import org.json4s._

sealed trait OMExtensionType

case object M extends OMExtensionType
case object A extends OMExtensionType
case object F extends OMExtensionType
case object D extends OMExtensionType
case object C extends OMExtensionType
case object U extends OMExtensionType
case object S extends OMExtensionType

sealed trait OMAddressTranslationMode {
  val atm: String
}

case object Sv32 extends OMAddressTranslationMode {
  val atm = "Sv32"
}
case object Sv39 extends OMAddressTranslationMode {
  val atm = "Sv39"
}
case object Sv48 extends OMAddressTranslationMode {
  val atm = "Sv48"
}

sealed trait OMBaseInstructionSet {
  def isa: String
}

case object RV32E extends OMBaseInstructionSet {
  val isa = "RV32E"
}
case object RV32I extends OMBaseInstructionSet {
  val isa = "RV32I"
}
case object RV64I extends OMBaseInstructionSet {
  val isa = "RV64I"
}
case object RV128I extends OMBaseInstructionSet {
  val isa = "RV128I"
}

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

case class OMISAInfo(
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

  def apply(info: OMISAInfo): OMISA = {
    OMISA(
      _types = getTypes,
      xLen = info.xLen,
      baseSpecification = info.baseSpecification,
      base = info.base,
      m = info.m,
      a = info.a,
      f = info.f,
      d = info.d,
      c = info.c,
      u = info.u,
      s = info.s,
      addressTranslationModes = info.addressTranslationModes
    )
  }
}

case object OMBaseInstructionSetSerializer extends Serializer[OMBaseInstructionSet] {
  private val OMBaseInstructionSetClass = classOf[OMBaseInstructionSet]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), OMBaseInstructionSet] = {
    case (TypeInfo(OMBaseInstructionSetClass, _), json) => json match {
      case JObject(JField("isa", JString(isa)) :: _) =>
        isa  match {
          case "RV32E" => RV32E
          case "RV32I" => RV32I
          case "RV64I" => RV64I
          case "RV128I" => RV128I
          case x => throw new MappingException("Can't convert " + x + " to OMBaseInstructionSet")
        }
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case x: OMBaseInstructionSet =>
      import JsonDSL._
      ("base1" -> x.isa)
  }
}

case object OMAddressTranslationModeSerializer extends Serializer[OMAddressTranslationMode] {
  private val OMAddressTranslationModeClass = classOf[OMAddressTranslationMode]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), OMAddressTranslationMode] = {
    case (TypeInfo(OMAddressTranslationModeClass, _), json) => json match {
      case JObject(JField("atm", JString(atm)) :: _) =>
        atm  match {
          case "Sv32" => Sv32
          case "Sv39" => Sv39
          case "Sv48" => Sv48
          case x => throw new MappingException("Can't convert " + x + " to OMAddressTranslationMode")
        }
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case atm: OMAddressTranslationMode =>
      import JsonDSL._
      ("addressTranslationMode" -> atm.atm)
  }
}