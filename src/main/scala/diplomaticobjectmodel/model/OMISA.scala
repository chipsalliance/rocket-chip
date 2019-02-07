// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.rocket.RocketCoreParams
import freechips.rocketchip.util.BooleanToAugmentedBoolean

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

object OMISA {
  def rocketISA(coreParams: RocketCoreParams, xLen: Int): OMISA = {
    val baseInstructionSet = xLen match {
      case 32 => RV32I
      case 64 => RV64I
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen: $xLen")
    }

    val isaExtSpec = ISAExtensions.specVersion _

    val baseSpec = BaseExtensions.specVersion _

    val baseISAVersion = baseInstructionSet match {
      case RV32E => "1.9"
      case RV32I => "2.0"
      case RV64I => "2.0"
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid baseISAVersion: $baseInstructionSet")
    }

    val addressTranslationModes = xLen match {
        case 32 => Sv32
        case 64 => Sv39
        case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen: $xLen")
      }

    OMISA(
      xLen = xLen,
      baseSpecification = baseSpec(baseInstructionSet, baseISAVersion),
      base = baseInstructionSet,
      m = coreParams.mulDiv.map(x => isaExtSpec(M, "2.0")),
      a = coreParams.useAtomics.option(isaExtSpec(A, "2.0")),
      f = coreParams.fpu.map(x => isaExtSpec(F, "2.0")),
      d = coreParams.fpu.filter(_.fLen > 32).map(x => isaExtSpec(D, "2.0")),
      c = coreParams.useCompressed.option(isaExtSpec(C, " 2.0")),
      u = coreParams.useUser.option(isaExtSpec(U, "1.10")),
      s = coreParams.useVM.option(isaExtSpec(S, "1.10")),
      addressTranslationModes = Seq(addressTranslationModes)
    )
  }
}