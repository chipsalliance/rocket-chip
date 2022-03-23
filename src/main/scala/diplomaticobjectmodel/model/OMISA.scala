// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.util.BooleanToAugmentedBoolean

trait OMExtensionType extends OMEnum
case object M extends OMExtensionType
case object A extends OMExtensionType
case object F extends OMExtensionType
case object D extends OMExtensionType
case object C extends OMExtensionType
case object B extends OMExtensionType
case object U extends OMExtensionType
case object S extends OMExtensionType
case object H extends OMExtensionType

trait OMAddressTranslationMode extends OMEnum
case object Bare extends OMAddressTranslationMode
case object Sv32 extends OMAddressTranslationMode
case object Sv39 extends OMAddressTranslationMode
case object Sv48 extends OMAddressTranslationMode
// unratified/subject-to-change in the RISC-V priviledged ISA specification:
case object Sv57 extends OMAddressTranslationMode

trait OMBaseInstructionSet extends OMEnum
case object RV32E extends OMBaseInstructionSet
case object RV32I extends OMBaseInstructionSet
case object RV64E extends OMBaseInstructionSet
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
  b: Option[OMSpecification] = None,
  v: Option[OMVectorExtension] = None,
  u: Option[OMSpecification],
  s: Option[OMSpecification],
  h: Option[OMSpecification],
  addressTranslationModes: Seq[OMAddressTranslationMode],
  customExtensions: Seq[OMCustomExtensionSpecification],
  _types: Seq[String] = Seq("OMISA", "OMCompoundType")
) extends OMCompoundType

case class OMVectorExtension(
  version: String,
  vLen: Int,
  sLen: Int,
  eLen: Int,
  vstartALU: Boolean, // whether non-memory/non-vsetvl instructions permit vstart != 0
  name: String = "V Standard Extension for Vector Operations",
  _types: Seq[String] = Seq("OMVectorExtension")
)

object OMISA {
  def rocketISA(tile: RocketTile, xLen: Int, pgLevels: Int): OMISA = {
    val coreParams = tile.rocketParams.core

    val baseInstructionSet = xLen match {
      case 32 => if (coreParams.useRVE) RV32E else RV32I
      case 64 => if (coreParams.useRVE) RV64E else RV64I
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen: $xLen")
    }

    val customExtensions = {
      if (coreParams.haveCFlush) List (Xsifivecflushdlone(full = true, line = tile.dcache.canSupportCFlushLine)) else Nil
    }

    val isaExtSpec = ISAExtensions.specVersion _

    val baseSpec = BaseExtensions.specVersion _

    val baseISAVersion = baseInstructionSet match {
      case RV32E => "1.9"
      case RV32I => "2.0"
      case RV64E => "1.9"
      case RV64I => "2.0"
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid baseISAVersion: $baseInstructionSet")
    }

    val addressTranslationModes = xLen match {
      case _ if !coreParams.useVM => Bare
      case 32 if (pgLevels == 2) => Sv32
      case 64 if (pgLevels == 3) => Sv39
      case 64 if (pgLevels == 4) => Sv48
      case 64 if (pgLevels == 5) => Sv57
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen/PgLevels combination: $xLen/$pgLevels")
    }

    OMISA(
      xLen = xLen,
      baseSpecification = baseSpec(baseInstructionSet, baseISAVersion),
      base = baseInstructionSet,
      m = coreParams.mulDiv.map(x => isaExtSpec(M, "2.0")),
      a = coreParams.useAtomics.option(isaExtSpec(A, "2.0")),
      f = coreParams.fpu.map(x => isaExtSpec(F, "2.0")),
      d = coreParams.fpu.filter(_.fLen > 32).map(x => isaExtSpec(D, "2.0")),
      c = coreParams.useCompressed.option(isaExtSpec(C, "2.0")),
      u = (coreParams.hasSupervisorMode || coreParams.useUser).option(isaExtSpec(U, "1.10")),
      s = coreParams.hasSupervisorMode.option(isaExtSpec(S, "1.10")),
      h = coreParams.useHypervisor.option(isaExtSpec(H, "0.6")),
      addressTranslationModes = Seq(addressTranslationModes),
      customExtensions = customExtensions
    )
  }
}
