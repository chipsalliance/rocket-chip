// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import chisel3.experimental.RawModule
import freechips.rocketchip.regmapper._

trait OMRange extends OMCompoundType {
  def base: BigInt
  def size: BigInt
}

case class OMBitRange(
  base: BigInt,
  size: BigInt,
  _types: Seq[String] = Seq("OMBitRange", "OMCompoundType")
) extends OMRange

case class OMAddressSet(
  base: BigInt,
  mask: BigInt,
  _types: Seq[String] = Seq("OMAddressSet", "OMCompoundType")
) extends OMCompoundType

// Permissions are for memory regions
case class OMPermissions(
  readable: Boolean,
  writeable: Boolean,
  executable: Boolean,
  cacheable: Boolean,
  atomics: Boolean,
  _types: Seq[String] = Seq("OMPermissions", "OMCompoundType")
) extends OMCompoundType

case class OMRegFieldDesc(
  name: String,
  description: String,
  group: Option[String],
  access: OMRegFieldAccessType,
  wrType: Option[OMRegFieldWrType],
  rdAction: Option[OMRegFieldRdAction],
  volatile: Boolean,
  resetValue: Option[BigInt],
  _types: Seq[String] = Seq("OMRegFieldDesc", "OMCompoundType")
) extends OMCompoundType

case class OMRegField (
  bitRange: OMBitRange,
  description: Option[OMRegFieldDesc],
  _types: Seq[String] = Seq("OMRegField", "OMCompoundType")
) extends OMCompoundType

case class OMRegFieldGroup (
  name: String,
  description: Option[String],
  _types: Seq[String] = Seq("OMRegFieldGroup", "OMCompoundType")
) extends OMCompoundType

case class OMRegisterMap (
  registerFields: Seq[OMRegField],
  groups: Seq[OMRegFieldGroup],
  _types: Seq[String] = Seq("OMRegisterMap", "OMCompoundType")
) extends OMCompoundType

  /**
    * A device can have multiple MemoryRegions.
    * An example is the SPIFlash. There is a MemoryRegion for the control registers and the
    * the actual memory mapped flash which behaves like a memory.
    **/
case class OMMemoryRegion (
  name: String,
  description: String,
  addressSets: Seq[OMAddressSet],
  permissions: OMPermissions,
  registerMap: Option[OMRegisterMap],
  _types: Seq[String] = Seq("OMMemoryRegion", "OMCompoundType")
) extends OMCompoundType

object OMRegister {

  private def getRegFieldAccessType(rfd: RegFieldDesc): OMRegFieldAccessType = {
    rfd.access match {
      case RegFieldAccessType.R => R
      case RegFieldAccessType.W => W
      case RegFieldAccessType.RW => RW
    }
  }

  private def getRegFieldWrType(rfd: RegFieldDesc): Option[OMRegFieldWrType] = {
    rfd.wrType.map {
      wrt =>
        wrt match {
          case RegFieldWrType.ONE_TO_CLEAR => RFWT_ONE_TO_CLEAR
          case RegFieldWrType.ONE_TO_SET => RFWT_ONE_TO_SET
          case RegFieldWrType.ONE_TO_TOGGLE => RFWT_ONE_TO_TOGGLE
          case RegFieldWrType.ZERO_TO_CLEAR => RFWT_ZERO_TO_CLEAR
          case RegFieldWrType.ZERO_TO_SET => RFWT_ZERO_TO_SET
          case RegFieldWrType.ZERO_TO_TOGGLE => RFWT_ZERO_TO_TOGGLE
          case RegFieldWrType.CLEAR => RFWT_CLEAR
          case RegFieldWrType.SET => RFWT_SET
          case RegFieldWrType.MODIFY => RFWT_MODIFY
        }
    }
  }

  private def getRegFieldRdAction(rfd: RegFieldDesc): Option[OMRegFieldRdAction] = {
    rfd.rdAction.map {
      ra =>
        ra match {
          case RegFieldRdAction.CLEAR => RFRA_CLEAR
          case RegFieldRdAction.SET => RFRA_SET
          case RegFieldRdAction.MODIFY => RFRA_MODIFY
        }
    }
  }

  private def getRegFieldDesc(rf: RegField, byteOffset: Int, bitOffset: Int): Option[OMRegFieldDesc] = {
    rf.desc.map {
      rfd =>
        OMRegFieldDesc(
          name = rfd.name,
          description = rfd.desc,
          group = rfd.group,
          access = getRegFieldAccessType(rfd),
          wrType = getRegFieldWrType(rfd),
          rdAction = getRegFieldRdAction(rfd),
          volatile = rfd.volatile,
          resetValue = rfd.reset
        )
    }
  }

  private def getBitRange(rf: RegField, byteOffset: Int, bitOffset: Int): OMBitRange = {
    OMBitRange(base = (byteOffset * 8) + bitOffset, size = rf.width)
  }

  private def getRegField(rf: RegField, byteOffset: Int, bitOffset: Int): OMRegField = {
    OMRegField (
      bitRange = getBitRange(rf, byteOffset, bitOffset),
      description = getRegFieldDesc(rf, byteOffset, bitOffset)
    )
  }

  private def makeRegisters(mapping: Seq[(Int, Seq[RegField])]): Seq[OMRegField] = {
    mapping.flatMap {
      case (byteOffset, seq) =>
        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bitOffset, regField) =>
          getRegField(regField, byteOffset, bitOffset)
        }
    }.sortBy(_.bitRange.base)
  }

  private def makeGroups(mapping: Seq[(Int, Seq[RegField])]): Seq[OMRegFieldGroup] = {
    val groups = for {
      (_,seq) <- mapping
      regField <- seq
      desc <- regField.desc
    } yield  OMRegFieldGroup(
      name = desc.group.getOrElse(""),
      description = desc.groupDesc
    )
    groups.distinct.sortBy(_.name)
  }

  private def makeRegisterMap(mapping: Seq[(Int, Seq[RegField])]): OMRegisterMap = {
    OMRegisterMap(
      registerFields = makeRegisters(mapping),
      groups = makeGroups(mapping)
    )
  }

  def convert(mapping: RegField.Map*): OMRegisterMap = {
    makeRegisterMap(mapping)
  }

}
