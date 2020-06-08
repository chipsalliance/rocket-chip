// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

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

case class  OMRegFieldEnumeration(
  id: BigInt,
  name: String,
  description: String,
  _types: Seq[String] = Seq("OMRegFieldEnumeration", "OMCompoundType")
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
  enumerations: Seq[OMRegFieldEnumeration] = Seq(),
  addressBlock: Option[String] = None,
  // TODO: register files
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

case class OMAddressBlock (
  name: String,
  baseAddress:  BigInt, // to match the IP-XACT terminology, though this is in practice an offset
  range: BigInt,
  width: Int,
  _types: Seq[String] = Seq("OMAddressBlock", "OMCompoundType")
) extends OMCompoundType

// TODO: OMRegisterFile

case class OMRegisterMap (
  registerFields: Seq[OMRegField],
  groups: Seq[OMRegFieldGroup],
  addressBlocks: Seq[OMAddressBlock] = Nil, // Note: this is intended to be used in the OMMemoryMap and is rather redundant here.
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
  // would it make more sense to add a name: Option[String] to OMAddressSet? This seems redundant
  registerMap: Option[OMRegisterMap],
  addressBlocks: Seq[OMAddressBlock] = Nil,
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

  private def getRegFieldEnumerations(enumerations: Map[BigInt, (String, String)]): List[OMRegFieldEnumeration] = {
    enumerations.map{
      case (key:BigInt, (name: String, description: String)) =>
        OMRegFieldEnumeration(
          id = key,
          name = name,
          description = description
        )
    }.toList
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
          resetValue = rfd.reset,
          enumerations = getRegFieldEnumerations(rfd.enumerations),
          addressBlock = rfd.addressBlock.map{_.name}
          // TODO: register files
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
    }.sortBy(_.bitRange.base).filter(_.bitRange.size > 0)
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

  private def makeAddressBlocks(mapping: Seq[(Int, Seq[RegField])]): Seq[OMAddressBlock] = {
    val addressBlocks = for {
      (_,seq) <- mapping
      regField <- seq
      desc <- regField.desc
      ab <- desc.addressBlock
    } yield  OMAddressBlock(
      name = ab.name,
      baseAddress = ab.addressOffset,
      range = ab.range,
      width = ab.width
    )
    addressBlocks.distinct.sortBy(_.baseAddress)
  }

  private def makeRegisterMap(mapping: Seq[(Int, Seq[RegField])]): OMRegisterMap = {
    OMRegisterMap(
      registerFields = makeRegisters(mapping),
      groups = makeGroups(mapping),
      addressBlocks = makeAddressBlocks(mapping)
    )
  }

  def convert(mapping: RegField.Map*): OMRegisterMap = {
    makeRegisterMap(mapping)
  }

  def convertSeq(mapping: Seq[RegField.Map]): OMRegisterMap = OMRegister.convert(mapping: _*)

}
