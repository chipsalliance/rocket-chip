// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import chisel3.experimental.RawModule
import freechips.rocketchip.regmapper.RegFieldAccessType.RegFieldAccessType
import freechips.rocketchip.regmapper.RegFieldRdAction.RegFieldRdAction
import freechips.rocketchip.regmapper.RegFieldWrType.RegFieldWrType
import freechips.rocketchip.regmapper._

trait OMRange extends OMCompoundType {
  def base: BigInt
  def size: BigInt
}

case class OMBitRange(
  base: BigInt,
  size: BigInt
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
  name: String,
  description: String,
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

  def makeRegMappingSer(
    rawModule: RawModule,
    moduleName: String,
    baseAddress: BigInt,
    width: Int,
    byteOffset: Int,
    bitOffset: Int,
    regField: RegField): RegFieldDescSer = {

    val anonRegFieldName = s"unnamedRegField${byteOffset.toHexString}_${bitOffset}"
    val selectedRegFieldName = regField.desc.map(_.name).getOrElse(anonRegFieldName)

    val map = Map[BigInt, (String, String)]() // TODO

    // TODO: enumerations will be handled in upcoming PR
    //    ("enumerations" -> desc.map {d =>
    //      Option(d.enumerations.map { case (key, (name, edesc)) =>
    //        (("value" -> key) ~ ("name" -> name) ~ ("description" -> edesc))
    //      }).filter(_.nonEmpty)}) )

    val desc = regField.desc

    val regFieldDescSer = RegFieldDescSer(
      byteOffset = s"0x${byteOffset.toInt.toHexString}",
      bitOffset = bitOffset,
      bitWidth = width,
      name = selectedRegFieldName,
      desc = desc.map {_.desc}.getOrElse("None"),
      group = desc.map {_.group.getOrElse("None")}.getOrElse("None"),
      groupDesc = desc.map {_.groupDesc.getOrElse("None")}.getOrElse("None"),
      accessType = desc.map {_.access.toString}.getOrElse("None"),
      wrType = desc.map(_.wrType.toString).getOrElse("None"),
      rdAction = desc.map(_.rdAction.toString).getOrElse("None"),
      volatile = desc.map(_.volatile).getOrElse(false),
      hasReset = desc.map {_.reset != None }.getOrElse(false),
      resetValue = desc.map{_.reset.getOrElse(BigInt(0))}.getOrElse(BigInt(0)),
      enumerations = map
    )

    regFieldDescSer
  }

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

  private def getRegFieldDesc(rf: RegField): Option[OMRegFieldDesc] = {
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

  private def getBitRange(rf: RegField): OMBitRange = {
    OMBitRange(base = 0, size = 0)
  }

  private def getRegField(rf: RegField): OMRegField = {
    OMRegField (
      bitRange = getBitRange(rf),
      description = getRegFieldDesc(rf)
    )
  }

  private def makeRegisters(baseAddress: BigInt,
    mapping: Seq[(Int, Seq[RegField])]): Seq[OMRegField] = {
    mapping.flatMap {
      case (byteOffset, seq) =>
        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bitOffset, regField) =>

         getRegField(regField)
        }
    }
  }

  private def makeGroups(baseAddress: BigInt,
    mapping: Seq[(Int, Seq[RegField])]): Seq[OMRegFieldGroup] = {
    mapping.flatMap {
      case (byteOffset, seq) =>
        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bitOffset, regField) =>
          OMRegFieldGroup(
            name = "",
            description = None // Option[String],
          )
        }
    }
  }

  private def makeRegisterMap(rawModule: RawModule,
    baseAddress: BigInt,
    mapping: Seq[(Int, Seq[RegField])]): OMRegisterMap = {

    makeRegisters(baseAddress, mapping)

    OMRegisterMap(
      name = "",
      description = "",
      registerFields = makeRegisters(baseAddress, mapping),
      groups = makeGroups(baseAddress, mapping)
    )
  }

  private def makeMemoryRegionFromRegisters(
    rawModule: RawModule,
    baseAddress: BigInt,
    mapping: Seq[(Int, Seq[RegField])]
  ): OMRegisterMap = {
    makeRegisterMap(rawModule, baseAddress, mapping)
  }

  def convert(
    rawModule: RawModule,
    baseAddress: BigInt,
    mapping: RegField.Map*
  ): OMRegisterMap = {

    val moduleName = rawModule.name
    val baseHex = s"0x${baseAddress.toInt.toHexString}"
    val displayName = s"${moduleName}.${baseHex}"

    makeRegisterMap(rawModule, baseAddress, mapping)
  }

}