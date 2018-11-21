// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMRange extends OMCompoundType {
  def base: BigInt
  def size: BigInt
}

trait OMBitRange extends OMRange

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
  resetValue: Option[Int],
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

