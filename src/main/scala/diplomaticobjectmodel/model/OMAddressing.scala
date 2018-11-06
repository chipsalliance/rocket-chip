// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMRange extends OMCompoundType {
  def base: Int
  def size: Int
}

trait OMBitRange extends OMRange

case class OMAddressSet(
  base: Int,
  mask: Int
) extends OMCompoundType

// Permissions are for memory regions
case class OMPermissions(
  readable: Boolean,
  writeable: Boolean,
  executable: Boolean,
  cacheable: Boolean,
  atomics: Boolean
) extends OMCompoundType



case class OMRegFieldDesc(
  name: String,
  description: String,
  group: Option[String],
  access: OMRegFieldAccessType,
  wrType: Option[OMRegFieldWrType],
  rdAction: Option[OMRegFieldRdAction],
  volatile: Boolean,
  resetValue: Option[Int]
) extends OMCompoundType

case class OMRegField (
  bitRange: OMBitRange,
  description: Option[OMRegFieldDesc]
) extends OMCompoundType

case class OMRegFieldGroup (
  name: String,
  description: Option[String]
) extends OMCompoundType

case class OMRegisterMap (
  name: String,
  description: String,
  registerFields: List[OMRegField],
  groups: List[OMRegFieldGroup]
) extends OMCompoundType

  /**
    * A device can have multiple MemoryRegions.
    * An example is the SPIFlash. There is a MemoryRegion for the control registers and the
    * the actual memory mapped flash which behaves like a memory.
    **/
case class OMMemoryRegion (
  name: String,
  description: String,
  addressSets: List[OMAddressSet],
  permissions: OMPermissions,
  registerMap: Option[OMRegisterMap]
) extends OMCompoundType

