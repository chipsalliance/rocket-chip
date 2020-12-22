// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import scala.util.matching.Regex

// This information is not used internally by the regmap(...) function.
// However, the author of a RegField may be the best person to provide this
// information which is likely to be needed by downstream SW and Documentation
// tools.

object RegFieldAccessType extends scala.Enumeration {
  type RegFieldAccessType = Value
  val R, W, RW = Value
}
import RegFieldAccessType._

object RegFieldWrType extends scala.Enumeration {
  type RegFieldWrType = Value
  val ONE_TO_CLEAR, ONE_TO_SET, ONE_TO_TOGGLE, ZERO_TO_CLEAR,
    ZERO_TO_SET, ZERO_TO_TOGGLE, CLEAR, SET, MODIFY = Value
}
import RegFieldWrType._

object RegFieldRdAction extends scala.Enumeration {
  type RegFieldRdAction = Value
  val CLEAR, SET, MODIFY = Value
}
import RegFieldRdAction._

// A RegFieldDesc is a documentation-only metadata about a RegField.
// This is similar to the IP-XACT concept of a Field within a
// Register. Notably, RegMapper does not have a concept
// of a Register to match the IP-XACT concept.
case class RegFieldDesc (
  // A short name of the register field, which
  // could be expected to be used in macros/diagrams
  name: String,
  // A longer documentation of what the register
  // field does.
  desc: String,
  // The "register" name that this register field can be
  // considered to be a part of, in traditional ways of
  // thinking about memory mapped control registers as being
  // fixed in size. Generally expected to be used in macros/diagrams
  group: Option[String] = None,
  // A "register" description for the above group, a longer
  // description of what the register does.
  groupDesc: Option[String] = None,
  // The general access type of the register field
  access: RegFieldAccessType = RegFieldAccessType.RW,
  // The special write type of the register field
  wrType: Option[RegFieldWrType] = None,
  // the special read side effects of the register field
  rdAction: Option[RegFieldRdAction] = None,
  // Whether this register field can change between reads without
  // being accessed again by the bus interface
  volatile: Boolean = false,
  // TODO: testable?
  // The reset value of this register, if it has one
  reset: Option[BigInt] = None,
  // Enumerated values that this register field can take
  enumerations: Map[BigInt, (String, String)] = Map(),
  // The IP-XACT concept of an addressBlock which this
  // register field can considered to be a part of, if exporting
  // IP-XACT or similar outputs. 
  addressBlock: Option[AddressBlockInfo] = None
  // TODO: registerFiles
) {
  require(RegFieldDesc.nameAcceptable(name),
    s"RegFieldDesc.name of '$name' is not of the form '${RegFieldDesc.nameRegex.toString}'")

  // We could also check for group name here, but that can have a
  // significant runtime overhead because every RegField can be
  // annotated with the group name,
  // so we put the check in the RegFieldGroup helper function instead.

}

object RegFieldDesc {
  def reserved: RegFieldDesc = RegFieldDesc("reserved", "", access=RegFieldAccessType.R, reset=Some(0))

  // This Regex is more limited than the IP-XACT standard,
  // which allows some unicode characters as well.
  val nameRegex: Regex = """^[_:A-Za-z][-._:A-Za-z0-9]*$""".r

  def nameAcceptable(name: String): Boolean = name match {
    case RegFieldDesc.nameRegex(_*) => true
    case _ => false
  }
}

// Our descriptions are in terms of RegFields only, which is somewhat
// unusual for developers who are used to things being defined as bitfields
// within registers. The "Group" allows a string & (optional) description
// to be added which describes the conceptual "Group" the RegField belongs to.
// This can be used by downstream flows as they see fit to present the information.

object RegFieldGroup {
  def apply (name: String, desc: Option[String], regs: Seq[RegField], descFirstOnly: Boolean = true): Seq[RegField] = {
    require(RegFieldDesc.nameAcceptable(name),
      s"RegFieldDesc.group of '$name' is not of the form '${RegFieldDesc.nameRegex.toString}'")
    regs.zipWithIndex.map {case (r, i) =>
      val gDesc = if ((i > 0) & descFirstOnly) None else desc
      r.desc.map { d =>
        r.copy(desc = Some(d.copy(group = Some(name), groupDesc = gDesc)) )
      }.getOrElse(r)
    }
  }
}

// The "AddressBlock" allows an optional AddressBlockInfo to be associated
// with a register field.
// This can be used by downstream flows as they see fit to present the
// information. This is generally designed to match the IP-XACT
// concept of an AddressBlock.

case class AddressBlockInfo (
  // The short name of the address block
  name: String,
  addressOffset: BigInt, // Offset of the address block (in bytes) from the MemoryMap base / base of the Register Router node.
                         // This is generally NOT an absoluate address.
                         // Note this is NOT expected to be considered as part of RegField's offset:
                         // adding an AddressBlockInfo should not change that value in the RegField serialized to JSON or OM, for example.
  range: BigInt, // Size of the address block (in bytes)
  width: Int     // assumed access size of registers in this block (e.g. 32 or 64 bits).
                 // Again this is ONLY documentation. RegField hardware generation ignores this.
);

// Add the AddressBlock to a list of RegFields' descriptions. If they have no RegFieldDesc,
// this has no effect.
object RegFieldAddressBlock {

  def apply(addressBlockInfo: AddressBlockInfo,
    addAddressOffset: Boolean,
    regmap: RegField.Map*): Seq[RegField.Map] = {
    regmap.toList.map { regmapEntry =>
      // each entry has a form like offset -> Seq[RegField]
      // We either add the addressBlockInfo.addressOffset or not
      // and also update each of the child RegField's descriptions
      val regFields = regmapEntry._2
      val regFieldsWithAddressBlockAdded = regFields.map {r =>
        r.desc.map { d =>
          r.copy(desc = Some(d.copy(addressBlock = Some(addressBlockInfo))))
        }.getOrElse(r)
      }
      val offsetIncrement = if (addAddressOffset) addressBlockInfo.addressOffset.toInt else 0
      (regmapEntry._1 + offsetIncrement) -> regFieldsWithAddressBlockAdded
    }
  }
}

// TODO: RegisterFiles
