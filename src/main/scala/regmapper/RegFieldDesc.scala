// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

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

case class RegFieldDesc (
  name: String,
  desc: String,
  group: Option[String] = None,
  groupDesc: Option[String] = None,
  access: RegFieldAccessType = RegFieldAccessType.RW,
  wrType: Option[RegFieldWrType] = None,
  rdAction: Option[RegFieldRdAction] = None,
  volatile: Boolean = false,
  // TODO: testable?
  reset: Option[BigInt] = None,
  enumerations: Map[BigInt, (String, String)] = Map()
)

object RegFieldDesc {
  def reserved: RegFieldDesc = RegFieldDesc("reserved", "", access=RegFieldAccessType.R, reset=Some(0))
}

// Our descriptions are in terms of RegFields only, which is somewhat
// unusual for developers who are used to things being defined as bitfields
// within registers. The "Group" allows a string & (optional) description
// to be added which describes the conceptual "Group" the RegField belongs to.
// This can be used by downstream flows as they see fit to present the information.

object RegFieldGroup {
  def apply (name: String, desc: Option[String], regs: Seq[RegField], descFirstOnly: Boolean = true): Seq[RegField] = {
    regs.zipWithIndex.map {case (r, i) =>
      val gDesc = if ((i > 0) & descFirstOnly) None else desc
      r.desc.map { d =>
        r.copy(desc = Some(d.copy(group = Some(name), groupDesc = gDesc)) )
      }.getOrElse(r)
    }
  }
}
