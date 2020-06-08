// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting
package rule

import firrtl._
import firrtl.annotations.{IsModule, SingleTargetAnnotation, Target}
import firrtl.ir._
import firrtl.options.Dependency

import freechips.rocketchip.transforms.naming.RenameDesiredNames

import chisel3.aop.{Aspect, Select}
import chisel3.RawModule

import scala.collection.mutable

/** Captures the original desired name for a module
  */
case class DesiredNameAnnotation(
  desiredName: String,
  target: IsModule
) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): DesiredNameAnnotation = {
    this.copy(target = newTarget)
  }
}

/** Collects [[DesiredNameAnnotation]]s for [[LintConflictingModuleNames]] to lint
  */
case object LintConflictingModuleNamesAspect extends Aspect[RawModule] {
  def toAnnotation(top: RawModule): AnnotationSeq = {
    Select.collectDeep(top) {
      case m: RawModule => DesiredNameAnnotation(m.desiredName, m.toTarget)
    }.toSeq
  }
}

/** This LintRule checks for module name collisions
  *
  * Module name collisions occur when different [[Module]]s are annotated with
  * [[DesiredNameAnnotation]]s that have the same desiredName. Module name
  * conflicts will cause a [[LintViolation]].
  */
final class LintConflictingModuleNames extends LintRule {
  val recommendedFix: String = "override desiredName based on module parameters ('override def desiredName = \"...\"') or use RenameModulesAspect"

  val lintName: String = "conflicting-module-names"

  // depends on DedupModules which comes from super[LintRule].optionalPrerequisites
  override def optionalPrerequisites: Seq[Dependency[Transform]] =
    Dependency[RenameDesiredNames] +: super.optionalPrerequisites

  override def execute(state: CircuitState): CircuitState = {
    val violations = new Violations()

    val modMap = state.circuit.modules.collect {
      case m: Module => m.name -> m
    }.toMap

    val desiredNameAnnos = state.annotations.collect {
      case a: DesiredNameAnnotation if a.target.circuit == state.circuit.main => a
    }

    val moduleToDesiredName: mutable.Map[String, mutable.Set[String]] = mutable.Map()

    val nameMap = desiredNameAnnos.groupBy(_.desiredName).mapValues { annos =>
      annos.map(a => Target.referringModule(a.target).module).distinct.map { referringModule =>
        require(modMap.contains(referringModule), s"ModuleNameAnnotations may not refer to blackboxes: $referringModule")
        val desiredNames = moduleToDesiredName.getOrElseUpdate(referringModule, mutable.Set())
        desiredNames += annos.head.desiredName
        modMap(referringModule)
      }
    }

    val conflictingDesiredNames = moduleToDesiredName.collect {
      case kv@ (moduleName, desiredName) if desiredName.size > 1 => kv
    }

    require(conflictingDesiredNames.size == 0, {
      val explanation = conflictingDesiredNames.map {
        case (modName, desiredNames) => s"  ${modName}: ${desiredNames.mkString(", ")}"
      }.mkString("\n")
      s"Modules may not have more than one desiredName:\n${explanation}"
    })

    nameMap.foreach {
      case (desiredName, modules) if modules.size > 1 =>
        val msg = s"Module conflicts for desired name $desiredName: ${modules.map(_.name).mkString(", ")}"
        val info = MultiInfo(modules.map(_.info))
        val mods = violations.getOrElse((info, msg), Set.empty)
        violations((info, msg)) = mods ++ modules.map(_.name)
      case _ =>
    }

    val whitelist = collectWhitelist(state.annotations)
    val errorList = violations.collect {
      case ((info, message), mods) if !isWhitelisted(info, whitelist) => Violation(this, info, message, mods)
    }.toSeq.sortBy { _.toString }
    val newAnnos = errorList ++ state.annotations

    state.copy(annotations = newAnnos)
  }
}
