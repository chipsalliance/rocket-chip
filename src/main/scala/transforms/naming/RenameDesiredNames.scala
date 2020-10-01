// See LICENSE.SiFive for license details.

package freechips.rocketchip.transforms.naming

import firrtl._
import firrtl.annotations.{CircuitTarget, IsModule, SingleTargetAnnotation, Target}
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.transforms.DedupModules

import freechips.rocketchip.linting.rule.DesiredNameAnnotation

import scala.collection.mutable

/** A helper to rename modules in a [[Circuit]]
  */
object RenameModules {
  def onStmt(moduleNameMap: Map[String, String])(stmt: Statement): Statement = stmt match {
    case inst: WDefInstance if moduleNameMap.contains(inst.module) => inst.copy(module = moduleNameMap(inst.module))
    case inst: DefInstance if moduleNameMap.contains(inst.module) => inst.copy(module = moduleNameMap(inst.module))
    case other => other.mapStmt(onStmt(moduleNameMap))
  }

  /** Renames the modules in a circuit given a mapping of old names to new names
    *
    * @param nameMappings mapping of old to new names
    * @param circuit the circuit to rename
    */
  def apply(nameMappings: Map[String, String], circuit: Circuit): Circuit = {
    val modules = circuit.modules.map {
      case mod: Module => mod.mapStmt(onStmt(nameMappings)).mapString(m => nameMappings.getOrElse(m, m))
      case ext: ExtModule => ext
    }
    val main = nameMappings.getOrElse(circuit.main, circuit.main)
    circuit.copy(main = main, modules = modules)
  }
}

/** Specifies the desired name to rename the module to, overriding the Module.desiredName
  */
case class OverrideDesiredNameAnnotation(
  desiredName: String,
  target: IsModule
) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): OverrideDesiredNameAnnotation = {
    this.copy(target = newTarget)
  }
}

/** Renames modules based on their overridden desired names
  *
  * Desired module name overrides are specified by
  * [[OverrideDesiredNameAnnotation]].
  */
class RenameDesiredNames extends Transform with DependencyAPIMigration {

  override def prerequisites = Seq(Dependency[DedupModules])
  override def optionalPrerequisiteOf = Seq(Dependency[VerilogEmitter])

  override def invalidates(transform: Transform) = false

  def execute(state: CircuitState): CircuitState = {
    val modMap = state.circuit.modules.collect {
      case m: Module => m.name -> m
    }.toMap

    val overrideDesiredNameAnnos = state.annotations.collect {
      case a: OverrideDesiredNameAnnotation if a.target.circuit == state.circuit.main => a
    }

    val moduleToDesiredName: mutable.Map[String, mutable.Set[String]] = mutable.Map()

    val nameMap = overrideDesiredNameAnnos.groupBy(_.desiredName).mapValues { annos =>
      annos.map(a => Target.referringModule(a.target).module).distinct.map { referringModule =>
        require(modMap.contains(referringModule), "ModuleNameAnnotations may not refer to blackboxes")
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

    val renamedDesiredNames = mutable.Set[String]()
    val nameMappings = nameMap.flatMap { case (desiredName, modules) =>
      if (modules.size == 1) {
        renamedDesiredNames += desiredName
        Some(modules.head.name -> desiredName)
      } else {
        None
      }
    }.toMap

    val finalNames = state.circuit.modules.map {
      case m: Module if nameMappings.contains(m.name) => nameMappings(m.name)
      case m: DefModule => m.name
    }
    val renameConflicts = finalNames.groupBy(identity).collect {
      case (_, conflicts) if conflicts.size > 1 => conflicts.head
    }
    require(renameConflicts.size == 0, s"desired names conflict with pre-existing module names: ${renameConflicts.mkString(", ")}")

    val circuit = RenameModules(nameMappings, state.circuit)

    val newMain = CircuitTarget(circuit.main)
    val oldMain = CircuitTarget(state.circuit.main)
    val renames = RenameMap()
    nameMappings.foreach { case (from, to) =>
      renames.record(oldMain.module(from), newMain.module(to))
    }

    // delete override annotations and rename desired name annotations for ones that were renamed
    val newAnnos = state.annotations.flatMap {
      case a: OverrideDesiredNameAnnotation if renamedDesiredNames(a.desiredName) => None
      case a: DesiredNameAnnotation if nameMappings.contains(Target.referringModule(a.target).module) =>
        Some(a.copy(desiredName = nameMappings(Target.referringModule(a.target).module)))
      case a => Some(a)
    }

    state.copy(circuit = circuit, annotations = newAnnos, renames = Some(renames))
  }
}
