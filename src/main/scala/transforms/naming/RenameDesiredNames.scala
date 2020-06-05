// See LICENSE.SiFive for license details.

package freechips.rocketchip.transforms.naming

import firrtl._
import firrtl.annotations.{CircuitTarget, HasSerializationHints, IsModule, SingleTargetAnnotation, Target}
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

/** A strategy for generating new names for modules
  */
sealed trait NamingStrategy {

  /** Generates a new stable module name based on the desired name and the module IR node
    *
    * called by [[RenameDesiredNames]] to rename modules
    *
    * @param desiredName the requested name for the module; generated name should contain this name
    * @param module the module targeted by the desiredName
    */
  def getName(desiredName: String)(module: Module): String
}

/** A naming strategy that always returns the desired name
  */
case object ExactNamingStrategy extends NamingStrategy {
  def getName(desiredName: String)(module: Module): String = desiredName
}

/** Specifies a naming strategy to use for a module and the modules that it collides with
  */
case class NamingStrategyAnnotation(
  strategy: NamingStrategy,
  target: IsModule
) extends SingleTargetAnnotation[IsModule] with HasSerializationHints {
  def typeHints: Seq[Class[_]] = Seq(
    ExactNamingStrategy.getClass
  )

  def duplicate(newTarget: IsModule): NamingStrategyAnnotation = {
    this.copy(target = newTarget)
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
  * [[OverrideDesiredNameAnnotation]]. If one of the modules is targeted by a
  * [[NamingStrategyAnnotation]] then the transform will attempt to rename
  * modules according to that naming strategy. The default naming strategy is
  * [[ExactNamingStrategy]].
  */
class RenameDesiredNames extends Transform with DependencyAPIMigration {

  override def prerequisites = Seq(Dependency[DedupModules])
  override def optionalPrerequisiteOf = Seq(Dependency[VerilogEmitter])

  override def invalidates(transform: Transform) = false

  /* Creates a rename mapping using the selected [[NamingStrategy]]
   *
   * @return a map of original module name to new name if possible, None if the
   * strategy fails to result in unique names for all the modules using this
   * strategy
   */
  private def checkStrategy(
    strategy: NamingStrategy,
    desiredName: String,
    modules: Seq[Module]): Option[Map[String, String]] = {
    val fn = strategy.getName(desiredName)(_)
    val nameMap = modules.map(m => m.name -> fn(m)).toMap
    if (nameMap.values.toSet.size == modules.size) Some(nameMap) else None
  }

  def execute(state: CircuitState): CircuitState = {
    val modMap = state.circuit.modules.collect {
      case m: Module => m.name -> m
    }.toMap

    val namingStrategyAnnos = state.annotations.collect {
      case a: NamingStrategyAnnotation if a.target.circuit == state.circuit.main => a
    }

    val strategyMap = namingStrategyAnnos.groupBy { a =>
      val referringModule = Target.referringModule(a.target).module
      require(modMap.contains(referringModule), "NamingStrategyAnnotation may not refer to blackboxes")
      referringModule
    }.map { case (module, annos) =>
      val strategies = annos.map(_.strategy).distinct
      require(strategies.size == 1, s"Conflicting naming strategies for module $module: ${strategies.mkString(", ")}")
      module -> strategies.head
    }

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
      val strategyOpt = modules.collectFirst {
        case m if strategyMap.contains(m.name) => strategyMap(m.name)
      }
      val strategy = strategyOpt.getOrElse(ExactNamingStrategy)
      val result = checkStrategy(strategy, desiredName, modules)
      if (result.isDefined) {
        renamedDesiredNames += desiredName
      }
      result
    }.flatten.toMap

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
