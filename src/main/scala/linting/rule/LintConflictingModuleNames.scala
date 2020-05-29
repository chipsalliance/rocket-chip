// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting
package rule

import firrtl._
import firrtl.annotations.{CircuitTarget, HasSerializationHints, IsModule, SingleTargetAnnotation, Target}
import firrtl.ir._
import firrtl.options.{Dependency, HasShellOptions, PreservesAll, ShellOption}
import firrtl.stage.{Forms, RunFirrtlTransformAnnotation}
import firrtl.transforms.DedupModules

import chisel3.aop.{Aspect, Select}
import chisel3.RawModule
import chisel3.util.Queue

import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImpLike}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.system.TestHarness

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

/** A strategy for generating a new names for modules
  */
sealed trait NamingStrategy {

  /** Generates a new stable module name based on the desired name and the module IR node
    *
    * called by [[LintConflictingModuleNames]] to rename modules
    *
    * @param desiredName the requested name for the module, generated name should contain this name
    * @param module the module targeted by the desiredName
    */
  def getName(desiredName: String)(module: Module): String
}

/** A naming strategy that always returns the desired name
  */
case object ExactNamingStrategy extends NamingStrategy {
  def getName(desiredName: String)(module: Module): String = desiredName
}

/** A naming strategy that appends the port structure hash to the desired name i.e. "{desiredName}_p{hash}"
  */
case object PortStructureNamingStrategy extends NamingStrategy {
  def getName(desiredName: String)(module: Module): String = {
    val noNamePorts = module.ports.map(NamingStrategy.removePortNames(_))
     NamingStrategy.appendHashCode(desiredName, "p", noNamePorts.hashCode)
  }
}

/** A naming strategy that appends the name-agnostic module content hash to the desired name i.e. "{desiredName}_c{hash}"
  */
case object ContentStructureNamingStrategy extends NamingStrategy {
  def getName(desiredName: String)(module: Module): String = {
    val noNameModule = NamingStrategy.removeModuleNames(module)
    NamingStrategy.appendHashCode(desiredName, "c", noNameModule.hashCode)
  }
}

/** A naming strategy that appends the module content hash to the desired name i.e. "{desiredName}_C{hash}"
  */
case object ContentNamingStrategy extends NamingStrategy {
  def getName(desiredName: String)(module: Module): String = {
    val noInfoModule = NamingStrategy.removeModuleInfo(module).copy(name = NamingStrategy.emptyName)
    NamingStrategy.appendHashCode(desiredName, "C", noInfoModule.hashCode)
  }
}

object NamingStrategy {

  final val emptyName: String = ""

  def appendHashCode(desiredName: String, hashPrefix: String, hashCode: Int): String = {
    s"${desiredName}_$hashPrefix" + f"${hashCode}%08X"
  }


  // remove name helpers

  def removeTypeNames(tpe: Type): Type = tpe match {
    case g: GroundType => g
    case v: VectorType => v
    case b: BundleType => b.copy(fields = b.fields.map { field =>
      field.copy(name = emptyName)
    })
  }

  def removeStatementNames(stmt: Statement): Statement = stmt match {
    case i: DefInstance => i.copy(
      name = emptyName,
      module = emptyName,
      info = NoInfo)
    case i: WDefInstance => i.copy(
      name = emptyName,
      module = emptyName,
      tpe = removeTypeNames(i.tpe),
      info = NoInfo)
    case _ => stmt
      .mapStmt(removeStatementNames)
      .mapString(_ => emptyName)
      .mapInfo(_ => NoInfo)
      .mapType(removeTypeNames)
  }

  def removePortNames(port: Port): Port = {
    port.copy(name = emptyName, tpe = removeTypeNames(port.tpe), info = NoInfo)
  }

  def removeModuleNames(mod: Module): Module = {
    mod.copy(
      ports = mod.ports.map(removePortNames(_)),
      body = removeStatementNames(mod.body),
      name = emptyName,
      info = NoInfo)
  }

  // remove Info helpers

  def removeStatementInfo(stmt: Statement): Statement = {
    stmt.mapStmt(removeStatementInfo).mapInfo(_ => NoInfo)
  }

  def removePortInfo(port: Port): Port = {
    port.copy(info = NoInfo)
  }

  def removeModuleInfo(mod: Module): Module = {
    mod.copy(
      ports = mod.ports.map(removePortInfo(_)),
      body = removeStatementInfo(mod.body),
      info = NoInfo)
  }
}

/** Specifies a naming strategy to use for a module and the modules that it collides with
  */
case class NamingStrategyAnnotation(
  strategy: NamingStrategy,
  target: IsModule
) extends SingleTargetAnnotation[IsModule] with HasSerializationHints {
  def typeHints: Seq[Class[_]] = Seq(
    ExactNamingStrategy.getClass,
    PortStructureNamingStrategy.getClass,
    ContentStructureNamingStrategy.getClass,
    ContentNamingStrategy.getClass
  )

  def duplicate(newTarget: IsModule): NamingStrategyAnnotation = {
    this.copy(target = newTarget)
  }
}

/** Specifies the original desired name for a module
  */
case class DesiredNameAnnotation(
  desiredName: String,
  target: IsModule
) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): DesiredNameAnnotation = {
    this.copy(target = newTarget)
  }
}

/** Specifies the desired name to rename the module to
  */
case class OverrideDesiredNameAnnotation(
  desiredName: String,
  target: IsModule
) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): OverrideDesiredNameAnnotation = {
    this.copy(target = newTarget)
  }
}

/** Collects [[DesireNameAnnotaion]]s for [[LintConflictingModuleNames]] to lint
  */
case object LintConflictingModuleNamesAspect extends Aspect[RawModule] {
  def toAnnotation(top: RawModule): AnnotationSeq = {
    Select.collectDeep(top) {
      case m: chisel3.Module => DesiredNameAnnotation(m.desiredName, m.toTarget)
    }.toSeq
  }
}

/** An example of how to customize the names of certain modules using an Aspect
  */
case object StabilizeNamesAspect extends Aspect[RawModule] {
  def toAnnotation(top: RawModule): AnnotationSeq = {
    RunFirrtlTransformAnnotation(new RenameDesiredNames) +:
    Select.collectDeep(top) {
      // annotating all Queues with a more descriptive desired name
      case m: Queue[_] => Seq(
        new OverrideDesiredNameAnnotation(s"Queue_${m.genType.getClass.getSimpleName}_entries_${m.entries}", m.toTarget),
        new NamingStrategyAnnotation(ContentStructureNamingStrategy, m.toTarget)
      )
      case m: TLMonitor => Seq(
        new OverrideDesiredNameAnnotation(m.desiredName, m.toTarget),
        new NamingStrategyAnnotation(ContentStructureNamingStrategy, m.toTarget)
      )

      // annotating specific instances
      case th: TestHarness =>
        val core = th.ldut.rocketTiles.head.module.core
        Seq(
          OverrideDesiredNameAnnotation("Rocket_Core_0", core.toTarget),
          NamingStrategyAnnotation(ExactNamingStrategy, core.toTarget),
        )
    }.flatten.toSeq
  }
}

/** This LintRule checks for module name collisions
  *
  * Module name collisions occur when different [[Module]]s are annotated with
  * [[DesiredNameAnnotation]]s that have the same desiredName. By default all
  * module name conflicts will cause a [[LintViolation]]. If one of the modules
  * is targeted by [[NamingStrategyAnnotation]] then the transform will attempt
  * to rename modules according to that naming strategy. If the conflicting
  * modules cannot be disambiguated by the naming strategy then a
  * [[LintViolation]] is emitted.
  */
final class LintConflictingModuleNames extends LintRule {
  val recommendedFix: String = "override desiredName based on module parameters"

  val lintName: String = "conflicting-module-names"

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

/** Renames modules based on their overrided desired names
  *
  * Desired module name overrides are specified by
  * [[OverrideDesiredNameAnnotation]]. If one of the modules is targeted by a
  * [[NamingStrategyAnnotation]] then the transform will attempt to rename
  * modules according to that naming strategy. If the conflicting modules
  * cannot be disambiguated by the naming strategy then a [[LintViolation]] is
  * emitted. The default naming strategy is [[ExactNamingStrategy]] which will
  * throw an exception if there are any module name conflicts.
  */
class RenameDesiredNames extends Transform with DependencyAPIMigration {

  override def prerequisites = Seq(Dependency[DedupModules])
  override def optionalPrerequisiteOf = Seq(Dependency[VerilogEmitter])

  override def invalidates(transform: Transform) = false

  private def checkStrategy(
    strategy: NamingStrategy,
    desiredName: String,
    modules: Seq[Module]): Option[Map[String, String]] = {
    val fn = strategy.getName(desiredName)(_)
    val nameMap = modules.map(m => m.name -> fn(m)).toMap
    if (nameMap.values.toSet.size == modules.size) Some(nameMap) else None
  }

  def execute(state: CircuitState): CircuitState = {
    val violations = new Violations()

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

    val circuit = RenameModules(nameMappings, state.circuit)

    val newMain = CircuitTarget(circuit.main)
    val oldMain = CircuitTarget(state.circuit.main)
    val renames = RenameMap()

    // delete override annotations and rename desired name annotaions for ones that were renamed
    val newAnnos = state.annotations.flatMap {
      case a: OverrideDesiredNameAnnotation if renamedDesiredNames(a.desiredName) => None
      case a: DesiredNameAnnotation if nameMappings.contains(a.desiredName) =>
        Some(a.copy(desiredName = nameMappings(a.desiredName)))
      case a => Some(a)
    }

    state.copy(circuit = circuit, annotations = newAnnos, renames = Some(renames))
  }
}
