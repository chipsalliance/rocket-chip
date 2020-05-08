// See LICENSE for license details.

package freechips.rocketchip.firrtl

import firrtl._
import firrtl.ir._
import firrtl.annotations.{Target, SingleTargetAnnotation, IsModule, CircuitTarget}
import firrtl.options.{HasShellOptions, PreservesAll, ShellOption}
import firrtl.stage.Forms

import chisel3.util.Queue
import chisel3.aop.{Aspect, Select}
import chisel3.RawModule

import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImpLike}

object RenameModules {
  def onStmt(moduleNameMap: Map[String, String])(stmt: Statement): Statement = stmt match {
    case inst: WDefInstance if moduleNameMap.contains(inst.module) => inst.copy(module = moduleNameMap(inst.module))
    case other => other.mapStmt(onStmt(moduleNameMap))
  }

  def apply(nameMappings: Map[String, String], circuit: Circuit): Circuit = {
    val modules = circuit.modules.map {
      case mod: Module => mod.mapStmt(onStmt(nameMappings)).mapString(m => nameMappings.getOrElse(m, m))
      case ext: ExtModule => ext
    }
    val main = nameMappings.getOrElse(circuit.main, circuit.main)
    circuit.copy(main = main, modules = modules)
  }
}

sealed trait NamingStrategy

// only except the desired name
case object ExactNamingStrategy extends NamingStrategy

// use port structure hash naming strategy
case object PortStructureNamingStrategy extends NamingStrategy

// use name-agnostic module content hash naming strategy
case object ContentStructureNamingStrategy extends NamingStrategy

// use module content hash naming strategy
case object ContentNamingStrategy extends NamingStrategy


case class NamingStrategyAnnotation(
  strategy: NamingStrategy,
  target: IsModule
)

case class ModuleNameAnnotation(
  desiredName: String,
  target: IsModule
) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): ModuleNameAnnotation = {
    this.copy(target = newTarget)
  }
}

case object StabilizeNamesAspect extends Aspect[RawModule] with HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "stabilize-names",
      toAnnotationSeq = a => Seq(this),
      helpText = "<stabilize names>",
      shortOption = None
    )
  )

  def toAnnotation(top: RawModule): AnnotationSeq = {
    Select.collectDeep(top) {
      case m: LazyModuleImpLike => new ModuleNameAnnotation(m.desiredName, m.toTarget)
      case m: Queue[_] => new ModuleNameAnnotation(m.desiredName, m.toTarget)
      case m: TLMonitor => new ModuleNameAnnotation(m.desiredName, m.toTarget)
    }.toSeq
  }
}

class StabilizeModuleNames extends Transform
  with DependencyAPIMigration
  with PreservesAll[Transform] {
  override def prerequisites = Forms.LowForm
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = Forms.LowEmitters

  import StabilizeModuleNames.Strategy

  private def pickStrategies(
    strategies: Seq[Strategy],
    desiredName: String,
    modules: Seq[Module]): Map[String, String] = {
    val result = strategies.foldLeft(None: Option[Map[String, String]]) {
      case (None, strategy) => StabilizeModuleNames.checkStrategy(strategy, desiredName, modules)
      case (some, _) => some
    }
    require(result.isDefined, s"No naming strategy disambiguates modules for desired name: $desiredName")
    result.get
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

    val moduleNameAnnos = state.annotations.collect {
      case a: ModuleNameAnnotation if a.target.circuit == state.circuit.main => a
    }

    val nameMap = moduleNameAnnos.groupBy(_.desiredName).mapValues { annos =>
      annos.distinct.map { a =>
        val referringModule = Target.referringModule(a.target).module
        require(modMap.contains(referringModule), "ModuleNameAnnotations may not refer to blackboxes")
        modMap(referringModule)
      }
    }

    val strategies: Seq[Strategy] = Seq(
      StabilizeModuleNames.exactName,
      StabilizeModuleNames.portStructureName,
      StabilizeModuleNames.contentsStructureName,
      StabilizeModuleNames.contentsName,
    )

    val nameMappings = nameMap.map { case (desiredName, modules) =>
      val strategyOpt = modules.collectFirst {
        case m if strategyMap.contains(m.name) => strategyMap(m.name)
      }
      if (strategyOpt.isDefined) {
        val strategy = strategyOpt.get match {
          case ExactNamingStrategy => StabilizeModuleNames.exactName
          case PortStructureNamingStrategy => StabilizeModuleNames.portStructureName
          case ContentStructureNamingStrategy => StabilizeModuleNames.contentsStructureName
          case ContentNamingStrategy => StabilizeModuleNames.contentsName
        }

        val result = StabilizeModuleNames.checkStrategy(strategy, desiredName, modules)
        require(result.isDefined, s"Requested naming strategy $strategy does not disambiguate module collisions for desired name: $desiredName")
        result.get
      } else {
        pickStrategies(strategies, desiredName, modules)
      }
    }.flatten.toMap

    val circuit = RenameModules(nameMappings, state.circuit)

    val newMain = CircuitTarget(circuit.main)
    val oldMain = CircuitTarget(state.circuit.main)
    val renames = RenameMap()
    nameMappings.foreach { case (from, to) =>
      renames.record(oldMain.module(from), newMain.module(to))
    }
    state.copy(circuit = circuit, renames = Some(renames))
  }
}

object StabilizeModuleNames {
  type Strategy = String => Module => String

  final val emptyName: String = ""

  def exact(nameMappings: Map[String, String], circuit: Circuit): Circuit = {
    RenameModules(nameMappings, circuit)
  }

  def checkStrategy(
    strategy: String => Module => String,
    desiredName: String,
    modules: Seq[Module]): Option[Map[String, String]] = {
    val fn = strategy(desiredName)
    val nameMap = modules.map(m => m.name -> fn(m)).toMap
    if (nameMap.values.toSet.size == modules.size) Some(nameMap) else None
  }

  def appendHashCode(desiredName: String, hashPrefix: String, hashCode: Int): String = {
    s"${desiredName}_$hashPrefix" + f"${hashCode}%08X"
  }

  val contentsStructureName: Strategy = (desiredName: String) => (module: Module) => {
    val noNameModule = removeModuleNames(module)
    appendHashCode(desiredName, "c", noNameModule.hashCode)
  }

  val contentsName: Strategy = (desiredName: String) => (module: Module) => {
    appendHashCode(desiredName, "C", removeModuleInfo(module).copy(name = emptyName).hashCode)
  }

  val exactName: Strategy = (desiredName: String) => (module: Module) => desiredName


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

  val portStructureName: Strategy = (desiredName: String) => (module: Module) => {
    val noNamePorts = module.ports.map(removePortNames(_))
     appendHashCode(desiredName, "p", noNamePorts.hashCode)
  }

  def removeModuleInfo(mod: Module): Module = {
    mod.copy(
      ports = mod.ports.map(removePortInfo(_)),
      body = removeStatementInfo(mod.body),
      info = NoInfo)
  }
}
