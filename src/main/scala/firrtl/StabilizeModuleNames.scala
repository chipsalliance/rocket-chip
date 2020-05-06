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

case class ModuleNameAnnotation(
  name: String,
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
    val commonLazyModule: LazyModule => Boolean = _ match {
      case _: TLWidthWidget => true
      case _: TLBuffer => true
      case _: TLFragmenter => true
      case _: TLFIFOFixer => true
      case _ => false
    }
    Select.collectDeep(top) {
      case l: LazyModuleImpLike if commonLazyModule(l.wrapper) =>
        new ModuleNameAnnotation(l.desiredName, l.toTarget)
      case m: Queue[_] =>
        new ModuleNameAnnotation(m.desiredName, m.toTarget)
      case m: TLMonitor =>
        new ModuleNameAnnotation(m.desiredName, m.toTarget)
    }.toSeq
  }
}

class StabilizeModuleNames extends Transform
  with DependencyAPIMigration
  with PreservesAll[Transform] {
  override def prerequisites = Forms.LowForm
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = Forms.LowEmitters

  type Strategy = String => Module => String

  def pickStrategies(
    strategies: Seq[Strategy],
    originalName: String,
    modules: Set[Module]): Map[String, String] = {
    val result = strategies.foldLeft(None: Option[Map[String, String]]) {
      case (None, strategy) => StabilizeModuleNames.checkStrategy(strategy, originalName, modules)
      case (some, _) => some
    }
    result.get
  }

  def execute(state: CircuitState): CircuitState = {
    val modMap = state.circuit.modules.collect {
      case m: Module => m.name -> m
    }.toMap

    val nameMap = state.annotations.collect {
      case m: ModuleNameAnnotation => m
    }.groupBy(_.name).mapValues(_.map(a => modMap(Target.referringModule(a.target).module)).toSet)

    val strategies: Seq[Strategy] = Seq(
      StabilizeModuleNames.exactName,
      StabilizeModuleNames.ioStructureName,
      StabilizeModuleNames.contentsStructureName,
      StabilizeModuleNames.contentsName,
    )

    val nameMappings = nameMap.map { case (originalName, modules) =>
      pickStrategies(strategies, originalName, modules)
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
  final val emptyName: String = ""

  def exact(nameMappings: Map[String, String], circuit: Circuit): Circuit = {
    RenameModules(nameMappings, circuit)
  }

  def checkStrategy(
    strategy: String => Module => String,
    originalName: String,
    modules: Set[Module]): Option[Map[String, String]] = {
    val fn = strategy(originalName)
    val nameMap = modules.map(m => m.name -> fn(m)).toMap
    if (nameMap.values.toSet.size == modules.size) Some(nameMap) else None
  }

  def appendHashCode(originalName: String, hashCode: Int): String = {
    originalName + "_" + f"${hashCode}%08X"
  }

  def contentsStructureName(originalName: String)(module: Module): String = {
    val noNameModule = removeModuleNames(module)
    appendHashCode(originalName, noNameModule.hashCode)
  }

  def contentsName(originalName: String)(module: Module): String = {
    appendHashCode(originalName, removeModuleInfo(module).copy(name = emptyName).hashCode)
  }

  def exactName(originalName: String)(module: Module): String = originalName


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

  def ioStructureName(originalName: String)(module: Module): String = {
    val noNamePorts = module.ports.map(removePortNames(_))
     appendHashCode(originalName, noNamePorts.hashCode)
  }

  def removeModuleInfo(mod: Module): Module = {
    mod.copy(
      ports = mod.ports.map(removePortInfo(_)),
      body = removeStatementInfo(mod.body),
      info = NoInfo)
  }
}
