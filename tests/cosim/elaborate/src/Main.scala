package tests.cosim.elabotate


import chisel3.aop.Select
import chisel3.aop.injecting.InjectingAspect
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{CIRCTTarget, CIRCTTargetAnnotation, ChiselStage, FirtoolOption}
import firrtl.options.TargetDirAnnotation
import firrtl.{AnnotationSeq, ChirrtlEmitter, EmitAllModulesAnnotation}
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.diplomacy.MonitorsEnabled
import freechips.rocketchip.subsystem.{CacheBlockBytes, SystemBusKey, SystemBusParams}
import mainargs._
import org.chipsalliance.cde.config.{Config, Field}
import freechips.rocketchip.rocket.{DCacheParams, FrontendModule, ICacheModule, ICacheParams, MulDivParams, Rocket, RocketCoreParams}
import freechips.rocketchip.tile.RocketTileParams

import freechips.rocketchip.system._
import chisel3.stage.{ChiselCli, ChiselStage}
import firrtl.AnnotationSeq
import firrtl.options.PhaseManager.PhaseDependency
import firrtl.options.{Dependency, Phase, PhaseManager, PreservesAll, Shell, Stage, StageMain}
import firrtl.stage.FirrtlCli
import freechips.rocketchip.stage.RocketChipCli

import scala.collection.immutable.Seq

/** Modified ChiselStage that includes the GenerateROMs phase */
private[freechips] final class RocketChiselStage extends ChiselStage {

  override val targets = Seq(
    Dependency[chisel3.stage.phases.Checks],
    Dependency[chisel3.stage.phases.Elaborate],
    Dependency[freechips.rocketchip.stage.phases.GenerateROMs],
    Dependency[chisel3.stage.phases.AddImplicitOutputFile],
    Dependency[chisel3.stage.phases.AddImplicitOutputAnnotationFile],
    Dependency[chisel3.stage.phases.MaybeAspectPhase],
    Dependency[chisel3.stage.phases.Emitter],
    Dependency[chisel3.stage.phases.Convert]
  )

}

class RocketChipStage extends Stage with PreservesAll[Phase] {

  override val shell = new Shell("rocket-chip") with RocketChipCli with ChiselCli with FirrtlCli
  val targets: Seq[PhaseDependency] = Seq(
    Dependency[freechips.rocketchip.stage.phases.Checks],
    Dependency[freechips.rocketchip.stage.phases.TransformAnnotations],
    Dependency[freechips.rocketchip.stage.phases.PreElaboration],
    Dependency[RocketChiselStage],
    Dependency[freechips.rocketchip.stage.phases.GenerateFirrtlAnnos],
    Dependency[freechips.rocketchip.stage.phases.AddDefaultTests],
    Dependency[freechips.rocketchip.stage.phases.GenerateTestSuiteMakefrags],
    Dependency[freechips.rocketchip.stage.phases.GenerateArtefacts]
  )

  private val pm = new PhaseManager(targets)

  override def run(annotations: AnnotationSeq): AnnotationSeq = pm.transform(annotations)

}

object Generator extends StageMain(new RocketChipStage)


object RocketTileParamsKey extends Field[RocketTileParams]

class cosimConfig extends Config((site, here, up) => {
  case MonitorsEnabled => false
  case freechips.rocketchip.tile.XLen => 64
  case freechips.rocketchip.tile.XLen => 64
  case freechips.rocketchip.tile.MaxHartIdBits => 4
  case freechips.rocketchip.tile.MaxHartIdBits => 4
  case freechips.rocketchip.rocket.PgLevels => if (site(freechips.rocketchip.tile.XLen) == 64) 3 else 2
  case freechips.rocketchip.rocket.PgLevels => if (site(freechips.rocketchip.tile.XLen) == 64) 3 else 2
  case RocketTileParamsKey => RocketTileParams(
    core = RocketCoreParams(mulDiv = Some(MulDivParams(
      mulUnroll = 8,
      mulEarlyOut = true,
      divEarlyOut = true))),
    dcache = Some(DCacheParams(
      rowBits = site(SystemBusKey).beatBits,
      nMSHRs = 0,
      blockBytes = site(CacheBlockBytes))),
    icache = Some(ICacheParams(
      rowBits = site(SystemBusKey).beatBits,
      blockBytes = site(CacheBlockBytes))))
  case SystemBusKey => SystemBusParams(
    beatBytes = site(freechips.rocketchip.tile.XLen) / 8,
    blockBytes = site(CacheBlockBytes))
  case DebugModuleKey => None
})

