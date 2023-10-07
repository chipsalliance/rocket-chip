// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases


import org.chipsalliance.cde.config.Parameters
import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{Dependency, Phase, PreservesAll, Unserializable}
import firrtl.options.Viewer.view
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.system.{DefaultTestSuites, RegressionTestSuite, RocketTestSuite}
import freechips.rocketchip.subsystem.{TilesLocated, InSubsystem, RocketTileAttachParams}
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.util.HasRocketChipStageUtils
import freechips.rocketchip.system.DefaultTestSuites._

import scala.collection.mutable

/** Annotation that contains a list of [[RocketTestSuite]]s to run */
case class RocketTestSuiteAnnotation(tests: Seq[RocketTestSuite]) extends NoTargetAnnotation with Unserializable

/** Generates [[RocketTestSuiteAnnotation]] depending on whether the top-module project is part of
 *  [[freechips.rocketchip.system]] or not (e.g. for unit tests).
 */
class AddDefaultTests extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(Dependency[freechips.rocketchip.system.RocketChiselStage])
  override val dependents = Seq(Dependency[GenerateTestSuiteMakefrags])

  def GenerateDefaultTestSuites(): List[RocketTestSuite] = {
    List(DefaultTestSuites.groundtest64("p"), DefaultTestSuites.emptyBmarks, DefaultTestSuites.singleRegression)
  }

  def GenerateSystemTestSuites(annotations: AnnotationSeq): scala.collection.mutable.Buffer[RocketTestSuite] = {
    val params: Parameters = getConfig(view[RocketChipOptions](annotations).configNames.get).toInstance
    val xlen = params(XLen)
    val tests = scala.collection.mutable.Buffer[RocketTestSuite]()

    val regressionTests = mutable.LinkedHashSet(
      "rv64ud-v-fcvt",
      "rv64ud-p-fdiv",
      "rv64ud-v-fadd",
      "rv64uf-v-fadd",
      "rv64um-v-mul",
      "rv64mi-p-breakpoint",
      "rv64uc-v-rvc",
      "rv64ud-v-structural",
      "rv64si-p-wfi",
      "rv64um-v-divw",
      "rv64ua-v-lrsc",
      "rv64ui-v-fence_i",
      "rv64ud-v-fcvt_w",
      "rv64uf-v-fmin",
      "rv64ui-v-sb",
      "rv64ua-v-amomax_d",
      "rv64ud-v-move",
      "rv64ud-v-fclass",
      "rv64ua-v-amoand_d",
      "rv64ua-v-amoxor_d",
      "rv64si-p-sbreak",
      "rv64ud-v-fmadd",
      "rv64uf-v-ldst",
      "rv64um-v-mulh",
      "rv64si-p-dirty",
      "rv32mi-p-ma_addr",
      "rv32mi-p-csr",
      "rv32ui-p-sh",
      "rv32ui-p-lh",
      "rv32uc-p-rvc",
      "rv32mi-p-sbreak",
      "rv32ui-p-sll")

    // TODO: for now only generate tests for the first core in the first subsystem
    val rocketTileParams = params(TilesLocated(InSubsystem)).collect { case n: RocketTileAttachParams => n }.map(_.tileParams)
    rocketTileParams.headOption.map { tileParams =>
      val coreParams = tileParams.core
      val vm = coreParams.useVM
      val env = if (vm) List("p", "v") else List("p")
      coreParams.fpu foreach { case cfg =>
        if (xlen == 32) {
          tests ++= env.map(rv32uf)
          if (cfg.fLen >= 64)
            tests ++= env.map(rv32ud)
          if (cfg.minFLen <= 16)
            tests ++= env.map(rv32uzfh)
        } else {
          tests += rv32udBenchmarks
          tests ++= env.map(rv64uf)
          if (cfg.fLen >= 64)
            tests ++= env.map(rv64ud)
          if (cfg.minFLen <= 16)
            tests ++= env.map(rv64uzfh)
        }
      }
      if (coreParams.useAtomics) {
        if (tileParams.dcache.flatMap(_.scratch).isEmpty)
          tests ++= env.map(if (xlen == 64) rv64ua else rv32ua)
        else
          tests ++= env.map(if (xlen == 64) rv64uaSansLRSC else rv32uaSansLRSC)
      }
      if (coreParams.useCompressed) tests ++= env.map(if (xlen == 64) rv64uc else rv32uc)
      val (rvi, rvu) =
        if (xlen == 64) ((if (vm) rv64i else rv64pi), rv64u)
        else ((if (vm) rv32i else rv32pi), rv32u)

      tests ++= rvi.map(_ ("p"))
      tests ++= (if (vm) List("v") else List()).flatMap(env => rvu.map(_ (env)))
      tests += benchmarks

      /* Filter the regression tests based on what the Rocket Chip configuration supports */
      val extensions = {
        val fd = coreParams.fpu.map {
          case cfg if cfg.fLen >= 64 => "fd"
          case _ => "f"
        }
        val m = coreParams.mulDiv.map { case _ => "m" }
        fd ++ m ++ Seq(if (coreParams.useRVE) Some("e") else Some("i"),
          if (coreParams.useAtomics) Some("a") else None,
          if (coreParams.useCompressed) Some("c") else None)
          .flatten
          .mkString("")
      }
      val re = s"""^rv$xlen[usm][$extensions].+""".r
      regressionTests.retain {
        case re() => true
        case _ => false
      }
      tests += new RegressionTestSuite(regressionTests)
    }
    tests
  }

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val ropts = view[RocketChipOptions](annotations)
    val tests = ropts.topPackage.get match {
      case "freechips.rocketchip.system" => GenerateSystemTestSuites(annotations)
      case _ => GenerateDefaultTestSuites()
    }

    RocketTestSuiteAnnotation(tests.toSeq) +: annotations
  }

}
