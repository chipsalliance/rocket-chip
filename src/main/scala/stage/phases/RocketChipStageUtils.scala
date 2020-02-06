// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import java.io.{File, FileWriter}

import Chisel.throwException
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.internal.firrtl.Circuit
import firrtl.options.Viewer.view
import firrtl.AnnotationSeq
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.subsystem.RocketTilesKey
import freechips.rocketchip.system.{DefaultTestSuites, RegressionTestSuite, TestGeneration}
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.util.{BlackBoxedROM, ROMGenerator}

import scala.collection.mutable.LinkedHashSet

trait HasRocketChipStageUtils {

  def getFullTopModuleClass(annotations: AnnotationSeq): String = {
    val rOpts = view[RocketChipOptions](annotations)
    s"${rOpts.topModulePackage.get}.${rOpts.topModuleClass.get}"
  }

  def getFullConfigClasses(annotations: AnnotationSeq): Seq[String] = {
    val rOpts = view[RocketChipOptions](annotations)
    val configClasses = rOpts.configs.get.split("_")
    configClasses.map( configClass => s"${rOpts.configsPackage.get}.${configClass}")
  }

  def getConfig(fullConfigClassNames: Seq[String]): Config = {
    new Config(fullConfigClassNames.foldRight(Parameters.empty) { case (currentName, config) =>
      val currentConfig = try {
        Class.forName(currentName).newInstance.asInstanceOf[Config]
      } catch {
        case e: java.lang.ClassNotFoundException =>
          throwException(s"""Unable to find part "$currentName" from "$fullConfigClassNames", did you misspell it?""", e)
      }
      currentConfig ++ config
    })
  }

  def getShortName(annotations: AnnotationSeq): String = {
    val rOpts = view[RocketChipOptions](annotations)
    rOpts.outputBaseName.getOrElse(s"${rOpts.configs.get}")
  }

  def getLongName(annotations: AnnotationSeq): String = {
    val rOpts = view[RocketChipOptions](annotations)
    rOpts.outputBaseName.getOrElse(s"${rOpts.topModulePackage.get}.${rOpts.configs.get}")
  }

  def enumerateROMs(circuit: Circuit): String = {
    val res = new StringBuilder
    val configs =
      circuit.components flatMap { m =>
        m.id match {
          case rom: BlackBoxedROM => Some((rom.name, ROMGenerator.lookup(rom)))
          case _ => None
        }
      }
    configs foreach { case (name, c) =>
      res append s"name ${name} depth ${c.depth} width ${c.width}\n"
    }
    res.toString
  }

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }

  /** Output software test Makefrags, which provide targets for integration testing. */
  def addTestSuites(annotations: AnnotationSeq) {
    import DefaultTestSuites._
    val params = getConfig(getFullConfigClasses(annotations)).toInstance
    val xlen = params(XLen)

    val regressionTests = LinkedHashSet(
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
    params(RocketTilesKey).headOption.map { tileParams =>
      val coreParams = tileParams.core
      val vm = coreParams.useVM
      val env = if (vm) List("p","v") else List("p")
      coreParams.fpu foreach { case cfg =>
        if (xlen == 32) {
          TestGeneration.addSuites(env.map(rv32uf))
          if (cfg.fLen >= 64)
            TestGeneration.addSuites(env.map(rv32ud))
        } else {
          TestGeneration.addSuite(rv32udBenchmarks)
          TestGeneration.addSuites(env.map(rv64uf))
          if (cfg.fLen >= 64)
            TestGeneration.addSuites(env.map(rv64ud))
        }
      }
      if (coreParams.useAtomics) {
        if (tileParams.dcache.flatMap(_.scratch).isEmpty)
          TestGeneration.addSuites(env.map(if (xlen == 64) rv64ua else rv32ua))
        else
          TestGeneration.addSuites(env.map(if (xlen == 64) rv64uaSansLRSC else rv32uaSansLRSC))
      }
      if (coreParams.useCompressed) TestGeneration.addSuites(env.map(if (xlen == 64) rv64uc else rv32uc))
      val (rvi, rvu) =
        if (xlen == 64) ((if (vm) rv64i else rv64pi), rv64u)
        else            ((if (vm) rv32i else rv32pi), rv32u)

      TestGeneration.addSuites(rvi.map(_("p")))
      TestGeneration.addSuites((if (vm) List("v") else List()).flatMap(env => rvu.map(_(env))))
      TestGeneration.addSuite(benchmarks)

      /* Filter the regression tests based on what the Rocket Chip configuration supports */
      val extensions = {
        val fd = coreParams.fpu.map {
          case cfg if cfg.fLen >= 64 => "fd"
          case _                     => "f"
        }
        val m = coreParams.mulDiv.map{ case _ => "m" }
        fd ++ m ++ Seq( if (coreParams.useRVE)        Some("e") else Some("i"),
          if (coreParams.useAtomics)    Some("a") else None,
          if (coreParams.useCompressed) Some("c") else None )
          .flatten
          .mkString("")
      }
      val re = s"""^rv$xlen[usm][$extensions].+""".r
      regressionTests.retain{
        case re() => true
        case _    => false
      }
      TestGeneration.addSuite(new RegressionTestSuite(regressionTests))
    }
  }

}
