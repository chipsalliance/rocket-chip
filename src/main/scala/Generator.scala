// See LICENSE for license details.

package rocketchip

import Chisel._
import scala.collection.mutable.{LinkedHashSet,LinkedHashMap}
import cde.{Parameters, ParameterDump, Config, Field, CDEMatchError, World}
import coreplex._

object GeneratorUtils {
  def getConfig(projectName: String, configClassName: String): Config = {
    val aggregateConfigs = configClassName.split('_')

    aggregateConfigs.foldRight(new Config()) { case (currentConfigName, finalConfig) =>
      val currentConfig = try {
        Class.forName(s"$projectName.$currentConfigName").newInstance.asInstanceOf[Config]
      } catch {
        case e: java.lang.ClassNotFoundException =>
          throwException("Unable to find part \"" + currentConfigName +
            "\" of configClassName \"" + configClassName +
            "\", did you misspell it?", e)
      }
      currentConfig ++ finalConfig
    }
  }

  def getParameters(config: Config): Parameters =
    Parameters.root(config.toInstance)

  def getParameters(projectName: String, configClassName: String): Parameters =
    getParameters(getConfig(projectName, configClassName))

  def elaborate(fullName: String, args: Array[String], params: Parameters) {
    val gen = () =>
      Class.forName(fullName)
        .getConstructor(classOf[cde.Parameters])
        .newInstance(params)
        .asInstanceOf[Module]

    chiselMain.run(args, gen)
  }

  def dumpParameters(fname: String) {
    val pdFile = TestGeneration.createOutputFile(fname)
    pdFile.write(ParameterDump.getDump)
    pdFile.close
  }

  def dumpKnobs(configClassName: String, world: World) {
    val knbFile = TestGeneration.createOutputFile(configClassName + ".knb")
    knbFile.write(world.getKnobs)
    knbFile.close

    val cstFile = TestGeneration.createOutputFile(configClassName + ".cst")
    cstFile.write(world.getConstraints)
    cstFile.close
  }

  def dumpConfigString(fname: String, params: Parameters) {
    val cfgFile = new java.io.FileOutputStream(Driver.targetDir + "/" + fname)
    cfgFile.write(params(ConfigString))
    cfgFile.close
  }
}
import GeneratorUtils._

object RocketChipGenerator extends App {
  val projectName = args(0)
  val topModuleName = args(1)
  val configClassName = args(2)

  val config = getConfig(projectName, configClassName)
  val world = config.toInstance
  val paramsFromConfig = Parameters.root(world)

  elaborate(s"$projectName.$topModuleName", args.drop(3), paramsFromConfig)

  TestGeneration.addSuite(new RegressionTestSuite(paramsFromConfig(RegressionTestNames)))
  TestGeneration.generateMakefrag(topModuleName, configClassName)
  TestBenchGeneration.generateVerilogFragment(
    topModuleName, configClassName, paramsFromConfig)
  TestBenchGeneration.generateCPPFragment(
    topModuleName, configClassName, paramsFromConfig)

  dumpParameters(s"$topModuleName.$configClassName.prm")
  dumpKnobs(configClassName, world)
  dumpConfigString(s"$configClassName.cfg", paramsFromConfig)
}
