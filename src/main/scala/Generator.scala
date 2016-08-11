// See LICENSE for license details.

package rocketchip

import Chisel._
import scala.collection.mutable.{LinkedHashSet,LinkedHashMap}
import cde.{Parameters, ParameterDump, Config, Field, CDEMatchError}
import coreplex._

object TestGenerator extends App {
  val projectName = args(0)
  val topModuleName = args(1)
  val configClassName = args(2)

  val aggregateConfigs = configClassName.split('_')

  val finalConfig = aggregateConfigs.foldRight(new Config()) { case (currentConfigName, finalConfig) =>
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
  val world = finalConfig.toInstance

  val paramsFromConfig: Parameters = Parameters.root(world)

  val gen = () => 
    Class.forName(s"$projectName.$topModuleName")
      .getConstructor(classOf[cde.Parameters])
      .newInstance(paramsFromConfig)
      .asInstanceOf[Module]

  chiselMain.run(args.drop(3), gen)
  //Driver.elaborate(gen, configName = configClassName)

  TestGeneration.addSuite(new RegressionTestSuite(paramsFromConfig(RegressionTestNames)))

  TestGeneration.generateMakefrag(topModuleName, configClassName)
  TestBenchGeneration.generateVerilogFragment(
    topModuleName, configClassName, paramsFromConfig)
  TestBenchGeneration.generateCPPFragment(
    topModuleName, configClassName, paramsFromConfig)

  val pdFile = TestGeneration.createOutputFile(s"$topModuleName.$configClassName.prm")
  pdFile.write(ParameterDump.getDump)
  pdFile.close
  val v = TestGeneration.createOutputFile(configClassName + ".knb")
  v.write(world.getKnobs)
  v.close
  val d = new java.io.FileOutputStream(Driver.targetDir + "/" + configClassName + ".cfg")
  d.write(paramsFromConfig(ConfigString))
  d.close
  val w = TestGeneration.createOutputFile(configClassName + ".cst")
  w.write(world.getConstraints)
  w.close
}
