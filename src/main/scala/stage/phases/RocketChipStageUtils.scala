// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import java.io.{File, FileWriter}

import firrtl.options.Viewer.view
import firrtl.AnnotationSeq
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.system.{DefaultTestSuites, TestGeneration}

trait HasRocketChipStageUtils {

  def getFullTopModuleClass(annotations: AnnotationSeq): String = {
    val entOpts = view[RocketChipOptions](annotations)
    s"${entOpts.topModulePackage.get}.${entOpts.topModuleClass.get}"
  }

  def getFullConfigClasses(annotations: AnnotationSeq): Seq[String] = {
    val entOpts = view[RocketChipOptions](annotations)
    val configClasses = entOpts.configs.get.split("_")
    configClasses.map( configClass => s"${entOpts.configsPackage.get}.${configClass}")
  }

  def getShortName(annotations: AnnotationSeq): String = {
    val entOpts = view[RocketChipOptions](annotations)
    entOpts.outputBaseName.getOrElse(s"${entOpts.configs.get}")
  }

  def getLongName(annotations: AnnotationSeq): String = {
    val entOpts = view[RocketChipOptions](annotations)
    entOpts.outputBaseName.getOrElse(s"${entOpts.topModulePackage.get}.${entOpts.configs.get}")
  }

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }

  /** Output software test Makefrags, which provide targets for integration testing. */
  def addTestSuites {
    TestGeneration.addSuite(DefaultTestSuites.groundtest64("p"))
    TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
    TestGeneration.addSuite(DefaultTestSuites.singleRegression)
  }

}
