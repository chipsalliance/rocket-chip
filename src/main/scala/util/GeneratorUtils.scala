// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.experimental.RawModule
import chisel3.internal.firrtl.Circuit
// TODO: better job of Makefrag generation for non-RocketChip testing platforms
import java.io.{File, FileWriter}

import firrtl.annotations.JsonProtocol
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.system.{DefaultTestSuites, TestGeneration}

/** Representation of the information this Generator needs to collect from external sources. */
case class ParsedInputNames(
    targetDir: String,
    topModuleProject: String,
    topModuleClass: String,
    configProject: String,
    configs: String) {
  val configClasses: Seq[String] = configs.split('_')
  def prepend(prefix: String, suffix: String) =
    if (prefix == "" || prefix == "_root_") suffix else (prefix + "." + suffix)
  val fullConfigClasses: Seq[String] = configClasses.map(x => prepend(configProject, x))
  val fullTopModuleClass: String = prepend(topModuleProject, topModuleClass)
}

/** Common utilities we supply to all Generators. In particular, supplies the
  * canonical ways of building various JVM elaboration-time structures.
  */
trait HasGeneratorUtilities {
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

  def getParameters(names: Seq[String]): Parameters = getParameters(getConfig(names))

  def getParameters(config: Config): Parameters = config.toInstance

  def elaborate(fullTopModuleClassName: String, params: Parameters): Circuit = {
    val top = () =>
      Class.forName(fullTopModuleClassName)
          .getConstructor(classOf[Parameters])
          .newInstance(params) match {
        case m: RawModule => m
        case l: LazyModule => LazyModule(l).module
      }

    Driver.elaborate(top)
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
}

/** Standardized command line interface for Scala entry point */
trait GeneratorApp extends App with HasGeneratorUtilities {
  lazy val names: ParsedInputNames = {
    require(args.size == 5, "Usage: sbt> " +
      "run TargetDir TopModuleProjectName TopModuleName " +
      "ConfigProjectName ConfigNameString")
    ParsedInputNames(
      targetDir = args(0),
      topModuleProject = args(1),
      topModuleClass = args(2),
      configProject = args(3),
      configs = args(4))
  }

  // Canonical ways of building various JVM elaboration-time structures
  lazy val td: String = names.targetDir
  lazy val config: Config = getConfig(names.fullConfigClasses)
  lazy val params: Parameters = config.toInstance
  lazy val circuit: Circuit = elaborate(names.fullTopModuleClass, params)

  val longName: String // Exhaustive name used to interface with external build tool targets

  /** Output FIRRTL, which an external compiler can turn into Verilog. */
  def generateFirrtl {
    Driver.dumpFirrtl(circuit, Some(new File(td, s"$longName.fir"))) // FIRRTL
  }

  def generateAnno {
    val annotationFile = new File(td, s"$longName.anno.json")
    val af = new FileWriter(annotationFile)
    af.write(JsonProtocol.serialize(circuit.annotations.map(_.toFirrtl)))
    af.close()
  }

  /** Output software test Makefrags, which provide targets for integration testing. */
  def generateTestSuiteMakefrags {
    addTestSuites
    writeOutputFile(td, s"$longName.d", TestGeneration.generateMakefrag) // Subsystem-specific test suites
  }

  def addTestSuites {
    TestGeneration.addSuite(DefaultTestSuites.groundtest64("p"))
    TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
    TestGeneration.addSuite(DefaultTestSuites.singleRegression)
  }

  def generateROMs {
    writeOutputFile(td, s"$longName.rom.conf", enumerateROMs(circuit))
  }

  /** Output files created as a side-effect of elaboration */
  def generateArtefacts {
    ElaborationArtefacts.files.foreach { case (extension, contents) =>
      writeOutputFile(td, s"$longName.$extension", contents ())
    }
  }

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }
}

object ElaborationArtefacts {
  var files: Seq[(String, () => String)] = Nil

  def add(extension: String, contents: => String) {
    files = (extension, () => contents) +: files
  }

  def contains(extension: String): Boolean = {
    files.foldLeft(false)((t, s) => {s._1 == extension | t})
  }
}
