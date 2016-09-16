// See LICENSE for license details.

package rocketchip

import Chisel._
import scala.collection.mutable.{LinkedHashSet,LinkedHashMap}
import cde._
import coreplex._
import firrtl._
import firrtl.Annotations._
import firrtl.passes.Pass
import java.io.{File, FileWriter, Writer, PrintWriter}

case object SynTopName extends Field[Option[String]]

case class ParsedInputNames(
    targetDir: String,
    topProject: String,
    topModuleClass: String,
    configProject: String,
    configs: String) {
  val configClasses: Seq[String] = configs.split('_')
  val fullConfigClasses: Seq[String] = configClasses.map(configProject + "." + _)
  val fullTopModuleClass: String = topProject + "." + topModuleClass
}

trait HasGeneratorUtilities {
  def getConfig(names: ParsedInputNames): Config = {
    names.fullConfigClasses.foldRight(new Config()) { case (currentName, config) =>
      val currentConfig = try {
        Class.forName(currentName).newInstance.asInstanceOf[Config]
      } catch {
        case e: java.lang.ClassNotFoundException =>
          throwException(s"""Unable to find part "$currentName" from "${names.configs}", did you misspell it?""", e)
      }
      currentConfig ++ config
    }
  }

  def getParameters(names: ParsedInputNames): Parameters = getParameters(getConfig(names))

  def getParameters(config: Config): Parameters = Parameters.root(config.toInstance)

  import chisel3.internal.firrtl.Circuit
  def elaborate(names: ParsedInputNames, params: Parameters): Circuit = {
    val gen = () =>
      Class.forName(names.fullTopModuleClass)
        .getConstructor(classOf[cde.Parameters])
        .newInstance(params)
        .asInstanceOf[Module]

    chisel3.Driver.elaborate(gen)
  }

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname) 
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }
}

object ConfigStringOutput {
  var contents: Option[String] = None
}

trait Generator extends App with HasGeneratorUtilities {
  lazy val names = {
    require(args.size == 5, "Usage: sbt> " + 
      "run TargetDir TopModuleProjectName TopModuleName ConfigProjectName ConfigNameString")
    ParsedInputNames(
      targetDir = args(0),
      topProject = args(1),
      topModuleClass = args(2),
      configProject = args(3),
      configs = args(4))
  }

  lazy val td = names.targetDir
  lazy val config = getConfig(names)
  lazy val world = config.toInstance
  lazy val params = Parameters.root(world)
  lazy val circuit = elaborate(names, params)
  lazy val longName = names.topModuleClass + "." + names.configs

  def writeOutputFiles() {
    TestGeneration.addSuite(new RegressionTestSuite(params(RegressionTestNames)))
    writeOutputFile(td, s"$longName.d", TestGeneration.generateMakefrag) // Coreplex-specific test suites
    writeOutputFile(td, s"$longName.prm", ParameterDump.getDump) // Parameters flagged with Dump()
    writeOutputFile(td, s"${names.configs}.knb", world.getKnobs) // Knobs for DSE
    writeOutputFile(td, s"${names.configs}.cst", world.getConstraints) // Constraints for DSE
    ConfigStringOutput.contents.foreach(c => writeOutputFile(td, s"${names.configs}.cfg", c)) // String for software

    params(SynTopName) match {
      case Some(synTopName) => {
        firrtl.Driver.compile(
          s"${names.targetDir}/$longName.fir",
          s"${names.targetDir}/$longName.TestHarness.v",
          new EmitHarnessVerilog(synTopName),
          Parser.UseInfo
        )

        firrtl.Driver.compile(
          s"${names.targetDir}/$longName.fir",
          s"${names.targetDir}/$longName.v",
          new EmitTopVerilog(synTopName),
          Parser.UseInfo,
          AnnotationMap(Seq(
            passes.InferReadWriteAnnotation(
              s"${names.topModuleClass}",
              FirrtlVerilogCompiler.infer_read_write_id
            ),
            passes.ReplSeqMemAnnotation(
              s"-c:${synTopName}:-o:${names.targetDir}/$longName.conf",
              FirrtlVerilogCompiler.repl_seq_mem_id
            )
          ))
        )
      }

    case None => {
      firrtl.Driver.compile(
          s"${names.targetDir}/$longName.fir",
          s"${names.targetDir}/$longName.v",
          new RocketChipPassManager,
          Parser.UseInfo
        )
      }

      new PrintWriter(new File(s"${names.targetDir}/$longName.TestHarness.v")).close
      new PrintWriter(new File(s"${names.targetDir}/$longName.conf")).close
    }
  }
}

object FirrtlVerilogCompiler {
  val infer_read_write_id = TransID(-1)
  val repl_seq_mem_id     = TransID(-2)
}

class EmitTopVerilog(topName: String) extends RocketChipPassManager {
  override def operateHigh() = Seq(
    new ReParentCircuit(topName)
  )

  override def operateMiddle() = Seq(
      new passes.InferReadWrite(FirrtlVerilogCompiler.infer_read_write_id),
      new passes.ReplSeqMem(FirrtlVerilogCompiler.repl_seq_mem_id)
    )

  override def operateLow() = Seq(
      new RemoveUnusedModules
    )
}

class EmitHarnessVerilog(topName: String) extends RocketChipPassManager {
  override def operateHigh() = Seq(
      new ConvertToExtMod( (m: firrtl.ir.Module) => m.name == topName )
    )

  override def operateLow() = Seq(
      new RemoveUnusedModules
    )
}

object RocketChipGenerator extends Generator {
  chisel3.Driver.dumpFirrtl(circuit, Some(new File(td, s"$longName.fir"))) // FIRRTL
  writeOutputFiles()
}
