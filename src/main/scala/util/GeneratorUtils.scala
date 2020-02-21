// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import java.io.{File, FileWriter}

import Chisel.throwException
import freechips.rocketchip.diplomacy.{LazyModuleImp, LazyModule, BaseNode}
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.internal.firrtl.Circuit

trait HasRocketChipStageUtils {

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

  def injectMonitor(targetDir: String, circuit: Circuit) = {
    println("DEBUG_AOP: inject monitor")
    val dip = SelectDiplomacy(circuit).render(targetDir, "DiplomacyGraph", DOTGRAPH)
    val impModules: List[LazyModuleImp] = SelectDiplomacy.collectImpModules()
    val str = SelectDiplomacy.collectImpModules(targetDir, "collectImp.txt")
    val str1 = SelectDiplomacy.collectBaseNodes(targetDir, "collectBaseNodes.txt")
    val str2 = SelectDiplomacy.collectLazyModules(targetDir, "collectLazyModules.txt")

    val baseNodes: List[BaseNode] = SelectDiplomacy.collectBaseNodes()
    baseNodes.foreach{ n =>
      println(s"DEBUG_AOP: BASE_NODE name: ${n.name} valName; ${n.valName} formatNode: +++${n.formatNode}---")
      println("INPUTS")
      n.inputs.foreach{println}
      println("OUTPUTS")
      n.outputs.foreach{println}
    }

    InjectMonitor.pr()
  }
}

object InjectMonitor {

  def pr() = {
    println("DEBUG_AOP: from apply() InjectMonitor")
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
