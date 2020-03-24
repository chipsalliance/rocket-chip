// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import scala.io.Source
import java.nio.file.{Paths, Files}

import Chisel._
import chisel3.RawModule
//import chisel3.internal.firrtl.{Circuit, DefModule}
import chisel3.internal.firrtl._
import java.io.{File, FileWriter}

import firrtl.annotations.JsonProtocol
import freechips.rocketchip.config._
import freechips.rocketchip.system.{DefaultTestSuites, TestGeneration}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.diplomacy.{LazyModuleImp, LazyModule, BaseNode}
import freechips.rocketchip.diplomaticobjectmodel.HasLogicalTreeNode
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model.{OMComponent, OMInterrupt}

import chisel3.aop.{Aspect, Select}

import firrtl.graph._

import scala.collection.mutable

import scala.collection.JavaConverters._

trait GraphType
object DOTGRAPH extends GraphType
object GRAPHML extends GraphType

case class RTreeData (val item: Any)


object SelectDiplomacy {

  val digraph = new MutableDiGraph[RTreeData]

  def findNode(name:String): Any = {
    None
  }

  /** Collects all Lazy Module Imp as a List.
    */
  def collectImpModules(): List[LazyModuleImp] = {
    val edges = digraph.getEdgeMap
    var lst: List[LazyModuleImp] = List[LazyModuleImp]()
    edges.foreach { case (parent, children) =>
      parent.item match {
        case x: LazyModuleImp => lst = x :: lst
        case _ => None
      }
    }
    lst
  }

  /** Create a file with all Lazy Module Imp names.
    */
  def collectImpModules(targetDir: String, fname: String): String = {
    val sb = new mutable.StringBuilder()
    val edges = digraph.getEdgeMap
    edges.foreach { case (parent, children) =>
      parent.item match {
        case x: LazyModuleImp => sb.append(s"${x.getClass.getName}.${x.name}\n")
        case _ => 
      }
    }
    writeFile(targetDir, fname, sb.toString)
    sb.toString
  }

  /** Collects all BaseNodes as a List.
    */
  def collectBaseNodes(): List[BaseNode] = {

    val vertices = digraph.getVertices
    var lst: List[BaseNode] = List[BaseNode]()
    vertices.foreach { case x =>
      x.item match {
        case x: BaseNode => lst = x :: lst
        case _ => None
      }
    }
    lst
  }

  /** Create a file with all Base Nodes names.
    */
  def collectBaseNodes(targetDir: String, fname: String): String = {
    val sb = new mutable.StringBuilder()
    val edges = digraph.getEdgeMap
    edges.foreach { case (parent, children) =>
      parent.item match {
        case x: BaseNode => sb.append(s"${x.getClass.getName}.${x.name}\n")
        case _ => 
      }
    }
    writeFile(targetDir, fname, sb.toString)
    sb.toString
  }

  /** Collects all Lazy Modules as a List.
    */
  def collectLazyModules(): List[LazyModule] = {
    val edges = digraph.getEdgeMap
    var lst: List[LazyModule] = List[LazyModule]()
    edges.foreach { case (parent, children) =>
      parent.item match {
        case x: LazyModule => lst = x :: lst
        case _ => None
      }
    }
    lst
  }

  /** Create a file with all Lazy Module names.
    */
  def collectLazyModules(targetDir: String, fname: String): String = {
    val sb = new mutable.StringBuilder()
    val edges = digraph.getEdgeMap
    edges.foreach { case (parent, children) =>
      parent.item match {
        case x: LazyModule => sb.append(s"${x.getClass.getName}.${x.name}\n")
        case _ => 
      }
    }
    writeFile(targetDir, fname, sb.toString)
    sb.toString
  }

//  def findImpModuleByName(name: String): LazyModuleImp = {
//    collectImpModules.find(Some(_).name == name)
//  }
//
  def findBaseNodeByName(name: String): Option[BaseNode] = {
    var ret: Option[BaseNode] = None
    collectBaseNodes.foreach{x => if (x.name == name) ret = Some(x) }
    ret
  }
//
//  def findLazyModuleByName(name: String): LazyModule = {
//    collectLazyModules.find(_.name == name)
//  }
//
//
//  def findImpModuleByRegex(name: String): LazyModuleImp = {
//    collectImpModules.find(_.name == name)
//  }
//
//  def findBaseNodeByRegex(name: String): BaseNode = {
//    collectBaseNodes.find(_.name == name)
//  }
//
//  def findLazyModuleByRegex(name: String): LazyModule = {
//    collectLazyModules.find(_.name == name)
//  }

  def mixedNodes() = collectBaseNodes().collect{case x: MixedNode[Data, Data, Data, Data, Data, Data, Data, Data] => x}
  def mixedCustomNodes() = collectBaseNodes().collect{case x: MixedCustomNode[Data, Data, Data, Data, Data, Data, Data, Data] => x}
  def customNodes() = collectBaseNodes().collect{case x: CustomNode[Data, Data, Data, Data, Data] => x}
  def junctionNodes() = collectBaseNodes().collect{case x: JunctionNode[Data, Data, Data, Data, Data] => x}
  def mixedAdapterNodes() = collectBaseNodes().collect{case x: MixedAdapterNode[Data, Data, Data, Data, Data, Data, Data, Data] => x}
  def adapterNodes() = collectBaseNodes().collect{case x: AdapterNode[Data, Data, Data, Data, Data] => x}
  def identityNodes() = collectBaseNodes().collect{case x: IdentityNode[Data, Data, Data, Data, Data] => x}
  def ephemeralNodes() = collectBaseNodes().collect{case x: EphemeralNode[Data, Data, Data, Data, Data] => x}
  def mixedNexusNodes() = collectBaseNodes().collect{case x: MixedNexusNode[Data, Data, Data, Data, Data, Data, Data, Data] => x}
  def nexusNodes() = collectBaseNodes().collect{case x: NexusNode[Data, Data, Data, Data, Data] => x}
  def sourceNodes() = collectBaseNodes().collect{case x: SourceNode[Data, Data, Data, Data, Data] =>  x}
  def sinkNodes() = collectBaseNodes().collect{case x: SinkNode[Data, Data, Data, Data, Data] => x}
  def mixedTestNodes() = collectBaseNodes().collect{case x: MixedTestNode[Data, Data, Data, Data, Data, Data, Data, Data] => x}

  def clientTLNodes() = collectBaseNodes().collect{case x: TLClientNode => x}
  def clientTLNode(s: String) = collectBaseNodes().collect{case x: TLClientNode if(s == x.name) => x}

/** Return source nodes of type T from the imp module M
  */
  def getSrcNodes[M]() = {
    val clientNodes = SelectDiplomacy().sourceNodes.collect{
      case a:  SourceNode[Data, Data, Data, Data, Data] if(a.getClass.getName == "freechips.rocketchip.tilelink.TLClientNode") => a.asInstanceOf[TLClientNode]
    }.filter{case c => c.parents.foldLeft(false){(z,f) => 
      val v1 = f match {
        case x: M => true
        case _ => false
      }
      z || v1
    }}

    clientNodes
  }

  def getSrcNode[M]() = getSrcNodes[M]().head

/** Return sink nodes of type T from the imp module M
  */
  def getSinkNodes[M]() = {
    val clientNodes = SelectDiplomacy().sinkNodes.collect{
      case a:  SourceNode[Data, Data, Data, Data, Data] if(a.getClass.getName == "freechips.rocketchip.tilelink.TLManagerNode") => a.asInstanceOf[TLManagerNode]
    }.filter{case c => c.parents.foldLeft(false){(z,f) => 
      val v1 = f match {
        case x: M => true
        case _ => false
      }
      z || v1
    }}

    clientNodes
  }

  def getSinkNode[M]() = getSinkNodes[M]().head

  def apply() = {

    def diplChildren(parent: RTreeData, children: List[LazyModule]): Unit = {
      children.foreach{c =>
        if(! s"${c.getClass.getName}".contains("anon")) {
          diplNodes(parent, c.nodesAop())
          digraph.addVertex(parent)
          val a = diplChildren(parent, c.childrenAop())
          val c1 = RTreeData(c)
          digraph.addVertex(c1)
          digraph.addEdge(parent, c1)
        }
      }
    }

    def diplNodes(parent: RTreeData, nodes: List[BaseNode]) = {
      if(! s"${parent.item.getClass.getName}".contains("anon")) {
        digraph.addVertex(parent)
        nodes.foreach{n => if(! s"${n.getClass.getName}".contains("anon")) {
          val n1 = RTreeData(n)
          digraph.addVertex(n1)
          digraph.addEdge(parent, n1)
        }}
      }
    }

    LazyModule.impToLazyModuleMap.foreach{ case (k, v) =>

      val imp = v._1
      val lm = v._2
      if(! (s"${lm.getClass.getName}".contains("anon"))) {
        val data = RTreeData(imp)
        digraph.addVertex(data)
        diplChildren(data,  lm.childrenAop())
        diplNodes(data, lm.nodesAop())
      }
    }
    this
  }

  def viewDiplomacy() = {
    val pwdDir = System.getenv().asScala{"PWD"}
    println(s"view Diplomacy PWD ${pwdDir}")

    val ItemRegex = """(\w+)\s+(\w+)""".r
    val filename = s"${pwdDir}/diplomacy.view"
    if(Files.exists(Paths.get(filename))) {
      for (line <- Source.fromFile(filename).getLines) {
        line match {
          case line if line.startsWith("//") => ""
          case ItemRegex(a, b) =>
            a match {
              case "graph" => b match {
                case "dot" => render(pwdDir, "diplGraph", DOTGRAPH)
              }
              case "BaseNode" => b match {
                case "all" => {
                  val baseNodes = SelectDiplomacy().collectBaseNodes()
                  baseNodes.foreach{n => println(s"BASE NODE name: ${n.name} cls: ${n.getClass.getName} <<< ${n} >>>")}
                }
                case "byGroup" => {
                  for(e <- mixedNodes()) println(s"NodesByGroup mixedNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- mixedCustomNodes()) println(s"NodesByGroup mixedCustomNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- mixedCustomNodes()) println(s"NodesByGroup mixedCustomNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- junctionNodes()) println(s"NodesByGroup junctionNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- mixedAdapterNodes()) println(s"NodesByGroup mixedAdapterNode name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- adapterNodes()) println(s"NodesByGroup adapterNode name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- identityNodes()) println(s"NodesByGroup identityNode name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- ephemeralNodes()) println(s"NodesByGroup ephemeraNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- mixedNexusNodes()) println(s"NodesByGroup mixedNexuxNode name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- nexusNodes()) println(s"NodesByGroup nexuxNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- ephemeralNodes()) println(s"NodesByGroup ephemeralNode name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- sourceNodes()) println(s"NodesByGroup sourceNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- sinkNodes()) println(s"NodesByGroup sinkNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                  for(e <- mixedTestNodes()) println(s"NodesByGroup mixedTestNodes name: ${e.name} cls: ${e.getClass.getName} <<<${e}>>>")
                }
              }
              case "impModules" => b match {
                case "all" => {
                  val impMod = SelectDiplomacy().collectImpModules()
                  impMod.foreach{n => println(s"IMP Module ${n.getClass.getName}")}
                }
              }
              case "lazyModules" => b match {
                case "all" => {
                  val lazyMod = SelectDiplomacy().collectLazyModules()
                  lazyMod.foreach{n => println(s"Lazy Module ${n.getClass.getName}")}
                }
              }
            }
          case _ => ""
        }
      }
    }
  }

  def writeFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }


  def render(targetDir: String, fileName: String, grtype: GraphType): Unit = {

    grtype match {
      case DOTGRAPH =>
        val s = new mutable.StringBuilder()
        val rankDir: String = "LR"

        s.append(s"""digraph $fileName {""" + "\n")
        s.append(s""" rankdir="$rankDir";""" + "\n")

        val edges = digraph.getEdgeMap
        edges.foreach { case (parent, children) =>

          parent.item match {
            case y: BaseNode => 
              s.append(s"""  "${y.name}" [shape=ellipse,style=filled,color=skyblue]""" + "\n")
              children.foreach {child =>
                child.item match {
                  case x: BaseNode =>
                    s.append(s"""  "${x.name}" [shape=ellipse,style=filled,color=skyblue]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case x: LazyModule =>
                    s.append(s"""  "${x.name}" [shape=box,style=filled,color=green]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case x: LazyModuleImp =>
                    s.append(s"""  "${x.name}" [shape=hexagon,style=filled,color=red]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case _ => // println("DEBUG_AOP UNKNOWN"); ""
                }
              }
            case y: LazyModule =>
              s.append(s"""  "${y.name}" [shape=box,style=filled,color=green]""" + "\n")
              children.foreach {child =>
                child.item match {
                  case x: BaseNode =>
                    s.append(s"""  "${x.name}" [shape=ellipse,style=filled,color=skyblue]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case x: LazyModule =>
                    s.append(s"""  "${x.name}" [shape=box,style=filled,color=green]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case x: LazyModuleImp =>
                    s.append(s"""  "${x.name}" [shape=hexagon,style=filled,color=red]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case _ => // println("DEBUG_AOP UNKNOWN"); ""
                }
              }
            case y: LazyModuleImp =>
              s.append(s"""  "${y.name}" [shape=hexagon,style=filled,color=red]""" + "\n")
              children.foreach {child =>
                child.item match {
                  case x: BaseNode =>
                    s.append(s"""  "${x.name}" [shape=ellipse,style=filled,color=skyblue]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case x: LazyModule =>
                    s.append(s"""  "${x.name}" [shape=box,style=filled,color=green]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case x: LazyModuleImp =>
                    s.append(s"""  "${x.name}" [shape=hexagon,style=filled,color=red]""" + "\n")
                    s.append(s"""  "${y.name}" -> "${x.name}";""" + "\n")
                  case _ => // println("DEBUG_AOP UNKNOWN"); ""
                }
              }
            case _ => // println("DEBUG_AOP UNKNOWN"); ""
          }
        }
        s.append("}\n")
        writeFile(targetDir, s"${fileName}.gv", s.toString)
    }
  }

}


/*********
API
===

collectImpModules(): List[LazyModuleImp]
collectImpModules(targetDir: String, fname: String): String

def collectBaseNodes(): List[BaseNode]
def collectBaseNodes(targetDir: String, fname: String): String

def collectLazyModules(): List[LazyModule]
def collectLazyModules(targetDir: String, fname: String): String

 * ************/
