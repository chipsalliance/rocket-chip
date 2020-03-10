// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.RawModule
//import chisel3.internal.firrtl.{Circuit, DefModule}
import chisel3.internal.firrtl._
import java.io.{File, FileWriter}

import firrtl.annotations.JsonProtocol
import freechips.rocketchip.config._
import freechips.rocketchip.system.{DefaultTestSuites, TestGeneration}
//import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{LazyModuleImp, LazyModule, BaseNode}
import freechips.rocketchip.diplomaticobjectmodel.HasLogicalTreeNode
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model.{OMComponent, OMInterrupt}

import chisel3.aop.{Aspect, Select}

import firrtl.graph._

import scala.collection.mutable

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


//  def getPaths(gr: MutableDiGraph[TreeData]): List[Any] = {
  def getPaths() = {
  }

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
