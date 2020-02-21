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
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.diplomaticobjectmodel.HasLogicalTreeNode
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model.{OMComponent, OMInterrupt}

import chisel3.aop.{Aspect, Select}

import firrtl.graph._

import scala.collection.mutable

trait HasAopDiplomacyUtilities {

  def writeFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }

  /** Rendering utility for graph */
  def renderGraphML (graph: MutableDiGraph[LazyModuleImp], name: String, compToName: mutable.HashMap[String, String]): String = {

    val s = new mutable.StringBuilder()

    s.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    s.append("<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:y=\"http://www.yworks.com/xml/graphml\">\n")
    s.append("  <key for=\"node\" id=\"n\" yfiles.type=\"nodegraphics\"/>\n")
    s.append("  <key for=\"edge\" id=\"e\" yfiles.type=\"edgegraphics\"/>\n")
    s.append("  <key for=\"node\" id=\"d\" attr.name=\"Description\" attr.type=\"string\"/>\n")
    s.append("  <graph id=\"G\" edgedefault=\"directed\">\n")

    val edges = graph.getEdgeMap

    val sp = "  "
    edges.foreach { case (parent, children) =>  
      s.append(s"""    <node id="${compToName(parent.toString)}">\n""")
      s.append("""       <data key="n"><y:ShapeNode><y:NodeLabel modelName="sides" modelPosition="w" rotationAngle="270.0">${compToName(parent.toString)}</y:NodeLabel></y:ShapeNode></data>\n""")
      s.append("""       <data key="d">${compToName(parent.toString)}</data>\n""")
      s.append("""     </node>\n""")
    }

    edges.foreach { case (parent, children) =>
      children.foreach { child =>
        s.append(s"""    <edge source=\"${compToName(parent.toString)}\" target=\"${compToName(child.toString)}\"/>\n""")
      }
    }

    s.append("  </graph>\n")
    s.append("</graphml>\n")

    s.toString
  } 

  /** Rendering utility for dot graph */
  def renderToDot (digraph: MutableDiGraph[LazyModuleImp], name: String, compToName: mutable.HashMap[String, String]): String = {
    val s = new mutable.StringBuilder()
    val rankDir: String = "LR"

    s.append(s"""digraph $name {""" + "\n")
    s.append(s""" rankdir="$rankDir";""" + "\n")

    val edges = digraph.getEdgeMap

    edges.foreach { case (parent, children) =>
      children.foreach { child =>
        s.append(s"""  "${compToName(parent.toString)}" -> \"${compToName(child.toString)}";""" + "\n")
      }
    }
    s.append("}\n")
    s.toString
  }

  /** Rendering utility for graph */
  def renderTileLinkDot (digraph: MutableDiGraph[String], name: String): String = {
    val s = new mutable.StringBuilder()
    val rankDir: String = "LR"

    s.append(s"""digraph $name {""" + "\n")
    s.append(s""" rankdir="$rankDir";""" + "\n")

    val edges = digraph.getEdgeMap

    edges.foreach { case (parent, children) =>
      children.foreach { child =>
        s.append(s"""  "${parent}" -> "${child}";""" + "\n")
      }
    }
    s.append("}\n")
    s.toString
  }

  /** Output Lazy Module Imp Graph in dot and graphml format. */
  def lazyModuleImpGraph(targetDir: String, circuit: Circuit): String = {
    val res = new StringBuilder
    val graphName = "LazyModuleImpGraph"
    val digraph = new MutableDiGraph[LazyModuleImp]
    val compToName = new mutable.HashMap[String, String]

    val configs =
      circuit.components flatMap { c =>
        c.id match {
          case x : freechips.rocketchip.diplomacy.LazyModuleImp if(! s"${x.getClass.getName}".contains("anon")) =>           
            compToName(c.id.toString) = c.name
            val children = Select.instances(x).filter(y => (! s"${y.getClass.getName}".contains("anon"))).collect {case z: LazyModuleImp => z }
            digraph.addVertex(x)
            children.foreach{y => digraph.addEdge(x, y)}
            None
          case _ => None
        }
      }

    val dotRend = renderToDot (digraph, graphName, compToName)
    writeFile(targetDir, s"${graphName}.gv", dotRend)
    val gml = renderGraphML(digraph, graphName, compToName)
    writeFile(targetDir, s"${graphName}.graphml", gml)

    configs foreach { case x =>
      res append s"${x}\n"
    }
    res.toString
  }

  /** Diplomacy Source/Sink graph (only in dot format) */
  def diplomacySrcSinkGraph(targetDir: String, circuit: Circuit): String = {
    val res = new StringBuilder
    val graphName = "DiplomacySrcSinkGraph"
    val digraph = new MutableDiGraph[String]


    val c1 =
      circuit.components flatMap { c =>
        c.id match {
          case x: LazyModuleImp if(! s"${x.getClass.getName}".contains("anon")) =>           
            val children = Select.instances(x).filter(y => (! s"${y.getClass.getName}".contains("anon"))).map {y => 
              y match {
                case z: LazyModuleImp => 
                  // children-of-lazy-module-imp
                  val childrenOfLMI = Select.instances(z).filter(y => (! s"${y.getClass.getName}".contains("anon")))
                  digraph.addVertex(z.name)
                  childrenOfLMI.foreach{a => 
                    digraph.addVertex(a.name)
                    digraph.addEdge(z.name, a.name)
                  }
                  None
                case _ => None
              }
            }
            None
          case _ => None
        }
      }

    val dotRend = renderTileLinkDot (digraph, graphName)
    writeFile(targetDir, s"${graphName}.gv", dotRend)

    res.toString
  }

  /** Output diplomacy link in dot and graphml format. */
//  def diplomacyLinkOfLazyModule1(targetDir: String, circuit: Circuit): String = {
//    val res = new StringBuilder
//    val graphName = "DiplomacyLinkGraph"
//    val digraph = new MutableDiGraph[String]
//
//    LazyModule.impToLazyModuleMap.foreach{ case (k, v) =>
//      println(s"DEBUG_AOP LazyModuleImp ${k}")
//      digraph.addVertex(k)
//
//      v.nodesAop().foreach { n =>
//        println(s"DEBUG_AOP    node name: ${n.name} index: ${n.index}")
//        println(s"DEBUG_AOP    node formatNode: ${n.formatNode} nodedebugstring ${n.nodedebugstring}")
//      }
//
//      v.childrenAop().foreach{c => 
//        digraph.addVertex(c.name)
//        digraph.addEdge(k, c.name)
//        println(s"DEBUG_AOP    child name: ${c.name} param p: ${c.p}")
//        println(s"DEBUG_AOP    child name: ${c.getClass.getName}")
//      }
//    }
//    val dotRend = renderDiplomaticLinkDot (digraph, graphName)
//    writeFile(targetDir, s"${graphName}.gv", dotRend)
//
//    res.toString
//  }


}

