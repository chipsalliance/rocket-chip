// See LICENSE for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.LogicalTreeNode
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.util.LocationMap

import firrtl.graph._

case object HierarchyKey extends Field[DiGraph[HierarchicalLocation]]

case object ESS0 extends HierarchicalLocation("ESS0")
case object ESS1 extends HierarchicalLocation("ESS1")

case class EmptySubsystemParams(
  ibus: InterruptBusWrapper,
  tlBusWrapperLocationMap: LocationMap[TLBusWrapper],
  logicalTreeNode: LogicalTreeNode,
  asyncClockGroupsNode: ClockGroupEphemeralNode)

class EmptySubsystem(name: String, params: EmptySubsystemParams)(implicit p: Parameters) extends LazyModule with Attachable {

  val ibus = params.ibus
  def attachableSubhierarchies = None
  override val tlBusWrapperLocationMap = params.tlBusWrapperLocationMap
  def logicalTreeNode = params.logicalTreeNode
  implicit val asyncClockGroupsNode = params.asyncClockGroupsNode

  lazy val module = new LazyModuleImp(this) {
    //override def desiredName: String = name
  }
}

trait HasConfigurableHierarchy { this: Attachable =>
  def location: HierarchicalLocation

  def createHierarchyMap(
    root: HierarchicalLocation,
    graph: DiGraph[HierarchicalLocation],
    context: Attachable,
    params: EmptySubsystemParams): Unit = {

    // Add the current hiearchy to the map
    hierarchyMap += (root -> context)

    // If the current hierarchy has attachable subhierarchies, add those to the map
    context.attachableSubhierarchies.foreach(_.foreach { hierarchy =>
      hierarchy match {
        case ss: BaseSubsystem => hierarchyMap += (InSubsystem -> ss)
        case _ => throw new Exception("Undefined subhierarchy type.")
    }})

    // Create and recurse on child hierarchies
    val edges = graph.getEdges(root)
    edges.foreach { edge =>
      val ess = context { LazyModule(new EmptySubsystem(edge.name, params)) }
      createHierarchyMap(edge, graph, ess, params)
    }
  }

  val essParams = EmptySubsystemParams(
    ibus = this.ibus,
    tlBusWrapperLocationMap = this.tlBusWrapperLocationMap,
    logicalTreeNode = this.logicalTreeNode,
    asyncClockGroupsNode = this.asyncClockGroupsNode)

  val hierarchyMap = LocationMap.empty[Attachable]
  createHierarchyMap(location, p(HierarchyKey), this, essParams)
  println("\n\n\nPrinting p(HierarchyKey):")
  println(p(HierarchyKey))
  println("\n\n\nPrinting generated hierarchyMap:")
  println(hierarchyMap)

}

class Hierarchy(val root: HierarchicalLocation) {
  require(root == InSystem || root == InSubsystem, "Invalid root hierarchy")

  val graph = new MutableDiGraph[HierarchicalLocation]
  graph.addVertex(root)

  def addSubhierarchy(parent: HierarchicalLocation, child: HierarchicalLocation): Unit = {
    graph.addVertex(child)
    graph.addEdge(parent,child) 
  }

  def closeHierarchy(): DiGraph[HierarchicalLocation] = {
    DiGraph(graph)
  }

}

object Hierarchy {
  def init(root: HierarchicalLocation): Hierarchy = {
    val hierarchy = new Hierarchy(root)
    hierarchy
  }

  def default(root: HierarchicalLocation): DiGraph[HierarchicalLocation] = {
    val h = init(root)
    h.closeHierarchy()
  }

}
