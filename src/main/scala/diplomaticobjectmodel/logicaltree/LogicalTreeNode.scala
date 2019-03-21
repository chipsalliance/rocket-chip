// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable

trait LogicalTreeNode {
  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent]
}

/** LogicalTreeEdges hold LogicalTree Nodes which will be used to construct the logical tree.
  * The LogicalTreeEdge is used to construct a parent child relationship between different modules.
  *
  * 1. First a list of   val edges = ArrayBuffer[LogicalTreeEdge]() is constructed.
  *
  * 2. Then def getTreeMap(): Map[LogicalTree, List[LogicalTree]] converts the list into a map which represents the tree.
  *
  * 3. Then the tree map is converted into a OMLogicalTree by the  def makeTree(): Tree[LogicalTree].
  *
  * @param parent The parent LogicalTree node
  * @param child The child LogicalTree node
  */
case class LogicalTreeEdge(
  parent: LogicalTreeNode,
  child: LogicalTreeNode
)

object LogicalModuleTree {
  private val tree: mutable.Map[LogicalTreeNode, Seq[LogicalTreeNode]] = mutable.Map[LogicalTreeNode, Seq[LogicalTreeNode]]()
  def add(parent: LogicalTreeNode, child: LogicalTreeNode): Unit = {
    val edge = LogicalTreeEdge(parent, child)
    val treeNode = tree.get(edge.parent).map{
      case x => edge.child +: x
    }.getOrElse(Seq(edge.child))
    tree.put(edge.parent, treeNode)
  }

  def root: LogicalTreeNode = {
    val roots = tree.collect { case (k, _) if !tree.exists(_._2.contains(k)) => k }
    assert(roots.size == 1, "Logical Tree contains more than one root.")
    roots.head
  }

  def bind(resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
    def recurse(node: LogicalTreeNode): Seq[OMComponent] = {
      node.getOMComponents(resourceBindingsMap, tree.get(node).getOrElse(Nil).flatMap(recurse))
    }
    recurse(root)
  }
}
