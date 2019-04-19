// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.config.Field
import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable

trait LogicalTreeNode {
  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent]
}

class RootLogicalTreeNode extends LogicalTreeNode {
def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent] = children
}

object LogicalModuleTree {
  val rootLTN = new RootLogicalTreeNode()
  private val tree: mutable.Map[LogicalTreeNode, Seq[LogicalTreeNode]] = mutable.Map[LogicalTreeNode, Seq[LogicalTreeNode]]()
  def add(parent: LogicalTreeNode, child: => LogicalTreeNode): Unit = {
    val treeOpt = tree.get(parent)
    val treeNode = treeOpt.map{
      children => child +: children
    }.getOrElse(Seq(child))
    tree.put(parent, treeNode)
  }

  def root: LogicalTreeNode = {
    val roots = tree.collect { case (k, _) if !tree.exists(_._2.contains(k)) => k }
    assert(roots.size <= 2, "Logical Tree contains more than two roots.")
    roots.head
  }

  def bind(resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
    def getOMComponentTree(node: LogicalTreeNode): Seq[OMComponent] = {
      node.getOMComponents(resourceBindingsMap, tree.get(node).getOrElse(Nil).flatMap(getOMComponentTree))
    }

    getOMComponentTree(root)
  }
}
