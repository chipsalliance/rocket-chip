// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree


import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable

trait LogicalTreeNode {
  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent]
}

class GenericLogicalTreeNode extends LogicalTreeNode {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent] = Nil): Seq[OMComponent] =
    children
}

object LogicalModuleTree {
  val resourceScopes = collection.mutable.Map[LogicalTreeNode, () => ResourceBindingsMap]()

  val root = new GenericLogicalTreeNode()

  private val tree: mutable.Map[LogicalTreeNode, Seq[LogicalTreeNode]] = mutable.Map[LogicalTreeNode, Seq[LogicalTreeNode]]()

  private val children: mutable.Set[LogicalTreeNode] = mutable.Set[LogicalTreeNode]()

  def addResourceScope(ltn: LogicalTreeNode, rbm: () => ResourceBindingsMap): Unit = {
    require(! resourceScopes.contains(ltn))
    resourceScopes.put(ltn, rbm)
  }

  /*
   * Check that the parent does not contain the child already.
   * Check that the child has not been added to the tree more than once.
  */
  def add(parent: LogicalTreeNode, child: => LogicalTreeNode, resourceBindingsMap: Option[() => ResourceBindingsMap] = None): Unit = {
    require(! children.contains(child), s"Child LogicalTreeNode already exists in the map: $child")
    children.add(child)
    val treeOpt = tree.get(parent)
    val treeNode = treeOpt.map{
      children => child +: children
    }.getOrElse(Seq(child))
    tree.put(parent, treeNode)
    resourceBindingsMap.map(addResourceScope(child, _))
  }

  def rootLogicalTreeNode: LogicalTreeNode = {
    val roots = tree.collect { case (k, _) if !tree.exists(_._2.contains(k)) => k }
    assert(roots.size == 1, "Logical Tree contains more than one root.")
    roots.head
  }

  def bind(resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
    def getOMComponentTree(node: LogicalTreeNode): Seq[OMComponent] = {
      val rbm = resourceScopes.get(node) match {
        case Some(r) => r()
        case None => resourceBindingsMap
      }
      node.getOMComponents(rbm, tree.get(node).getOrElse(Nil).flatMap(getOMComponentTree))
    }

    getOMComponentTree(rootLogicalTreeNode)
  }
}
