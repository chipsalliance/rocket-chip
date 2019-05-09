// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.{BindingScope, Device, ResourceBindings, ResourceBindingsMap}
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable

abstract class LogicalTreeNode(protected val deviceOpt: Option[() => Device]) {
  def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent] = Nil): Seq[OMComponent]
  def getDevice = deviceOpt
}

object LogicalModuleTree {
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
    assert(roots.size == 1, "Logical Tree contains more than one root.")
    roots.head
  }

  def resourceBindings(deviceOpt: Option[() => Device]): ResourceBindings = deviceOpt match {
    case Some(device) => BindingScope.getResourceBindings(device())
    case None => ResourceBindings()
  }

  def bind(): Seq[OMComponent] = {
    def getOMComponentTree(node: LogicalTreeNode): Seq[OMComponent] = {

      node.getOMComponents(resourceBindings(node.getDevice), tree.get(node).getOrElse(Nil).flatMap(getOMComponentTree))
    }

    getOMComponentTree(root)
  }
}
