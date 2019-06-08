// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.BindingScope.bindingScopes
import freechips.rocketchip.diplomacy.{BindingScope, Device, ResourceBindings, ResourceBindingsMap, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class LogicalTreeNode(protected val deviceOpt: () => Option[Device]) {
  def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent] = Nil): Seq[OMComponent]

  def getDevice = deviceOpt
}

class GenericLogicalTreeNode extends LogicalTreeNode(() => None) {
  override def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent] = Nil): Seq[OMComponent] =
    children
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

  def rootLogicalTreeNode: LogicalTreeNode = {
    val roots = tree.collect { case (k, _) if !tree.exists(_._2.contains(k)) => k }
    assert(roots.size == 1, "Logical Tree contains more than one root.")
    roots.head
  }

  def getResourceBindings(device: Device, maps: ArrayBuffer[ResourceBindingsMap]): ResourceBindings = {
    val rbm = maps.find {
      rbm => rbm.map.contains(device)
    }.getOrElse {
      throw new IllegalArgumentException(s"""ResourceBindingsMap not found in BindingScope.resourceBindingsMaps""")
    }

    rbm.map.get(device).getOrElse(
      throw new IllegalArgumentException(s"""Device not found = ${device.asInstanceOf[SimpleDevice].devname} in BindingScope.resourceBindingsMaps""")
    )
  }

  def resourceBindings(deviceOpt: () => Option[Device], maps: ArrayBuffer[ResourceBindingsMap]): ResourceBindings = deviceOpt() match {
    case Some(device) => getResourceBindings(device, maps)
    case None => ResourceBindings()
  }

  def cache() = BindingScope.bindingScopes.map(_.getResourceBindingsMap)

  def treeIsEmpty() = tree.size == 0

  def bind(): Seq[OMComponent] = {
    val resourceBindingsMaps= cache()

    def getOMComponentTree(node: LogicalTreeNode): Seq[OMComponent] = {
      val rbs = resourceBindings(node.getDevice, resourceBindingsMaps)
      node.getOMComponents(rbs, tree.get(node).getOrElse(Nil).flatMap(getOMComponentTree))
    }

    getOMComponentTree(rootLogicalTreeNode)
  }
}
