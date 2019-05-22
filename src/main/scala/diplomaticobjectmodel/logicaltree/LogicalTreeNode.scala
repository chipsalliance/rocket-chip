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
  val root = new GenericLogicalTreeNode()

  def add(parent: LogicalTreeNode, child: => LogicalTreeNode): Unit = {
    val treeOpt = tree.get(parent)
    val treeNode = treeOpt.map{
      children => child +: children
    }.getOrElse(Seq(child))
    tree.put(parent, treeNode)
  }

  def rootLogicalTreeNode: LogicalTreeNode = {
    val roots = tree.collect { case (k, _) if !tree.exists(_._2.contains(k)) => k }
    assert(roots.size <= 2, "Logical Tree contains more than two roots.")
    roots.head
  }

<<<<<<< HEAD
  def getResourceBindings(device: Device, maps: ArrayBuffer[ResourceBindingsMap]): ResourceBindings = {
    val rbm = maps.find {
      rbm => rbm.map.contains(device)
    }.getOrElse {
      throw new IllegalArgumentException(s"""ResourceBindingsMap not found in BindingScope.resourceBindingsMaps""")
    }
=======
  def getResourceBindings(device: Device): ResourceBindings = {
    val bindingScope = bindingScopes.find( bs => bs.getResourceBindingsMap.map.contains(device)).getOrElse {
      bindingScopes.foreach { s =>
        val stuff = s.getResourceBindingsMap.map.keys.collect { case x: SimpleDevice => x }
        println(s"BS: ${stuff.map(_.devname)}")
      }
      println(s"Device = ${device.asInstanceOf[SimpleDevice].devname} ")
>>>>>>> 8fd9717b... add new ram ltn nodes

      throw new IllegalArgumentException(s"""Device not found = ${device.asInstanceOf[SimpleDevice].devname} in BindingScope.resourceBindingsMaps""")
    }
    bindingScope.getResourceBindingsMap.map.get(device).getOrElse(ResourceBindings())
  }

  def resourceBindings(deviceOpt: () => Option[Device]): ResourceBindings = deviceOpt() match {
    case Some(device) => getResourceBindings(device)
    case None => ResourceBindings()
  }

  def cache() = {
    val l = BindingScope.bindingScopes.map{
      case bs => (bs, bs.getResourceBindingsMap)
    }

    l.toMap
  }

  def treeIsEmpty() = tree.size == 0

  def bind(): Seq[OMComponent] = {
    val resourceBindingsMaps= cache()

    def getOMComponentTree(node: LogicalTreeNode): Seq[OMComponent] = {
      node.getOMComponents(resourceBindings(node.getDevice), tree.get(node).getOrElse(Nil).flatMap(getOMComponentTree))
    }

    getOMComponentTree(rootLogicalTreeNode)
  }
}
