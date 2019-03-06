// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelUtils
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import freechips.rocketchip.util.ElaborationArtefacts
import scala.collection.mutable.ArrayBuffer


case object OMRegistry {
  private var resourceBindingsMap: Option[ResourceBindingsMap] = None

  def setResourceBindingsMap(rbm: ResourceBindingsMap): Unit = resourceBindingsMap = Some(rbm)
  def getResourceBindingsMap(): ResourceBindingsMap = resourceBindingsMap.get
}

trait LogicalTree {
  def getOMComponents(children: Seq[OMComponent]): Seq[OMComponent]
}

case class LogicalTreeEdge(
  parent: LogicalTree,
  child: LogicalTree
)

case object LogicalModuleTree {
  val edges = ArrayBuffer[LogicalTreeEdge]()

  def add(parent: LogicalTree, child: LogicalTree): Unit = {
     edges += LogicalTreeEdge(parent, child)
  }

  private def cycleCheck(): Boolean = false

  def getTreeMap(): Map[LogicalTree, List[LogicalTree]] = {
    edges.groupBy(_.parent).map{ case (k, v) => (k, v.map(_.child).toList)}
  }

  def findRoot(): LogicalTree = {
    val values = getTreeMap().values.flatten.map{ case c => c }.toSet

    val roots = getTreeMap().keys.map { case p if ! values.contains(p) => p}

    assert(roots.size == 1)
    roots.head
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, children: List[Tree[A]]) extends Tree[A]

object OMRegistrarTree {
  def makeTree(): Tree[LogicalTree] = {
    val root: LogicalTree = LogicalModuleTree.findRoot()
    val treeMap = LogicalModuleTree.getTreeMap()

    def tree(r: LogicalTree): Tree[LogicalTree] = {
      val children = treeMap.getOrElse(r, Nil)

      children match {
        case Nil => Leaf(r)
        case cs =>
          Branch(r, cs.map(tree(_)))
      }
    }
    tree(root)
  }
}

object OMTree {
  def tree(t: Tree[LogicalTree]): Seq[OMComponent] =
    t match {
      case Leaf(r) => r.getOMComponents(Nil) // a.getOMComponents()
      case Branch(r, cs) =>
          val components = cs.flatMap(tree(_))
          r.getOMComponents(components)
    }
}

case object OMPipeline {
  def process(): Unit = {
    val registrarTree: Tree[LogicalTree] = OMRegistrarTree.makeTree()
    val om = OMTree.tree(registrarTree)
    ElaborationArtefacts.add("objectModel1.json", DiplomaticObjectModelUtils.toJson(om))
  }
}