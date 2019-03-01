// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3.experimental.RawModule
import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable

trait OMRegistrar {
  def parent: Option[OMRegistrar]
  def children: List[OMRegistrar]
  def getOMComponents(): Seq[OMComponent] // These are the child components
  def insertChildren(): Unit // this function inserts the child components into this node's component
}

/**
  * Used to construct the logical spanning tree from the Edge's
  * @param components
  * @param children
  */

case class OMTreeNode(
  registrar: OMRegistrar,
  components: List[OMComponent],
  children: List[OMTreeNode]
)

case class RawModuleContainer(
  rm: RawModule
)

case object OMRegistry {
  private var resourceBindingsMap: Option[ResourceBindingsMap] = None

  private val registry = mutable.Map[RawModuleContainer, OMRegistrar]()

  def setResourceBindingsMap(rbm: ResourceBindingsMap): Unit = resourceBindingsMap = Some(rbm)
  def getResourceBindingsMap(): ResourceBindingsMap = resourceBindingsMap.get

  def register(m: RawModule, r: OMRegistrar): Unit = {
    registry += (RawModuleContainer(m) -> r)
  }

  def get(m: RawModuleContainer): Option[OMRegistrar] = {
    registry.get(m)
  }
}

case class LogicalTreeEdge(
  parent: RawModuleContainer,
  child: RawModuleContainer
)

case object LogicalModuleTree {
  val edges = List[LogicalTreeEdge]()

  private def cycleCheck(): Boolean = false

  def getTreeMap(): Map[RawModuleContainer, List[RawModuleContainer]] = {
    edges.groupBy(_.parent).map{ case (k, v) => (k, v.map(_.child))}
  }

  def findRoot(): LogicalTreeEdge = {
    val roots = edges.map {
      case r if r.parent == None => r
    }
    assert(roots.size == 1)
    roots.head
  }
}

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: Option[A], children: List[Tree[A]]) extends Tree[A]

object OMRegistrarTree {
  def makeTree(): Tree[OMRegistrar] = {
    val root = LogicalModuleTree.findRoot()
    val treeMap = LogicalModuleTree.getTreeMap()

    def tree(m: RawModuleContainer): Tree[OMRegistrar] = {
      val r = OMRegistry.get(m)
      require(r.isDefined, s"Registrar is not defined for rawmodule")
      val children = treeMap.getOrElse(m, Nil)

      children match {
        case Nil => Leaf(r.get)
        case cs => Branch(r, cs.map(tree(_)))
      }
    }
    tree(root.parent)
  }
}



object OMTree {
  def size_bad(t: Tree[OMRegistrar]): OMRegistrar =
    t match {
      case Leaf(a) => 1
      case Branch(a, b) => size_bad(a) + size_bad(b) + 1
    }

  def size(t: Tree[OMRegistrar]): OMRegistrar = {
    @tailrec
    def inner_size(l: List[Tree[OMRegistrar]], acc: OMRegistrar): OMRegistrar =
      l match {
        case Nil => acc
        case Leaf(v) :: ls => inner_size(ls, acc + 1)
        case Branch(a, b) :: ls => inner_size(a :: b :: ls, acc + 1)
      }
    inner_size(List(t), 0)
  }
}

case object Foo {
  def addOMArtefacts(): Unit = {
    //    val domComponents = getObjectModel()
    //    ElaborationArtefacts.add("objectModel1.json", DiplomaticObjectModelUtils.toJson(domComponents))
  }

}

case object OMPipeline {
  val registrarTree: Tree[OMRegistrar] = OMRegistrarTree.makeTree()


}