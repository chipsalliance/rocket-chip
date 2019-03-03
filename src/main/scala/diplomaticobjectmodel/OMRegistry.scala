// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3.experimental.RawModule
import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelUtils
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import freechips.rocketchip.tile.OMRegistry.registry
import freechips.rocketchip.util.ElaborationArtefacts

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait OMRegistrar {
  def getOMComponents(): Seq[OMComponent]
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
  val edges = ArrayBuffer[LogicalTreeEdge]()

  def add(parent: RawModule, child: RawModule): Unit = {
     edges += LogicalTreeEdge(RawModuleContainer(parent), RawModuleContainer(child))
  }

  private def cycleCheck(): Boolean = false

  def getTreeMap(): Map[RawModuleContainer, ArrayBuffer[RawModuleContainer]] = {
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

case class RawModuleParentRegistrarChildEdge(
  parent: RawModuleContainer,
  child: OMRegistrar
)

/**
  * A map of parent raw modules to child registrars
  * Use for leaves which are not raw modules
  */
case object OMParentToChildRegistry {

  private val registry = mutable.Map[RawModuleContainer, ArrayBuffer[OMRegistrar]]()

  def register(m: RawModule, r: OMRegistrar): Unit = {
    val c = RawModuleContainer(m)
    if (!registry.contains(c)) {
      registry + (c -> ArrayBuffer[OMRegistrar](r))
    }
    else {
      var l = registry.get(c)
      l.map(_ += r)
    }
  }

  def contains(m: RawModuleContainer): Boolean = {
    registry.contains(m)
  }

  def get(m: RawModuleContainer): Option[ArrayBuffer[OMRegistrar]] = {
    registry.get(m)
  }

  def makeChildren(rmc: RawModuleContainer): List[Tree[OMRegistrar]] = {
    val o = registry.get(rmc)
    o.map{l => l.map(Leaf(_))}.getOrElse(Nil).toList
  }
}

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
        case Nil =>
          if (OMParentToChildRegistry.contains(m)) {
            Branch(r, OMParentToChildRegistry.makeChildren(m))
          }
          else {
            Leaf(r.get)
          }
        case cs =>
          val l: List[Tree[OMRegistrar]] = OMParentToChildRegistry.makeChildren(m)
          Branch(r, cs.map(tree(_)).toList ++ l)
      }
    }
    tree(root.parent)
  }
}

//object OMTree {
//  def tree(t: Tree[OMRegistrar]): List[OMComponent] =
//    t match {
//      case Leaf(r) => r.getOMComponents() // a.getOMComponents()
//      case Branch(r, cs) => r.map{
//        case r =>
//          val components = cs.flatMap(tree(_))
//          r.getOMComponents()
//      }.get
//    }
//}
//
//case object OMPipeline {
//  def process(): Unit = {
//    val registrarTree: Tree[OMRegistrar] = OMRegistrarTree.makeTree()
//    val om = OMTree.tree(registrarTree)
//  }
//
//  def  addOMArtefacts(): Unit = {
//    val domComponents = process()
//    ElaborationArtefacts.add("objectModel1.json", DiplomaticObjectModelUtils.toJson(domComponents))
//  }
//}