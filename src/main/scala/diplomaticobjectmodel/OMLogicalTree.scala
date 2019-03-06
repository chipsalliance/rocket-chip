// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import scala.collection.mutable.ArrayBuffer


trait LogicalTree {
  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent]): Seq[OMComponent]
}

/** LogicalTreeEdges hold LogicalTree Nodes which will be used to construct the logical tree.
  * The LogicalTreeEdge is used to construct a parent child relationship between different modules.
  *
  * @param parent The parent LogicalTree node
  * @param child The child LogicalTree node
  */
case class LogicalTreeEdge(
  parent: LogicalTree,
  child: LogicalTree
)

object LogicalModuleTree {
  val edges = ArrayBuffer[LogicalTreeEdge]()

  def add(parent: LogicalTree, child: LogicalTree): Unit = {
     edges += LogicalTreeEdge(parent, child)
  }

  /**
    * The getTreeMap function constructs a tree by converting the list of LogicalTreeEdges into a Map which
    * contains each of the parents and their children
    *
    * @return
    */
  def getTreeMap(): Map[LogicalTree, List[LogicalTree]] = {
    edges.groupBy(_.parent).map{ case (k, v) => (k, v.map(_.child).toList)}
  }

  def findRoot(): LogicalTree = {
    val values = getTreeMap().values.flatten.toSet

    val roots = getTreeMap().keys.map { case p if ! values.contains(p) => p}

    assert(roots.size == 1, "Logical Tree contains more than one root.")
    roots.head
  }
}

case class Tree[A](parent: A, children: List[Tree[A]])

object OMLogicalTreeTree {
  def makeTree(): Tree[LogicalTree] = {
    val root: LogicalTree = LogicalModuleTree.findRoot()
    val treeMap = LogicalModuleTree.getTreeMap()

    def recurse(treeNode: LogicalTree): Tree[LogicalTree] = {
      Tree(treeNode, treeMap.getOrElse(treeNode, Nil).map(recurse(_)))
    }
    recurse(root)
  }
}

object OMTree {
  def tree(t: Tree[LogicalTree], resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
    val components =  t.children.flatMap(tree(_, resourceBindingsMap))
    t.parent.getOMComponents(resourceBindingsMap, components)
  }
}
