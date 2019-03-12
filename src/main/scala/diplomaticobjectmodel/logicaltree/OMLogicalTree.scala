// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import scala.collection.mutable.ArrayBuffer


trait LogicalTree {
  def getOMComponents(resourceBindingsMap: ResourceBindingsMap, children: Seq[OMComponent]): Seq[OMComponent]
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
    val keys = getTreeMap().keys.toSet

    val roots = values.diff(keys)

    assert(roots.size == 1, "Logical Tree contains more than one root.")
    roots.head
  }
}

case class Tree[A](parent: A, children: List[Tree[A]])

object OMLogicalTree {
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
  /**
    * Child components are the OM children of the current node and are added to the current node.
    *
    * @param t
    * @param resourceBindingsMap
    * @return
    */
  def tree(t: Tree[LogicalTree], resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
    val childComponents =  t.children.flatMap(tree(_, resourceBindingsMap))
    t.parent.getOMComponents(resourceBindingsMap, childComponents)
  }
}

