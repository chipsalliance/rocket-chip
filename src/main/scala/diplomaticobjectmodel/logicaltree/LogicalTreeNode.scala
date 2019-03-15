// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

import scala.collection.mutable

trait LogicalTreeNode {
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
  parent: LogicalTreeNode,
  child: LogicalTreeNode
)

//object LogicalModuleTree {
//  val edges = ArrayBuffer[LogicalTreeEdge]()
//
//  def add(parent: LogicalTreeNode, child: LogicalTreeNode): Unit = {
//     edges += LogicalTreeEdge(parent, child)
//  }
//
//  /**
//    * The getTreeMap function constructs a tree by converting the list of LogicalTreeEdges into a Map which
//    * contains each of the parents and their children
//    *
//    * @return
//    */
//  def getTreeMap: Map[LogicalTreeNode, List[LogicalTreeNode]] = {
//    edges.groupBy(_.parent).map{ case (k, v) => (k, v.map(_.child).toList)}
//  }
//
//  def findRoot(treeMap: Map[LogicalTreeNode, List[LogicalTreeNode]]): LogicalTreeNode = {
//    val roots = treeMap.collect{ case (k, _) if !treeMap.exists(_._2.contains(k)) => k }
//    assert(roots.size == 1, "Logical Tree contains more than one root.")
//    roots.head
//  }
//}
//
//case class Tree[A](parent: A, children: List[Tree[A]])
//
//object OMLogicalTree {
//  def makeTree(): Tree[LogicalTreeNode] = {
//    val treeMap = LogicalModuleTree.getTreeMap
//    val root: LogicalTreeNode = LogicalModuleTree.findRoot(treeMap)
//
//    def recurse(treeNode: LogicalTreeNode): Tree[LogicalTreeNode] = {
//      Tree(treeNode, treeMap.getOrElse(treeNode, Nil).map(recurse(_)))
//    }
//    recurse(root)
//  }
//}
//
//object OMTree {
//  /**
//    * Child components are the OM children of the current node and are added to the current node.
//    *
//    * @param t
//    * @param resourceBindingsMap
//    * @return
//    */
//  def tree(t: Tree[LogicalTreeNode], resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
//    val childComponents =  t.children.flatMap(tree(_, resourceBindingsMap))
//    t.parent.getOMComponents(resourceBindingsMap, childComponents)
//  }
//}

object LogicalModuleTree {
  private val tree: mutable.Map[LogicalTreeNode, Seq[LogicalTreeNode]] = mutable.Map[LogicalTreeNode, Seq[LogicalTreeNode]]()
  def add(parent: LogicalTreeNode, child: LogicalTreeNode): Unit = {
    val edge = LogicalTreeEdge(parent, child)
    val xx1 = tree.get(edge.parent).map{
      case x => edge.child +: x
    }.getOrElse(Seq(edge.child))
    tree.put(edge.parent, xx1)
  }

  def root: LogicalTreeNode = {
    val roots = tree.collect { case (k, _) if !tree.exists(_._2.contains(k)) => k }
    assert(roots.size == 1, "Logical Tree contains more than one root.")
    roots.head
  }

  def bind(resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
    def recurse(node: LogicalTreeNode): Seq[OMComponent] = {
      node.getOMComponents(resourceBindingsMap, tree.get(node).getOrElse(Nil).flatMap(recurse))
    }
    recurse(root)
  }
}

