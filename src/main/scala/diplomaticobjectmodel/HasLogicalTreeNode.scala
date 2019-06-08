// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel


import freechips.rocketchip.diplomaticobjectmodel.logicaltree._

trait HasLogicalTreeNode {
  def logicalTreeNode: LogicalTreeNode
  def addLogicalTreeNode(childLogicalTreeNode: LogicalTreeNode): Unit = LogicalModuleTree.add(logicalTreeNode, childLogicalTreeNode)
}

