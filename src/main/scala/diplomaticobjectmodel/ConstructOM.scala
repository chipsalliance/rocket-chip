// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import freechips.rocketchip.util.ElaborationArtefacts

case object ConstructOM {
  def constructOM(resourceBindingsMap: ResourceBindingsMap): Unit = {
    val tree: Tree[LogicalTreeNode] = OMLogicalTree.makeTree()
    val om: Seq[OMComponent] = OMTree.tree(tree, resourceBindingsMap)
    ElaborationArtefacts.add("objectModel1.json", DiplomaticObjectModelUtils.toJson(om))
  }
}