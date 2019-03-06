// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelUtils
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import freechips.rocketchip.util.ElaborationArtefacts

case object OMProcessor {
  def process(resourceBindingsMap: ResourceBindingsMap): Unit = {
    val tree: Tree[LogicalTree] = OMLogicalTreeTree.makeTree()
    val om: Seq[OMComponent] = OMTree.tree(tree, resourceBindingsMap)
    ElaborationArtefacts.add("objectModel1.json", DiplomaticObjectModelUtils.toJson(om))
  }
}