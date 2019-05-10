// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import freechips.rocketchip.util.ElaborationArtefacts

case object ConstructOM {
  def constructOM(): Unit = {
    val om: Seq[OMComponent] = LogicalModuleTree.bind()
    ElaborationArtefacts.add("objectModel.json", DiplomaticObjectModelUtils.toJson(om))
  }
}
