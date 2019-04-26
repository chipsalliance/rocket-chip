// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import freechips.rocketchip.diplomacy.ResourceBindingsMap
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import freechips.rocketchip.util.ElaborationArtefacts

object ConstructOM {
  private var resourceBindingsMap: () => ResourceBindingsMap = _

  def setBindingsMap(rbm: () => ResourceBindingsMap): Unit = resourceBindingsMap = rbm

  def constructOM(): Unit = {
    val om: Seq[OMComponent] = LogicalModuleTree.bind(resourceBindingsMap())
    ElaborationArtefacts.add("objectModel.json", DiplomaticObjectModelUtils.toJson(om))
  }
}

