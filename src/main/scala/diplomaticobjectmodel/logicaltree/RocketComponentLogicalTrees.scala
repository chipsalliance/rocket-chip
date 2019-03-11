package diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.{ResourceBindingsMap, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

class CLINTLogicalTree(device: SimpleDevice) extends LogicalTree {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    device.getOMComponents(resourceBindingsMap)
  }
}

class PLICLogicalTree(device: SimpleDevice) extends LogicalTree {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    device.getOMComponents(resourceBindingsMap)
  }
}


class DebugLogicalTree(device: SimpleDevice) extends LogicalTree {
  override def getOMComponents(resourceBindingsMap: ResourceBindingsMap, components: Seq[OMComponent]): Seq[OMComponent] = {
    device.getOMComponents(resourceBindingsMap)
  }
}

