// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import freechips.rocketchip.diplomacy._

package object axis
{
  type AXISInwardNode = InwardNodeHandle[AXISMasterPortParameters, AXISSlavePortParameters, AXISEdgeParameters, AXISBundle]
  type AXISOutwardNode = OutwardNodeHandle[AXISMasterPortParameters, AXISSlavePortParameters, AXISEdgeParameters, AXISBundle]
  type AXISNode = NodeHandle[AXISMasterPortParameters, AXISSlavePortParameters, AXISEdgeParameters, AXISBundle, AXISMasterPortParameters, AXISSlavePortParameters, AXISEdgeParameters, AXISBundle]
}
