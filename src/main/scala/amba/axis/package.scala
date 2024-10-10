// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import org.chipsalliance.diplomacy.nodes.{InwardNodeHandle, OutwardNodeHandle, NodeHandle}

package object axis
{
  type AXISInwardNode = InwardNodeHandle[AXISManagerPortParameters, AXISSubordinatePortParameters, AXISEdgeParameters, AXISBundle]
  type AXISOutwardNode = OutwardNodeHandle[AXISManagerPortParameters, AXISSubordinatePortParameters, AXISEdgeParameters, AXISBundle]
  type AXISNode = NodeHandle[AXISManagerPortParameters, AXISSubordinatePortParameters, AXISEdgeParameters, AXISBundle, AXISManagerPortParameters, AXISSubordinatePortParameters, AXISEdgeParameters, AXISBundle]
}
