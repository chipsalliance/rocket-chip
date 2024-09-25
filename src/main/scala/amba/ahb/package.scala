// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import org.chipsalliance.diplomacy.nodes.{InwardNodeHandle, OutwardNodeHandle, SimpleNodeHandle}

package object ahb
{
  type AHBSubordinateOutwardNode = OutwardNodeHandle[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBEdgeParameters, AHBSubordinateBundle]
  type AHBSubordinateInwardNode = InwardNodeHandle[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBEdgeParameters, AHBSubordinateBundle]
  type AHBSubordinateNode = SimpleNodeHandle[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBEdgeParameters, AHBSubordinateBundle]
  type AHBManagerOutwardNode = OutwardNodeHandle[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBEdgeParameters, AHBManagerBundle]
  type AHBManagerInwardNode = InwardNodeHandle[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBEdgeParameters, AHBManagerBundle]
  type AHBManagerNode = SimpleNodeHandle[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBEdgeParameters, AHBManagerBundle]
}
