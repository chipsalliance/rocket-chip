// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import org.chipsalliance.diplomacy.nodes.{InwardNodeHandle, OutwardNodeHandle, SimpleNodeHandle}

package object apb
{
  type APBOutwardNode = OutwardNodeHandle[APBManagerPortParameters, APBSubordinatePortParameters, APBEdgeParameters, APBBundle]
  type APBInwardNode = InwardNodeHandle[APBManagerPortParameters, APBSubordinatePortParameters, APBEdgeParameters, APBBundle]
  type APBNode = SimpleNodeHandle[APBManagerPortParameters, APBSubordinatePortParameters, APBEdgeParameters, APBBundle]
}
