// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object tilelink
{
  type TLInwardNode = InwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle]
  type TLOutwardNode = OutwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]
  type TLNode = NodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]
}
