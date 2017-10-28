// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object tilelink
{
  type TLInwardNode = InwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLBundle]
  type TLOutwardNode = OutwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLBundle]
  type TLNode = SimpleNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLBundle]
}
