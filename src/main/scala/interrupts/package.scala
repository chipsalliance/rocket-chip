// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object interrupts
{
  type IntInwardNode = InwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntNode = SimpleNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
}
