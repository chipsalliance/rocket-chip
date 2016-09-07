package uncore

import Chisel._

package object tilelink2
{
  type TLBaseNode = BaseNode[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
  def OH1ToUInt(x: UInt) = OHToUInt((x << 1 | UInt(1)) ^ x)
  def UIntToOH1(x: UInt, width: Int) = ~(SInt(-1, width=width).asUInt << x)(width-1, 0)
}
