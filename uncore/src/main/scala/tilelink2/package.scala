package uncore

import Chisel._

package object tilelink2
{
  type TLBaseNode = BaseNode[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
}
