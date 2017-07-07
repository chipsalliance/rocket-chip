// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object tilelink
{
  type TLInwardNode = InwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLBundle]
  type TLOutwardNode = OutwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLBundle]
  type TLAsyncOutwardNode = OutwardNodeHandle[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]
  type TLRationalOutwardNode = OutwardNodeHandle[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalBundle]
  type IntOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]
  type TLMixedNode = MixedNode[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle,
                               TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]
}
