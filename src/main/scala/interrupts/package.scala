// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object interrupts
{
  type IntInwardNode = InwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]
  type IntOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]
  type IntSyncInwardNode = InwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, SyncInterrupts]
  type IntSyncOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, SyncInterrupts]
}
