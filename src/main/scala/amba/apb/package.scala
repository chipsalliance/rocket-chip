// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import Chisel._
import freechips.rocketchip.diplomacy.OutwardNodeHandle

package object apb
{
  type APBOutwardNode = OutwardNodeHandle[APBMasterPortParameters, APBSlavePortParameters, APBBundle]
}
