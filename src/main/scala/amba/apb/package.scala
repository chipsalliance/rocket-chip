// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import Chisel._
import freechips.rocketchip.diplomacy._

package object apb
{
  type APBOutwardNode = OutwardNodeHandle[APBMasterPortParameters, APBSlavePortParameters, APBBundle]
  type APBInwardNode = InwardNodeHandle[APBMasterPortParameters, APBSlavePortParameters, APBBundle]
  type APBNode = SimpleNodeHandle[APBMasterPortParameters, APBSlavePortParameters, APBBundle]
}
