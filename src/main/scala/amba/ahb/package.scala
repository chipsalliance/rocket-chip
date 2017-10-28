// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import Chisel._
import freechips.rocketchip.diplomacy._

package object ahb
{
  type AHBOutwardNode = OutwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]
  type AHBInwardNode = InwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]
  type AHBNode = SimpleNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]
}
