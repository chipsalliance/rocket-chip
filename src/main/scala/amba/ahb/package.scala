// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import Chisel._
import freechips.rocketchip.diplomacy.OutwardNodeHandle

package object ahb
{
  type AHBOutwardNode = OutwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]
}
