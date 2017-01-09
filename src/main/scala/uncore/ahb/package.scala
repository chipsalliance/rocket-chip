// See LICENSE.SiFive for license details.

package uncore

import Chisel._
import diplomacy._

package object ahb
{
  type AHBOutwardNode = OutwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]
}
