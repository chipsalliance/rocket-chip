// See LICENSE.SiFive for license details.

package uncore

import Chisel._
import diplomacy._

package object apb
{
  type APBOutwardNode = OutwardNodeHandle[APBMasterPortParameters, APBSlavePortParameters, APBBundle]
}
