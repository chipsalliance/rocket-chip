// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import Chisel._
import freechips.rocketchip.diplomacy._

package object ahb
{
  type AHBSlaveOutwardNode = OutwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBSlaveBundle]
  type AHBSlaveInwardNode = InwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBSlaveBundle]
  type AHBSlaveNode = SimpleNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBSlaveBundle]
  type AHBMasterOutwardNode = OutwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBMasterBundle]
  type AHBMasterInwardNode = InwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBMasterBundle]
  type AHBMasterNode = SimpleNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBMasterBundle]
}
