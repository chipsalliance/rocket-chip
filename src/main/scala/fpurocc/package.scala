// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import Chisel._
import freechips.rocketchip.diplomacy._

package object NAMESPACE
{
  type NAMESPACEOutwardNode = OutwardNodeHandle[NAMESPACEMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBBundle]
  type NAMESPACEInwardNode = InwardNodeHandle[NAMESPACEMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBBundle]
  type NAMESPACENode = SimpleNodeHandle[NAMESPACEMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBBundle]
}
