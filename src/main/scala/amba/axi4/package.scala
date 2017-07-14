// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import Chisel._
import freechips.rocketchip.diplomacy.OutwardNodeHandle

package object axi4
{
  type AXI4OutwardNode = OutwardNodeHandle[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]
}
