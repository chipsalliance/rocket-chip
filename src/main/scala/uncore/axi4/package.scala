package uncore

import Chisel._
import diplomacy._

package object axi4
{
  type AXI4OutwardNode = OutwardNodeHandle[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]
}
