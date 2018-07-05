// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import freechips.rocketchip.diplomacy._

package object NAMESPACE
{
  type NAMESPACEOutwardNode = OutwardNodeHandle[NAMESPACENullParameters, NAMESPACESinkParameters, NAMESPACESinkParameters, NAMESPACEBundle]
  type NAMESPACEInwardNode = InwardNodeHandle[NAMESPACENullParameters, NAMESPACESinkParameters, NAMESPACESinkParameters, NAMESPACEBundle]
  type NAMESPACENode = SimpleNodeHandle[NAMESPACENullParameters, NAMESPACESinkParameters, NAMESPACESinkParameters, NAMESPACEBundle]
}
