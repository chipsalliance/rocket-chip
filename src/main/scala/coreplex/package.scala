// See LICENSE.SiFive for license details.

package freechips.rocketchip

import freechips.rocketchip.tilelink.TLOutwardNode
import freechips.rocketchip.interrupts.IntOutwardNode

package object coreplex
{
  implicit class TLCrossableNode(val node: TLOutwardNode)
  implicit class IntCrossableNode(val node: IntOutwardNode)
}
