// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.rocket.MulDivParams

case class OMMulDiv(
  divideBitsPerCycle: Int,
  divideMaxLatency: Int,
  divideMinLatency: Int,
  multiplyBitsPerCycle: Int,
  multiplyFullyPipelined: Boolean,
  multiplyMaxLatency: Int,
  multiplyMinLatency: Int,
  _types: Seq[String] = Seq("OMMulDiv", "OMComponent", "OMCompoundType")
) extends OMComponent


object OMMulDiv {
  def makeOMI(md: MulDivParams, xLen: Int): OMMulDiv = {
    val mulMinLatency =
      if (md.mulUnroll > 0) {
        if (md.mulEarlyOut) { 2 }
        else { xLen/md.mulUnroll }
      }
      else { xLen }

    val divMinLatency =
      if (md.divUnroll > 0) {
        if (md.divEarlyOut) { 3 }
        else { 1 + xLen/md.divUnroll }
      }
      else { xLen }

    OMMulDiv(
      divideBitsPerCycle = md.divUnroll,
      divideMaxLatency = xLen / md.divUnroll,
      divideMinLatency = divMinLatency,
      multiplyBitsPerCycle = md.mulUnroll,
      multiplyFullyPipelined = md.mulUnroll == xLen,
      multiplyMaxLatency = xLen / md.mulUnroll,
      multiplyMinLatency = mulMinLatency
    )
  }
}
