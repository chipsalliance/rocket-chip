// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object interrupts
{
  type IntInwardNode = InwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntNode = SimpleNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]

  implicit class IntClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: IntInwardNode) (implicit valName: ValName) = IntInwardCrossingHelper(valName.name, x, n)
    def crossOut(n: IntOutwardNode)(implicit valName: ValName) = IntOutwardCrossingHelper(valName.name, x, n)
    def cross(n: IntInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: IntOutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class SeqIntEdge(private val edges: Seq[IntEdge]) {
    /** Scan over interrupt edges calculating a running total of the number of interrupts. */
    def runningTotal: Seq[Int] = {
      edges.map(_.num).scanLeft(0)(_+_).init
    }

    /** Take a sequence of interrupts from several edges and combine them
      * into one flat sequence with their ranges stacked on top of each other.
      */
    def flattenSources(offset: Int): Seq[IntSourceParameters] = {
      edges
        .map(_.resolvedSources)
        .zip(edges.runningTotal)
        .map { case (sources, edgeOffset) =>
          sources.map(z => z.copy(rangeOpt = z.rangeOpt.map(_.offset(edgeOffset+offset))))
        }.flatten
    }
  }
}
