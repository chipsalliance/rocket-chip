// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._

abstract class LookupByClusterIdImpl {
  def apply[T <: Data](f: BaseClusterParams => Option[T], clusterId: UInt): T
}

case class ClustersWontDeduplicate(t: BaseClusterParams) extends LookupByClusterIdImpl {
  def apply[T <: Data](f: BaseClusterParams => Option[T], clusterId: UInt): T = f(t).get
}

case class PriorityMuxClusterIdFromSeq(seq: Seq[BaseClusterParams]) extends LookupByClusterIdImpl {
  def apply[T <: Data](f: BaseClusterParams => Option[T], clusterId: UInt): T =
    PriorityMux(seq.collect { case t if f(t).isDefined => (t.clusterId.U === clusterId) -> f(t).get })
}
