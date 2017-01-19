// See LICENSE.SiFive for license details.

package uncore.apb

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._

object APBImp extends NodeImp[APBMasterPortParameters, APBSlavePortParameters, APBEdgeParameters, APBEdgeParameters, APBBundle]
{
  def edgeO(pd: APBMasterPortParameters, pu: APBSlavePortParameters): APBEdgeParameters = APBEdgeParameters(pd, pu)
  def edgeI(pd: APBMasterPortParameters, pu: APBSlavePortParameters): APBEdgeParameters = APBEdgeParameters(pd, pu)
  def bundleO(eo: Seq[APBEdgeParameters]): Vec[APBBundle] = {
    require (!eo.isEmpty)
    Vec(eo.size, APBBundle(eo.map(_.bundle).reduce(_.union(_))))
  }
  def bundleI(ei: Seq[APBEdgeParameters]): Vec[APBBundle] = {
    require (!ei.isEmpty)
    Vec(ei.size, APBBundle(ei.map(_.bundle).reduce(_.union(_))))
  }

  def colour = "#00ccff" // bluish
  override def labelI(ei: APBEdgeParameters) = (ei.slave.beatBytes * 8).toString
  override def labelO(eo: APBEdgeParameters) = (eo.slave.beatBytes * 8).toString

  def connect(bo: => APBBundle, bi: => APBBundle, ei: => APBEdgeParameters)(implicit p: Parameters, sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
    (None, () => { bi <> bo })
  }

  override def mixO(pd: APBMasterPortParameters, node: OutwardNode[APBMasterPortParameters, APBSlavePortParameters, APBBundle]): APBMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: APBSlavePortParameters, node: InwardNode[APBMasterPortParameters, APBSlavePortParameters, APBBundle]): APBSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class APBIdentityNode() extends IdentityNode(APBImp)
case class APBMasterNode(portParams: APBMasterPortParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SourceNode(APBImp)(portParams, numPorts)
case class APBSlaveNode(portParams: APBSlavePortParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SinkNode(APBImp)(portParams, numPorts)
case class APBAdapterNode(
  masterFn:       Seq[APBMasterPortParameters]  => APBMasterPortParameters,
  slaveFn:        Seq[APBSlavePortParameters] => APBSlavePortParameters,
  numMasterPorts: Range.Inclusive = 1 to 1,
  numSlavePorts:  Range.Inclusive = 1 to 1)
  extends InteriorNode(APBImp)(masterFn, slaveFn, numMasterPorts, numSlavePorts)

// Nodes passed from an inner module
case class APBOutputNode() extends OutputNode(APBImp)
case class APBInputNode() extends InputNode(APBImp)

// Nodes used for external ports
case class APBBlindOutputNode(portParams: Seq[APBSlavePortParameters]) extends BlindOutputNode(APBImp)(portParams)
case class APBBlindInputNode(portParams: Seq[APBMasterPortParameters]) extends BlindInputNode(APBImp)(portParams)

case class APBInternalOutputNode(portParams: Seq[APBSlavePortParameters]) extends InternalOutputNode(APBImp)(portParams)
case class APBInternalInputNode(portParams: Seq[APBMasterPortParameters]) extends InternalInputNode(APBImp)(portParams)
