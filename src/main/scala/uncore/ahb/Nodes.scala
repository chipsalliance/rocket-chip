// See LICENSE.SiFive for license details.

package uncore.ahb

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._

object AHBImp extends NodeImp[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBEdgeParameters, AHBBundle]
{
  def edgeO(pd: AHBMasterPortParameters, pu: AHBSlavePortParameters): AHBEdgeParameters = AHBEdgeParameters(pd, pu)
  def edgeI(pd: AHBMasterPortParameters, pu: AHBSlavePortParameters): AHBEdgeParameters = AHBEdgeParameters(pd, pu)
  def bundleO(eo: Seq[AHBEdgeParameters]): Vec[AHBBundle] = {
    require (!eo.isEmpty)
    Vec(eo.size, AHBBundle(eo.map(_.bundle).reduce(_.union(_))))
  }
  def bundleI(ei: Seq[AHBEdgeParameters]): Vec[AHBBundle] = {
    require (!ei.isEmpty)
    Vec(ei.size, AHBBundle(ei.map(_.bundle).reduce(_.union(_))))
  }

  def colour = "#00ccff" // bluish
  override def labelI(ei: AHBEdgeParameters) = (ei.slave.beatBytes * 8).toString
  override def labelO(eo: AHBEdgeParameters) = (eo.slave.beatBytes * 8).toString

  def connect(bo: => AHBBundle, bi: => AHBBundle, ei: => AHBEdgeParameters)(implicit p: Parameters, sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
    (None, () => { bi <> bo })
  }

  override def mixO(pd: AHBMasterPortParameters, node: OutwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]): AHBMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AHBSlavePortParameters, node: InwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]): AHBSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class AHBIdentityNode() extends IdentityNode(AHBImp)
case class AHBMasterNode(portParams: AHBMasterPortParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SourceNode(AHBImp)(portParams, numPorts)
case class AHBSlaveNode(portParams: AHBSlavePortParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SinkNode(AHBImp)(portParams, numPorts)
case class AHBAdapterNode(
  masterFn:       Seq[AHBMasterPortParameters]  => AHBMasterPortParameters,
  slaveFn:        Seq[AHBSlavePortParameters] => AHBSlavePortParameters,
  numMasterPorts: Range.Inclusive = 1 to 1,
  numSlavePorts:  Range.Inclusive = 1 to 1)
  extends InteriorNode(AHBImp)(masterFn, slaveFn, numMasterPorts, numSlavePorts)

// Nodes passed from an inner module
case class AHBOutputNode() extends OutputNode(AHBImp)
case class AHBInputNode() extends InputNode(AHBImp)

// Nodes used for external ports
case class AHBBlindOutputNode(portParams: Seq[AHBSlavePortParameters]) extends BlindOutputNode(AHBImp)(portParams)
case class AHBBlindInputNode(portParams: Seq[AHBMasterPortParameters]) extends BlindInputNode(AHBImp)(portParams)

case class AHBInternalOutputNode(portParams: Seq[AHBSlavePortParameters]) extends InternalOutputNode(AHBImp)(portParams)
case class AHBInternalInputNode(portParams: Seq[AHBMasterPortParameters]) extends InternalInputNode(AHBImp)(portParams)
