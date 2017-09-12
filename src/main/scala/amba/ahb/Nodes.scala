// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.diplomacy._

object AHBImp extends NodeImp[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBEdgeParameters, AHBBundle]
{
  def edgeO(pd: AHBMasterPortParameters, pu: AHBSlavePortParameters): AHBEdgeParameters = AHBEdgeParameters(pd, pu)
  def edgeI(pd: AHBMasterPortParameters, pu: AHBSlavePortParameters): AHBEdgeParameters = AHBEdgeParameters(pd, pu)

  def bundleO(eo: AHBEdgeParameters): AHBBundle = AHBBundle(eo.bundle)
  def bundleI(ei: AHBEdgeParameters): AHBBundle = AHBBundle(ei.bundle)

  def colour = "#00ccff" // bluish
  override def labelI(ei: AHBEdgeParameters) = (ei.slave.beatBytes * 8).toString
  override def labelO(eo: AHBEdgeParameters) = (eo.slave.beatBytes * 8).toString

  override def mixO(pd: AHBMasterPortParameters, node: OutwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]): AHBMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AHBSlavePortParameters, node: InwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]): AHBSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class AHBIdentityNode()(implicit valName: ValName) extends IdentityNode(AHBImp)
case class AHBMasterNode(portParams: Seq[AHBMasterPortParameters])(implicit valName: ValName) extends SourceNode(AHBImp)(portParams)
case class AHBSlaveNode(portParams: Seq[AHBSlavePortParameters])(implicit valName: ValName) extends SinkNode(AHBImp)(portParams)
case class AHBNexusNode(
  masterFn:       Seq[AHBMasterPortParameters] => AHBMasterPortParameters,
  slaveFn:        Seq[AHBSlavePortParameters]  => AHBSlavePortParameters,
  numMasterPorts: Range.Inclusive = 1 to 999,
  numSlavePorts:  Range.Inclusive = 1 to 999)(
  implicit valName: ValName)
  extends NexusNode(AHBImp)(masterFn, slaveFn, numMasterPorts, numSlavePorts)

// Nodes passed from an inner module
case class AHBOutputNode()(implicit valName: ValName) extends OutputNode(AHBImp)
case class AHBInputNode()(implicit valName: ValName) extends InputNode(AHBImp)

// Nodes used for external ports
case class AHBBlindOutputNode(portParams: Seq[AHBSlavePortParameters])(implicit valName: ValName) extends BlindOutputNode(AHBImp)(portParams)
case class AHBBlindInputNode(portParams: Seq[AHBMasterPortParameters])(implicit valName: ValName) extends BlindInputNode(AHBImp)(portParams)

case class AHBInternalOutputNode(portParams: Seq[AHBSlavePortParameters])(implicit valName: ValName) extends InternalOutputNode(AHBImp)(portParams)
case class AHBInternalInputNode(portParams: Seq[AHBMasterPortParameters])(implicit valName: ValName) extends InternalInputNode(AHBImp)(portParams)
