// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object AHBImpSlave extends SimpleNodeImp[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBSlaveBundle]
{
  def edge(pd: AHBMasterPortParameters, pu: AHBSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = AHBEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AHBEdgeParameters) = AHBSlaveBundle(e.bundle)
  def render(e: AHBEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: AHBMasterPortParameters, node: OutwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBSlaveBundle]): AHBMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AHBSlavePortParameters, node: InwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBSlaveBundle]): AHBSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

object AHBImpMaster extends SimpleNodeImp[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBMasterBundle]
{
  def edge(pd: AHBMasterPortParameters, pu: AHBSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = AHBEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AHBEdgeParameters) = AHBMasterBundle(e.bundle)
  def render(e: AHBEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: AHBMasterPortParameters, node: OutwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBMasterBundle]): AHBMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AHBSlavePortParameters, node: InwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBMasterBundle]): AHBSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class AHBMasterSourceNode(portParams: Seq[AHBMasterPortParameters])(implicit valName: ValName) extends SourceNode(AHBImpMaster)(portParams)
case class AHBSlaveSourceNode(portParams: Seq[AHBMasterPortParameters])(implicit valName: ValName) extends SourceNode(AHBImpSlave)(portParams)
case class AHBMasterSinkNode(portParams: Seq[AHBSlavePortParameters])(implicit valName: ValName) extends SinkNode(AHBImpMaster)(portParams)
case class AHBSlaveSinkNode(portParams: Seq[AHBSlavePortParameters])(implicit valName: ValName) extends SinkNode(AHBImpSlave)(portParams)
case class AHBMasterIdentityNode()(implicit valName: ValName) extends IdentityNode(AHBImpMaster)()
case class AHBSlaveIdentityNode()(implicit valName: ValName) extends IdentityNode(AHBImpSlave)()

case class AHBArbiterNode(
  masterFn:       Seq[AHBMasterPortParameters] => AHBMasterPortParameters,
  slaveFn:        Seq[AHBSlavePortParameters]  => AHBSlavePortParameters)(
  implicit valName: ValName)
  extends MixedNexusNode(AHBImpMaster, AHBImpSlave)(masterFn, slaveFn)

case class AHBFanoutNode(
  masterFn:       Seq[AHBMasterPortParameters] => AHBMasterPortParameters,
  slaveFn:        Seq[AHBSlavePortParameters]  => AHBSlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(AHBImpSlave)(masterFn, slaveFn)
