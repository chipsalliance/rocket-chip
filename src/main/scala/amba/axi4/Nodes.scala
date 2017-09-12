// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object AXI4Imp extends NodeImp[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle]
{
  def edgeO(pd: AXI4MasterPortParameters, pu: AXI4SlavePortParameters): AXI4EdgeParameters = AXI4EdgeParameters(pd, pu)
  def edgeI(pd: AXI4MasterPortParameters, pu: AXI4SlavePortParameters): AXI4EdgeParameters = AXI4EdgeParameters(pd, pu)

  def bundleO(eo: AXI4EdgeParameters): AXI4Bundle = AXI4Bundle(eo.bundle)
  def bundleI(ei: AXI4EdgeParameters): AXI4Bundle = AXI4Bundle(ei.bundle)

  def colour = "#00ccff" // bluish
  override def labelI(ei: AXI4EdgeParameters) = (ei.slave.beatBytes * 8).toString
  override def labelO(eo: AXI4EdgeParameters) = (eo.slave.beatBytes * 8).toString

  override def mixO(pd: AXI4MasterPortParameters, node: OutwardNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]): AXI4MasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AXI4SlavePortParameters, node: InwardNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]): AXI4SlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class AXI4IdentityNode()(implicit valName: ValName) extends IdentityNode(AXI4Imp)
case class AXI4MasterNode(portParams: Seq[AXI4MasterPortParameters])(implicit valName: ValName) extends SourceNode(AXI4Imp)(portParams)
case class AXI4SlaveNode(portParams: Seq[AXI4SlavePortParameters])(implicit valName: ValName) extends SinkNode(AXI4Imp)(portParams)
case class AXI4AdapterNode(
  masterFn:  AXI4MasterPortParameters => AXI4MasterPortParameters,
  slaveFn:   AXI4SlavePortParameters  => AXI4SlavePortParameters,
  numPorts:  Range.Inclusive = 0 to 999)(
  implicit valName: ValName)
  extends AdapterNode(AXI4Imp)(masterFn, slaveFn, numPorts)

// Nodes passed from an inner module
case class AXI4OutputNode()(implicit valName: ValName) extends OutputNode(AXI4Imp)
case class AXI4InputNode()(implicit valName: ValName) extends InputNode(AXI4Imp)

// Nodes used for external ports
case class AXI4BlindOutputNode(portParams: Seq[AXI4SlavePortParameters])(implicit valName: ValName) extends BlindOutputNode(AXI4Imp)(portParams)
case class AXI4BlindInputNode(portParams: Seq[AXI4MasterPortParameters])(implicit valName: ValName) extends BlindInputNode(AXI4Imp)(portParams)

case class AXI4InternalOutputNode(portParams: Seq[AXI4SlavePortParameters])(implicit valName: ValName) extends InternalOutputNode(AXI4Imp)(portParams)
case class AXI4InternalInputNode(portParams: Seq[AXI4MasterPortParameters])(implicit valName: ValName) extends InternalInputNode(AXI4Imp)(portParams)
