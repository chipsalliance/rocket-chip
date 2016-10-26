// See LICENSE for license details.

package uncore.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._

object AXI4Imp extends NodeImp[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle]
{
  def edgeO(pd: AXI4MasterPortParameters, pu: AXI4SlavePortParameters): AXI4EdgeParameters = AXI4EdgeParameters(pd, pu)
  def edgeI(pd: AXI4MasterPortParameters, pu: AXI4SlavePortParameters): AXI4EdgeParameters = AXI4EdgeParameters(pd, pu)
  def bundleO(eo: Seq[AXI4EdgeParameters]): Vec[AXI4Bundle] = {
    require (!eo.isEmpty)
    Vec(eo.size, AXI4Bundle(eo.map(_.bundle).reduce(_.union(_))))
  }
  def bundleI(ei: Seq[AXI4EdgeParameters]): Vec[AXI4Bundle] = {
    require (!ei.isEmpty)
    Vec(ei.size, AXI4Bundle(ei.map(_.bundle).reduce(_.union(_))))
  }

  def colour = "#00ccff" // bluish
  def connect(bo: => AXI4Bundle, bi: => AXI4Bundle, ei: => AXI4EdgeParameters)(implicit sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
    (None, () => { bi <> bo })
  }

  override def mixO(pd: AXI4MasterPortParameters, node: OutwardNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]): AXI4MasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AXI4SlavePortParameters, node: InwardNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]): AXI4SlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class AXI4IdentityNode() extends IdentityNode(AXI4Imp)
case class AXI4MasterNode(portParams: AXI4MasterPortParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SourceNode(AXI4Imp)(portParams, numPorts)
case class AXI4SlaveNode(portParams: AXI4SlavePortParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SinkNode(AXI4Imp)(portParams, numPorts)
case class AXI4AdapterNode(
  masterFn:       Seq[AXI4MasterPortParameters]  => AXI4MasterPortParameters,
  slaveFn:        Seq[AXI4SlavePortParameters] => AXI4SlavePortParameters,
  numMasterPorts: Range.Inclusive = 1 to 1,
  numSlavePorts:  Range.Inclusive = 1 to 1)
  extends InteriorNode(AXI4Imp)(masterFn, slaveFn, numMasterPorts, numSlavePorts)

// Nodes passed from an inner module
case class AXI4OutputNode() extends OutputNode(AXI4Imp)
case class AXI4InputNode() extends InputNode(AXI4Imp)

// Nodes used for external ports
case class AXI4BlindOutputNode(portParams: AXI4SlavePortParameters) extends BlindOutputNode(AXI4Imp)(portParams)
case class AXI4BlindInputNode(portParams: AXI4MasterPortParameters) extends BlindInputNode(AXI4Imp)(portParams)
