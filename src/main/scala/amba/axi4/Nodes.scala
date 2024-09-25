// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.experimental.SourceInfo

import org.chipsalliance.cde.config.{Parameters, Field}

import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.nodes.{SimpleNodeImp, RenderedEdge, OutwardNode, InwardNode, SourceNode, SinkNode, NexusNode, AdapterNode, IdentityNode, MixedAdapterNode}

import freechips.rocketchip.util.AsyncQueueParams

case object AXI4MonitorBuilder extends Field[AXI4MonitorArgs => AXI4MonitorBase]

object AXI4Imp extends SimpleNodeImp[AXI4ManagerPortParameters, AXI4SubordinatePortParameters, AXI4EdgeParameters, AXI4Bundle]
{
  def edge(pd: AXI4ManagerPortParameters, pu: AXI4SubordinatePortParameters, p: Parameters, sourceInfo: SourceInfo) = AXI4EdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AXI4EdgeParameters) = AXI4Bundle(e.bundle)
  def render(e: AXI4EdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label  = (e.subordinate.beatBytes * 8).toString)

  override def monitor(bundle: AXI4Bundle, edge: AXI4EdgeParameters): Unit = {
    edge.params.lift(AXI4MonitorBuilder).foreach { builder =>
      val monitor = Module(builder(AXI4MonitorArgs(edge)))
      monitor.io.in := bundle
    }
  }

  override def mixO(pd: AXI4ManagerPortParameters, node: OutwardNode[AXI4ManagerPortParameters, AXI4SubordinatePortParameters, AXI4Bundle]): AXI4ManagerPortParameters  =
   pd.copy(managers = pd.managers.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AXI4SubordinatePortParameters, node: InwardNode[AXI4ManagerPortParameters, AXI4SubordinatePortParameters, AXI4Bundle]): AXI4SubordinatePortParameters =
   pu.copy(subordinates  = pu.subordinates.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class AXI4ManagerNode(portParams: Seq[AXI4ManagerPortParameters])(implicit valName: ValName) extends SourceNode(AXI4Imp)(portParams)
case class AXI4SubordinateNode(portParams: Seq[AXI4SubordinatePortParameters])(implicit valName: ValName) extends SinkNode(AXI4Imp)(portParams)
case class AXI4NexusNode(
  managerFn:       Seq[AXI4ManagerPortParameters] => AXI4ManagerPortParameters,
  subordinateFn:        Seq[AXI4SubordinatePortParameters]  => AXI4SubordinatePortParameters)(
  implicit valName: ValName)
  extends NexusNode(AXI4Imp)(managerFn, subordinateFn)
case class AXI4AdapterNode(
  managerFn:  AXI4ManagerPortParameters => AXI4ManagerPortParameters = { m => m },
  subordinateFn:   AXI4SubordinatePortParameters  => AXI4SubordinatePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(AXI4Imp)(managerFn, subordinateFn)
case class AXI4IdentityNode()(implicit valName: ValName) extends IdentityNode(AXI4Imp)()

object AXI4NameNode {
  def apply(name: ValName) = AXI4IdentityNode()(name)
  def apply(name: Option[String]): AXI4IdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): AXI4IdentityNode = apply(Some(name))
}

object AXI4AsyncImp extends SimpleNodeImp[AXI4AsyncManagerPortParameters, AXI4AsyncSubordinatePortParameters, AXI4AsyncEdgeParameters, AXI4AsyncBundle]
{
  def edge(pd: AXI4AsyncManagerPortParameters, pu: AXI4AsyncSubordinatePortParameters, p: Parameters, sourceInfo: SourceInfo) = AXI4AsyncEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AXI4AsyncEdgeParameters) = new AXI4AsyncBundle(e.bundle)
  def render(e: AXI4AsyncEdgeParameters) = RenderedEdge(colour = "#ff0000" /* red */, label = e.subordinate.async.depth.toString)

  override def mixO(pd: AXI4AsyncManagerPortParameters, node: OutwardNode[AXI4AsyncManagerPortParameters, AXI4AsyncSubordinatePortParameters, AXI4AsyncBundle]): AXI4AsyncManagerPortParameters  =
   pd.copy(base = pd.base.copy(managers = pd.base.managers.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: AXI4AsyncSubordinatePortParameters, node: InwardNode[AXI4AsyncManagerPortParameters, AXI4AsyncSubordinatePortParameters, AXI4AsyncBundle]): AXI4AsyncSubordinatePortParameters =
   pu.copy(base = pu.base.copy(subordinates  = pu.base.subordinates.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class AXI4AsyncSourceNode(sync: Option[Int])(implicit valName: ValName)
  extends MixedAdapterNode(AXI4Imp, AXI4AsyncImp)(
    dFn = { p => AXI4AsyncManagerPortParameters(p) },
    uFn = { p => p.base.copy(minLatency = p.base.minLatency + sync.getOrElse(p.async.sync)) })

case class AXI4AsyncSinkNode(async: AsyncQueueParams)(implicit valName: ValName)
  extends MixedAdapterNode(AXI4AsyncImp, AXI4Imp)(
    dFn = { p => p.base },
    uFn = { p => AXI4AsyncSubordinatePortParameters(async, p) })

case class AXI4AsyncIdentityNode()(implicit valName: ValName) extends IdentityNode(AXI4AsyncImp)()

object AXI4AsyncNameNode {
  def apply(name: ValName) = AXI4AsyncIdentityNode()(name)
  def apply(name: Option[String]): AXI4AsyncIdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): AXI4AsyncIdentityNode = apply(Some(name))
}

object AXI4CreditedImp extends SimpleNodeImp[AXI4CreditedManagerPortParameters, AXI4CreditedSubordinatePortParameters, AXI4CreditedEdgeParameters, AXI4CreditedBundle]
{
  def edge(pd: AXI4CreditedManagerPortParameters, pu: AXI4CreditedSubordinatePortParameters, p: Parameters, sourceInfo: SourceInfo) = AXI4CreditedEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AXI4CreditedEdgeParameters) = new AXI4CreditedBundle(e.bundle)
  def render(e: AXI4CreditedEdgeParameters) = RenderedEdge(colour = "#ffff00" /* yellow */, label = e.delay.toString)

  override def mixO(pd: AXI4CreditedManagerPortParameters, node: OutwardNode[AXI4CreditedManagerPortParameters, AXI4CreditedSubordinatePortParameters, AXI4CreditedBundle]): AXI4CreditedManagerPortParameters  =
   pd.copy(base = pd.base.copy(managers = pd.base.managers.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: AXI4CreditedSubordinatePortParameters, node: InwardNode[AXI4CreditedManagerPortParameters, AXI4CreditedSubordinatePortParameters, AXI4CreditedBundle]): AXI4CreditedSubordinatePortParameters =
   pu.copy(base = pu.base.copy(subordinates  = pu.base.subordinates.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class AXI4CreditedSourceNode(delay: AXI4CreditedDelay)(implicit valName: ValName)
  extends MixedAdapterNode(AXI4Imp, AXI4CreditedImp)(
    dFn = { p => AXI4CreditedManagerPortParameters(delay, p) },
    uFn = { p => p.base.copy(minLatency = 1) })

case class AXI4CreditedSinkNode(delay: AXI4CreditedDelay)(implicit valName: ValName)
  extends MixedAdapterNode(AXI4CreditedImp, AXI4Imp)(
    dFn = { p => p.base },
    uFn = { p => AXI4CreditedSubordinatePortParameters(delay, p) })

case class AXI4CreditedAdapterNode(
  managerFn: AXI4CreditedManagerPortParameters => AXI4CreditedManagerPortParameters = { s => s },
  subordinateFn:  AXI4CreditedSubordinatePortParameters  => AXI4CreditedSubordinatePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(AXI4CreditedImp)(managerFn, subordinateFn)

case class AXI4CreditedIdentityNode()(implicit valName: ValName) extends IdentityNode(AXI4CreditedImp)()

object AXI4CreditedNameNode {
  def apply(name: ValName) = AXI4CreditedIdentityNode()(name)
  def apply(name: Option[String]): AXI4CreditedIdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): AXI4CreditedIdentityNode = apply(Some(name))
}
