// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object IntImp extends SimpleNodeImp[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
{
  def edge(pd: IntSourcePortParameters, pu: IntSinkPortParameters, p: Parameters, sourceInfo: SourceInfo) = IntEdge(pd, pu, p, sourceInfo)
  def bundle(e: IntEdge) = Vec(e.source.num, Bool())
  def render(e: IntEdge) = RenderedEdge(colour = "#0000ff" /* blue */, label = e.source.sources.map(_.range.size).sum.toString, flipped = true)

  override def mixO(pd: IntSourcePortParameters, node: OutwardNode[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]): IntSourcePortParameters =
   pd.copy(sources = pd.sources.map  { s => s.copy (nodePath = node +: s.nodePath) })
  override def mixI(pu: IntSinkPortParameters, node: InwardNode[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]): IntSinkPortParameters =
   pu.copy(sinks   = pu.sinks.map    { s => s.copy (nodePath = node +: s.nodePath) })
}

case class IntSourceNode(portParams: Seq[IntSourcePortParameters])(implicit valName: ValName) extends SourceNode(IntImp)(portParams)
case class IntSinkNode(portParams: Seq[IntSinkPortParameters])(implicit valName: ValName) extends SinkNode(IntImp)(portParams)
case class IntAdapterNode(
  sourceFn: IntSourcePortParameters => IntSourcePortParameters = { s => s },
  sinkFn:   IntSinkPortParameters   => IntSinkPortParameters   = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(IntImp)(sourceFn, sinkFn)
case class IntIdentityNode()(implicit valName: ValName) extends IdentityNode(IntImp)()
case class IntEphemeralNode()(implicit valName: ValName) extends EphemeralNode(IntImp)()

object IntNameNode {
  def apply(name: ValName) = IntIdentityNode()(name)
  def apply(name: Option[String]): IntIdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): IntIdentityNode = apply(Some(name))
}

case class IntNexusNode(
  sourceFn:       Seq[IntSourcePortParameters] => IntSourcePortParameters,
  sinkFn:         Seq[IntSinkPortParameters]   => IntSinkPortParameters,
  inputRequiresOutput: Boolean = true,
  outputRequiresInput: Boolean = true)(
  implicit valName: ValName)
  extends NexusNode(IntImp)(sourceFn, sinkFn, inputRequiresOutput, outputRequiresInput)

object IntSyncImp extends SimpleNodeImp[IntSourcePortParameters, IntSinkPortParameters, IntEdge, SyncInterrupts]
{
  def edge(pd: IntSourcePortParameters, pu: IntSinkPortParameters, p: Parameters, sourceInfo: SourceInfo) = IntEdge(pd, pu, p, sourceInfo)
  def bundle(e: IntEdge) = new SyncInterrupts(e)
  def render(e: IntEdge) = RenderedEdge(colour = "#ff00ff" /* purple */, label = e.source.sources.map(_.range.size).sum.toString, flipped = true)

  override def mixO(pd: IntSourcePortParameters, node: OutwardNode[IntSourcePortParameters, IntSinkPortParameters, SyncInterrupts]): IntSourcePortParameters =
   pd.copy(sources = pd.sources.map  { s => s.copy (nodePath = node +: s.nodePath) })
  override def mixI(pu: IntSinkPortParameters, node: InwardNode[IntSourcePortParameters, IntSinkPortParameters, SyncInterrupts]): IntSinkPortParameters =
   pu.copy(sinks   = pu.sinks.map    { s => s.copy (nodePath = node +: s.nodePath) })
}

case class IntSyncIdentityNode()(implicit valName: ValName) extends IdentityNode(IntSyncImp)()

object IntSyncNameNode {
  def apply(name: ValName) = IntSyncIdentityNode()(name)
  def apply(name: Option[String]): IntSyncIdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): IntSyncIdentityNode = apply(Some(name))
}

case class IntSyncSourceNode(alreadyRegistered: Boolean)(implicit valName: ValName)
  extends MixedAdapterNode(IntImp, IntSyncImp)(
    dFn = { p => p },
    uFn = { p => p })
{
  override lazy val nodedebugstring = s"alreadyRegistered:${alreadyRegistered}"
}

case class IntSyncSinkNode(sync: Int)(implicit valName: ValName)
  extends MixedAdapterNode(IntSyncImp, IntImp)(
    dFn = { p => p },
    uFn = { p => p })
{
  override lazy val nodedebugstring = s"sync:${sync}"
}
