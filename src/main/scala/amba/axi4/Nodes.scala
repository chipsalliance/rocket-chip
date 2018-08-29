// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.AsyncQueueParams

object AXI4Imp extends SimpleNodeImp[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4Bundle]
{
  def edge(pd: AXI4MasterPortParameters, pu: AXI4SlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = AXI4EdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AXI4EdgeParameters) = AXI4Bundle(e.bundle)
  def render(e: AXI4EdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label  = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: AXI4MasterPortParameters, node: OutwardNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]): AXI4MasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AXI4SlavePortParameters, node: InwardNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]): AXI4SlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class AXI4MasterNode(portParams: Seq[AXI4MasterPortParameters])(implicit valName: ValName) extends SourceNode(AXI4Imp)(portParams)
case class AXI4SlaveNode(portParams: Seq[AXI4SlavePortParameters])(implicit valName: ValName) extends SinkNode(AXI4Imp)(portParams)
case class AXI4NexusNode(
  masterFn:       Seq[AXI4MasterPortParameters] => AXI4MasterPortParameters,
  slaveFn:        Seq[AXI4SlavePortParameters]  => AXI4SlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(AXI4Imp)(masterFn, slaveFn)
case class AXI4AdapterNode(
  masterFn:  AXI4MasterPortParameters => AXI4MasterPortParameters = { m => m },
  slaveFn:   AXI4SlavePortParameters  => AXI4SlavePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(AXI4Imp)(masterFn, slaveFn)
case class AXI4IdentityNode()(implicit valName: ValName) extends IdentityNode(AXI4Imp)()

object AXI4NameNode {
  def apply(name: ValName) = AXI4IdentityNode()(name)
  def apply(name: Option[String]): AXI4IdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): AXI4IdentityNode = apply(Some(name))
}

object AXI4AsyncImp extends SimpleNodeImp[AXI4AsyncMasterPortParameters, AXI4AsyncSlavePortParameters, AXI4AsyncEdgeParameters, AXI4AsyncBundle]
{
  def edge(pd: AXI4AsyncMasterPortParameters, pu: AXI4AsyncSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = AXI4AsyncEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AXI4AsyncEdgeParameters) = new AXI4AsyncBundle(e.bundle)
  def render(e: AXI4AsyncEdgeParameters) = RenderedEdge(colour = "#ff0000" /* red */, label = e.slave.async.depth.toString)

  override def mixO(pd: AXI4AsyncMasterPortParameters, node: OutwardNode[AXI4AsyncMasterPortParameters, AXI4AsyncSlavePortParameters, AXI4AsyncBundle]): AXI4AsyncMasterPortParameters  =
   pd.copy(base = pd.base.copy(masters = pd.base.masters.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: AXI4AsyncSlavePortParameters, node: InwardNode[AXI4AsyncMasterPortParameters, AXI4AsyncSlavePortParameters, AXI4AsyncBundle]): AXI4AsyncSlavePortParameters =
   pu.copy(base = pu.base.copy(slaves  = pu.base.slaves.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class AXI4AsyncSourceNode(sync: Option[Int])(implicit valName: ValName)
  extends MixedAdapterNode(AXI4Imp, AXI4AsyncImp)(
    dFn = { p => AXI4AsyncMasterPortParameters(p) },
    uFn = { p => p.base.copy(minLatency = p.base.minLatency + sync.getOrElse(p.async.sync)) })

case class AXI4AsyncSinkNode(async: AsyncQueueParams)(implicit valName: ValName)
  extends MixedAdapterNode(AXI4AsyncImp, AXI4Imp)(
    dFn = { p => p.base },
    uFn = { p => AXI4AsyncSlavePortParameters(async, p) })

case class AXI4AsyncIdentityNode()(implicit valName: ValName) extends IdentityNode(AXI4AsyncImp)()

object AXI4AsyncNameNode {
  def apply(name: ValName) = AXI4AsyncIdentityNode()(name)
  def apply(name: Option[String]): AXI4AsyncIdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): AXI4AsyncIdentityNode = apply(Some(name))
}
