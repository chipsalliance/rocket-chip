// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

// PI = PortInputParameters
// PO = PortOutputParameters
// EI = EdgeInput
// EO = EdgeOutput
abstract class NodeImp[PO, PI, EO, EI, B <: Bundle]
{
  def edgeO(po: PO, pi: PI): EO
  def edgeI(po: PO, pi: PI): EI
  def bundleO(eo: Seq[EO]): Vec[B]
  def bundleI(ei: Seq[EI]): Vec[B]
  def connect(bo: B, eo: EO, bi: B, ei: EI)(implicit sourceInfo: SourceInfo): Unit
}

class BaseNode[PO, PI, EO, EI, B <: Bundle](imp: NodeImp[PO, PI, EO, EI, B])(
  private val clientFn:  Option[Seq[PO] => PO],
  private val managerFn: Option[Seq[PI] => PI],
  private val numClientPorts:  Range.Inclusive,
  private val numManagerPorts: Range.Inclusive)
{
  // At least 0 ports must be supported
  require (!numClientPorts.isEmpty)
  require (!numManagerPorts.isEmpty)
  require (numClientPorts.start >= 0)
  require (numManagerPorts.start >= 0)

  val noClients  = numClientPorts.size  == 1 && numClientPorts.contains(0)
  val noManagers = numManagerPorts.size == 1 && numManagerPorts.contains(0)

  require (noClients  || clientFn.isDefined)
  require (noManagers || managerFn.isDefined)

  private val accClientPorts  = ListBuffer[BaseNode[PO, PI, EO, EI, B]]()
  private val accManagerPorts = ListBuffer[BaseNode[PO, PI, EO, EI, B]]()
  private var clientRealized  = false
  private var managerRealized = false

  private lazy val clientPorts  = { clientRealized  = true; require (numClientPorts.contains(accClientPorts.size));   accClientPorts.result() }
  private lazy val managerPorts = { managerRealized = true; require (numManagerPorts.contains(accManagerPorts.size)); accManagerPorts.result() }
  private lazy val clientParams  : Option[PO] = clientFn.map(_(managerPorts.map(_.clientParams.get)))
  private lazy val managerParams : Option[PI] = managerFn.map(_(clientPorts.map(_.managerParams.get)))

  lazy val edgesOut = clientPorts.map  { n => imp.edgeO(clientParams.get, n.managerParams.get) }
  lazy val edgesIn  = managerPorts.map { n => imp.edgeI(n.clientParams.get, managerParams.get) }
  
  lazy val bundleOut = imp.bundleO(edgesOut)
  lazy val bundleIn  = imp.bundleI(edgesIn)

  def connectOut = bundleOut
  def connectIn = bundleIn

  // source.edge(sink)
  protected[tilelink2] def edge(x: BaseNode[PO, PI, EO, EI, B])(implicit sourceInfo: SourceInfo) = {
    require (!noClients)
    require (!clientRealized)
    require (!x.noManagers)
    require (!x.managerRealized)
    val i = x.accManagerPorts.size
    val o = accClientPorts.size
    accClientPorts += x
    x.accManagerPorts += this
    () => {
      imp.connect(connectOut(o), edgesOut(o), x.connectIn(i), x.edgesIn(i))
    }
  }
}

class TLClientNode(
  params: TLClientParameters,
  numPorts: Range.Inclusive = 1 to 1) extends BaseNode(TLImp)(
    clientFn  = Some {case Seq() => TLClientPortParameters(Seq(params))},
    managerFn = None,
    numClientPorts  = numPorts,
    numManagerPorts = 0 to 0)
{
  require(numPorts.end >= 1)
}

object TLClientNode
{
  def apply(
    params: TLClientParameters,
    numPorts: Range.Inclusive = 1 to 1) = new TLClientNode(params, numPorts)
}

class TLManagerNode(
  beatBytes: Int,
  params: TLManagerParameters, 
  numPorts: Range.Inclusive = 1 to 1) extends BaseNode(TLImp)(
    clientFn  = None,
    managerFn = Some {case Seq() => TLManagerPortParameters(Seq(params), beatBytes)},
    numClientPorts  = 0 to 0,
    numManagerPorts = numPorts)
{
  require(numPorts.end >= 1)
}

object TLManagerNode
{
  def apply(
    beatBytes: Int,
    params: TLManagerParameters,
    numPorts: Range.Inclusive = 1 to 1) = new TLManagerNode(beatBytes, params, numPorts)
}

class TLAdapterNode(
  clientFn:        Seq[TLClientPortParameters]  => TLClientPortParameters,
  managerFn:       Seq[TLManagerPortParameters] => TLManagerPortParameters,
  numClientPorts:  Range.Inclusive = 1 to 1,
  numManagerPorts: Range.Inclusive = 1 to 1) extends BaseNode(TLImp)(
    clientFn  = Some(clientFn),
    managerFn = Some(managerFn),
    numClientPorts  = numClientPorts,
    numManagerPorts = numManagerPorts)

object TLAdapterNode
{
  def apply(
    clientFn:        Seq[TLClientPortParameters]  => TLClientPortParameters,
    managerFn:       Seq[TLManagerPortParameters] => TLManagerPortParameters,
    numClientPorts:  Range.Inclusive = 1 to 1,
    numManagerPorts: Range.Inclusive = 1 to 1) = new TLAdapterNode(clientFn, managerFn, numClientPorts, numManagerPorts)
}

class TLOutputNode extends BaseNode(TLImp)(
    clientFn  = Some({case Seq(x) => x}),
    managerFn = Some({case Seq(x) => x}),
    numClientPorts  = 1 to 1,
    numManagerPorts = 1 to 1)
{
  override def connectOut = bundleOut
  override def connectIn  = bundleOut
}

object TLOutputNode
{
  def apply() = new TLOutputNode()
}

class TLInputNode extends BaseNode(TLImp)(
    clientFn  = Some({case Seq(x) => x}),
    managerFn = Some({case Seq(x) => x}),
    numClientPorts  = 1 to 1,
    numManagerPorts = 1 to 1)
{
  override def connectOut = bundleIn
  override def connectIn  = bundleIn
}

object TLInputNode
{
  def apply() = new TLInputNode()
}
