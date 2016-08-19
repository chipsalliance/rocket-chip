// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer

class TLBaseNode(
  private val clientFn:  Option[Seq[TLClientPortParameters]  => TLClientPortParameters],
  private val managerFn: Option[Seq[TLManagerPortParameters] => TLManagerPortParameters],
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

  private val accClientPorts  = ListBuffer[TLBaseNode]()
  private val accManagerPorts = ListBuffer[TLBaseNode]()
  private var clientRealized  = false
  private var managerRealized = false

  protected[tilelink2] def edge(x: TLBaseNode) = {
    require (!noManagers)
    require (!managerRealized)
    require (!x.noClients)
    require (!x.clientRealized)
    val i = accManagerPorts.size
    val j = x.accClientPorts.size
    accManagerPorts += x
    x.accClientPorts += this
    (i, j)
  }

  private lazy val clientPorts  = { clientRealized  = true; require (numClientPorts.contains(accClientPorts.size));   accClientPorts.result() }
  private lazy val managerPorts = { managerRealized = true; require (numManagerPorts.contains(accManagerPorts.size)); accManagerPorts.result() }
  private lazy val clientParams  : Option[TLClientPortParameters]  = clientFn.map(_(managerPorts.map(_.clientParams.get)))
  private lazy val managerParams : Option[TLManagerPortParameters] = managerFn.map(_(clientPorts.map(_.managerParams.get)))

  lazy val edgesOut = clientPorts.map  { n => new TLEdgeOut(clientParams.get, n.managerParams.get) }
  lazy val edgesIn  = managerPorts.map { n => new TLEdgeIn (n.clientParams.get, managerParams.get) }
  
  lazy val bundleOut = Vec(edgesOut.size, new TLBundle(edgesOut.map(_.bundle).reduce(_.union(_))))
  lazy val bundleIn  = Vec(edgesIn .size, new TLBundle(edgesIn .map(_.bundle).reduce(_.union(_)))).flip
}

class TLClientNode(
  params: TLClientParameters,
  numPorts: Range.Inclusive = 1 to 1) extends TLBaseNode(
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
  numPorts: Range.Inclusive = 1 to 1) extends TLBaseNode(
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
  numManagerPorts: Range.Inclusive = 1 to 1) extends TLBaseNode(
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

class TLIDNode extends TLBaseNode(
    clientFn  = Some({case Seq(x) => x}),
    managerFn = Some({case Seq(x) => x}),
    numClientPorts  = 1 to 1,
    numManagerPorts = 1 to 1)

object TLIDNode
{
  def apply() = new TLIDNode()
}
