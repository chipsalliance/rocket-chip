// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

// PI = PortInputParameters
// PO = PortOutputParameters
// EI = EdgeInput
// EO = EdgeOutput
abstract class NodeImp[PO, PI, EO, EI, B <: Data]
{
  def edgeO(po: PO, pi: PI): EO
  def edgeI(po: PO, pi: PI): EI
  def bundleO(eo: Seq[EO]): Vec[B]
  def bundleI(ei: Seq[EI]): Vec[B]
  def connect(bo: => B, eo: => EO, bi: => B, ei: => EI)(implicit sourceInfo: SourceInfo): (Option[LazyModule], () => Unit)
  // If you want to track parameters as they flow through nodes, overload these:
  def mixO(po: PO, node: BaseNode[PO, PI, EO, EI, B]): PO = po
  def mixI(pi: PI, node: BaseNode[PO, PI, EO, EI, B]): PI = pi
}

abstract class RootNode
{
  // You cannot create a Node outside a LazyModule!
  require (!LazyModule.stack.isEmpty)

  val lazyModule = LazyModule.stack.head
  val index = lazyModule.nodes.size
  lazyModule.nodes = this :: lazyModule.nodes

  def name = lazyModule.name + "." + getClass.getName.split('.').last
  def colour = "blue"
  def omitGraphML = outputs.isEmpty && inputs.isEmpty

  protected[tilelink2] def outputs: Seq[RootNode]
  protected[tilelink2] def inputs:  Seq[RootNode]
}

class BaseNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])(
  private val oFn: (Int, Seq[PO]) => Seq[PO],
  private val iFn: (Int, Seq[PI]) => Seq[PI],
  private val numPO: Range.Inclusive,
  private val numPI: Range.Inclusive) extends RootNode
{
  // At least 0 ports must be supported
  require (!numPO.isEmpty, s"No number of outputs would be acceptable to ${name}${lazyModule.line}")
  require (!numPI.isEmpty, s"No number of inputs would be acceptable to ${name}${lazyModule.line}")
  require (numPO.start >= 0, s"${name} accepts a negative number of outputs${lazyModule.line}")
  require (numPI.start >= 0, s"${name} accepts a negative number of inputs${lazyModule.line}")

  val noOs = numPO.size == 1 && numPO.contains(0)
  val noIs = numPI.size == 1 && numPI.contains(0)

  private val accPO = ListBuffer[(Int, BaseNode[PO, PI, EO, EI, B])]()
  private val accPI = ListBuffer[(Int, BaseNode[PO, PI, EO, EI, B])]()
  private var oRealized  = false
  private var iRealized = false

  private def reqO() = require(numPO.contains(accPO.size), s"${name} has ${accPO.size} outputs, expected ${numPO}${lazyModule.line}")
  private def reqI() = require(numPI.contains(accPI.size), s"${name} has ${accPI.size} inputs, expected ${numPI}${lazyModule.line}")
  protected def reqE(o: Int, i: Int) = require(i == o, s"${name} has ${i} inputs and ${o} outputs; they must match${lazyModule.line}")

  private lazy val oPorts = { oRealized = true; reqO(); accPO.result() }
  private lazy val iPorts = { iRealized = true; reqI(); accPI.result() }

  protected[tilelink2] def outputs = oPorts.map(_._2)
  protected[tilelink2] def inputs  = iPorts.map(_._2)

  private lazy val oParams : Seq[PO] = {
    val o = oFn(oPorts.size, iPorts.map{ case (i, n) => n.oParams(i) })
    reqE(oPorts.size, o.size)
    o.map(imp.mixO(_, this))
  }
  private lazy val iParams : Seq[PI] = {
    val i = iFn(iPorts.size, oPorts.map{ case (o, n) => n.iParams(o) })
    reqE(i.size, iPorts.size)
    i.map(imp.mixI(_, this))
  }

  lazy val edgesOut = (oPorts zip oParams).map { case ((i, n), o) => imp.edgeO(o, n.iParams(i)) }
  lazy val edgesIn  = (iPorts zip iParams).map { case ((o, n), i) => imp.edgeI(n.oParams(o), i) }

  lazy val bundleOut = imp.bundleO(edgesOut)
  lazy val bundleIn  = imp.bundleI(edgesIn)

  def connectOut = bundleOut
  def connectIn = bundleIn

  def := (y: BaseNode[PO, PI, EO, EI, B])(implicit sourceInfo: SourceInfo): Option[LazyModule] = {
    val x = this // x := y
    val info = sourceLine(sourceInfo, " at ", "")
    require (!LazyModule.stack.isEmpty, s"${y.name} cannot be connected to ${x.name} outside of LazyModule scope" + info)
    require (!y.noOs, s"${y.name}${y.lazyModule.line} was incorrectly connected as a source" + info)
    require (!y.oRealized, s"${y.name}${y.lazyModule.line} was incorrectly connected as a source after it's .module was used" + info)
    require (!x.noIs, s"${x.name}${x.lazyModule.line} was incorrectly connected as a sink" + info)
    require (!x.iRealized, s"${x.name}${x.lazyModule.line} was incorrectly connected as a sink after it's .module was used" + info)
    val i = x.accPI.size
    val o = y.accPO.size
    y.accPO += ((i, x))
    x.accPI += ((o, y))
    val (out, binding) = imp.connect(y.connectOut(o), y.edgesOut(o), x.connectIn(i), x.edgesIn(i))
    LazyModule.stack.head.bindings = binding :: LazyModule.stack.head.bindings
    out
  }
}

class IdentityNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])
  extends BaseNode(imp)({case (_, s) => s}, {case (_, s) => s}, 0 to 999, 0 to 999)

class OutputNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B]) extends IdentityNode(imp)
{
  override def connectOut = bundleOut
  override def connectIn  = bundleOut
}

class InputNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B]) extends IdentityNode(imp)
{
  override def connectOut = bundleIn
  override def connectIn  = bundleIn
}

class SourceNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])(po: PO, num: Range.Inclusive = 1 to 1)
  extends BaseNode(imp)({case (n, Seq()) => Seq.fill(n)(po)}, {case (0, _) => Seq()}, num, 0 to 0)
{
  require (num.end >= 1, s"${name} is a source which does not accept outputs${lazyModule.line}")
}

class SinkNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])(pi: PI, num: Range.Inclusive = 1 to 1)
  extends BaseNode(imp)({case (0, _) => Seq()}, {case (n, Seq()) => Seq.fill(n)(pi)}, 0 to 0, num)
{
  require (num.end >= 1, s"${name} is a sink which does not accept inputs${lazyModule.line}")
}

class InteriorNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])
  (oFn: Seq[PO] => PO, iFn: Seq[PI] => PI, numPO: Range.Inclusive, numPI: Range.Inclusive)
  extends BaseNode(imp)({case (n,s) => Seq.fill(n)(oFn(s))}, {case (n,s) => Seq.fill(n)(iFn(s))}, numPO, numPI)
{
  require (numPO.end >= 1, s"${name} is an adapter which does not accept outputs${lazyModule.line}")
  require (numPI.end >= 1, s"${name} is an adapter which does not accept inputs${lazyModule.line}")
}
