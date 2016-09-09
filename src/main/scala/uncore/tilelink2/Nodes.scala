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
  def connect(bo: B, eo: EO, bi: B, ei: EI)(implicit sourceInfo: SourceInfo): Unit
}

class RootNode
{
  // You cannot create a Node outside a LazyModule!
  require (!LazyModule.stack.isEmpty)

  val lazyModule = LazyModule.stack.head
  lazyModule.nodes = this :: lazyModule.nodes
}

class BaseNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])(
  private val oFn: Option[Seq[PO] => PO],
  private val iFn: Option[Seq[PI] => PI],
  private val numPO: Range.Inclusive,
  private val numPI: Range.Inclusive) extends RootNode
{
  // At least 0 ports must be supported
  require (!numPO.isEmpty)
  require (!numPI.isEmpty)
  require (numPO.start >= 0)
  require (numPI.start >= 0)

  val noOs = numPO.size == 1 && numPO.contains(0)
  val noIs = numPI.size == 1 && numPI.contains(0)

  require (noOs || oFn.isDefined)
  require (noIs || iFn.isDefined)

  private val accPO = ListBuffer[BaseNode[PO, PI, EO, EI, B]]()
  private val accPI = ListBuffer[BaseNode[PO, PI, EO, EI, B]]()
  private var oRealized  = false
  private var iRealized = false

  def name = lazyModule.name + "." + getClass.getName.split('.').last
  private def reqO() = require(numPO.contains(accPO.size), s"${name} has ${accPO.size} outputs, expected ${numPO}${lazyModule.line}")
  private def reqI() = require(numPI.contains(accPI.size), s"${name} has ${accPI.size} inputs, expected ${numPI}${lazyModule.line}")

  private lazy val oPorts = { oRealized = true; reqO(); accPO.result() }
  private lazy val iPorts = { iRealized = true; reqI(); accPI.result() }
  private lazy val oParams : Option[PO] = oFn.map(_(iPorts.map(_.oParams.get)))
  private lazy val iParams : Option[PI] = iFn.map(_(oPorts.map(_.iParams.get)))

  lazy val edgesOut = oPorts.map { n => imp.edgeO(oParams.get, n.iParams.get) }
  lazy val edgesIn  = iPorts.map { n => imp.edgeI(n.oParams.get, iParams.get) }

  lazy val bundleOut = imp.bundleO(edgesOut)
  lazy val bundleIn  = imp.bundleI(edgesIn)

  def connectOut = bundleOut
  def connectIn = bundleIn

  // source.edge(sink)
  protected[tilelink2] def edge(x: BaseNode[PO, PI, EO, EI, B])(implicit sourceInfo: SourceInfo) = {
    require (!noOs)
    require (!oRealized)
    require (!x.noIs)
    require (!x.iRealized)
    val i = x.accPI.size
    val o = accPO.size
    accPO += x
    x.accPI += this
    () => {
      imp.connect(connectOut(o), edgesOut(o), x.connectIn(i), x.edgesIn(i))
    }
  }
}

class IdentityNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])
  extends BaseNode(imp)(Some{case Seq(x) => x}, Some{case Seq(x) => x}, 1 to 1, 1 to 1)

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
  extends BaseNode(imp)(Some{case Seq() => po}, None, num, 0 to 0)
{
  require (num.end >= 1)
}

class SinkNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])(pi: PI, num: Range.Inclusive = 1 to 1)
  extends BaseNode(imp)(None, Some{case Seq() => pi}, 0 to 0, num)
{
  require (num.end >= 1)
}

class InteriorNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])
  (oFn: Seq[PO] => PO, iFn: Seq[PI] => PI, numPO: Range.Inclusive, numPI: Range.Inclusive)
  extends BaseNode(imp)(Some(oFn), Some(iFn), numPO, numPI)
{
  require (numPO.end >= 1)
  require (numPI.end >= 1)
}
